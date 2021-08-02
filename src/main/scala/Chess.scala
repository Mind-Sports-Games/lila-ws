package lila.ws

import play.api.libs.json._
import strategygames.format.{ FEN, Forsyth, Uci, UciCharPair }
import strategygames.chess.opening.{ FullOpening, FullOpeningDB }
import strategygames.{ Game, GameLib, Pos, Role }
import strategygames.variant.Variant
import strategygames.chess.variant.Crazyhouse
import com.typesafe.scalalogging.Logger
import cats.syntax.option._

import ipc._

object Chess {

  private val logger = Logger(getClass)

  private val lib = GameLib.Chess()

  def apply(req: ClientOut.AnaMove): ClientIn =
    Monitor.time(_.chessMoveTime) {
      try {
        Game(lib, req.variant.some, Some(req.fen))(req.orig, req.dest, req.promotion)
          .toOption flatMap { case (game, move) =>
          game.pgnMoves.lastOption map { san =>
            makeNode(game, Uci.WithSan(lib, Uci(lib, move), san), req.path, req.chapterId)
          }
        } getOrElse ClientIn.StepFailure
      } catch {
        case e: java.lang.ArrayIndexOutOfBoundsException =>
          logger.warn(s"${req.fen} ${req.variant} ${req.orig}${req.dest}", e)
          ClientIn.StepFailure
      }
    }

  def apply(req: ClientOut.AnaDrop): ClientIn =
    Monitor.time(_.chessMoveTime) {
      try {
        (Game(lib, req.variant.some, Some(req.fen)), req.role, req.pos) match {
          case (Game.Chess(game), Role.ChessRole(role), Pos.Chess(pos))
            => game.drop(role, pos).toOption flatMap {
              case (game, drop) =>
                Game.Chess(game).pgnMoves.lastOption map { san =>
                  makeNode(
                    Game.Chess(game),
                    Uci.WithSan(lib, Uci(lib, drop), san),
                    req.path,
                    req.chapterId
                  )
                }
            } getOrElse ClientIn.StepFailure
          case _ => sys.error("Drop not implemented for libs other than chess")
        }
      } catch {
        case e: java.lang.ArrayIndexOutOfBoundsException =>
          logger.warn(s"${req.fen} ${req.variant} ${req.role}@${req.pos}", e)
          ClientIn.StepFailure
      }
    }

  def apply(req: ClientOut.AnaDests): ClientIn.Dests =
    Monitor.time(_.chessDestTime) {
      ClientIn.Dests(
        path = req.path,
        dests = {
          if (req.variant.standard && req.fen == Forsyth.initial(lib) && req.path.value.isEmpty)
            initialDests
          else {
            val sit = Game(lib, req.variant.some, Some(req.fen)).situation
            if (sit.playable(false)) json.destString(sit.destinations) else ""
          }
        },
        opening = {
          if (Variant.openingSensibleVariants(lib)(req.variant)) req.fen match {
            case FEN.Chess(fen) => FullOpeningDB findByFen fen
            case _ => sys.error("Invalid fen lib")
          }
          else None
        },
        chapterId = req.chapterId
      )
    }

  def apply(req: ClientOut.Opening): Option[ClientIn.Opening] =
    if (Variant.openingSensibleVariants(lib)(req.variant))
      req.fen match {
        case FEN.Chess(fen) => FullOpeningDB findByFen fen map {ClientIn.Opening(req.path, _)}
        case _ => sys.error("Invalid fen lib")
      }
    else None

  private def makeNode(
      game: Game,
      move: Uci.WithSan,
      path: Path,
      chapterId: Option[ChapterId]
  ): ClientIn.Node = {
    val movable = game.situation playable false
    val fen     = Forsyth.>>(lib, game)
    ClientIn.Node(
      path = path,
      id = UciCharPair(lib, move.uci),
      ply = game.turns,
      move = move,
      fen = fen,
      check = game.situation.check,
      dests = if (movable) game.situation.destinations else Map.empty,
      opening =
        if (game.turns <= 30 && Variant.openingSensibleVariants(lib)(game.board.variant))
          fen match {
            case FEN.Chess(fen) => FullOpeningDB findByFen fen
            case _ => sys.error("Invalid fen lib")
          }
        else None,
      drops = if (movable) game.situation.drops else Some(Nil),
      crazyData = game.situation.board.crazyData,
      chapterId = chapterId
    )
  }

  private val initialDests = "iqy muC gvx ltB bqs pxF jrz nvD ksA owE"

  object json {
    implicit val fenWrite         = Writes[FEN] { fen => JsString(fen.value) }
    implicit val pathWrite        = Writes[Path] { path => JsString(path.value) }
    implicit val uciWrite         = Writes[Uci] { uci => JsString(uci.uci) }
    implicit val uciCharPairWrite = Writes[UciCharPair] { ucp => JsString(ucp.toString) }
    implicit val posWrite         = Writes[Pos] { pos => JsString(pos.key) }
    implicit val chapterIdWrite   = Writes[ChapterId] { ch => JsString(ch.value) }
    implicit val openingWrite = Writes[FullOpening] { o =>
      Json.obj(
        "eco"  -> o.eco,
        "name" -> o.name
      )
    }
    implicit val destsJsonWriter: Writes[Map[Pos, List[Pos]]] = Writes { dests =>
      JsString(destString(dests))
    }
    def destString(dests: Map[Pos, List[Pos]]): String = {
      val sb    = new java.lang.StringBuilder(80)
      var first = true
      dests foreach { case (orig, dests) =>
        if (first) first = false
        else sb append " "
        sb append orig.piotr
        dests foreach { sb append _.piotr }
      }
      sb.toString
    }

    implicit val crazyhousePocketWriter: OWrites[Crazyhouse.Pocket] = OWrites { v =>
      JsObject(
        Crazyhouse.storableRoles.flatMap { role =>
          Some(v.roles.count(role == _)).filter(0 < _).map { count => role.name -> JsNumber(count) }
        }
      )
    }
    implicit val crazyhouseDataWriter: OWrites[Crazyhouse.Data] = OWrites { v =>
      Json.obj("pockets" -> List(v.pockets.white, v.pockets.black))
    }
  }
}
