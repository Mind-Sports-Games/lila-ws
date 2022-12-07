package lila.ws

import scala.collection.MapView

import play.api.libs.json._
import strategygames.format.{ FEN, Forsyth, Uci, UciCharPair }
import strategygames.opening.{ FullOpening, FullOpeningDB }
import strategygames.{ Game, GameLogic, Pocket, PocketData, Pos, Role, Situation }
import strategygames.variant.Variant
import strategygames.draughts
import com.typesafe.scalalogging.Logger
import cats.syntax.option._

import ipc._

object Chess {

  private val logger = Logger(getClass)

  def apply(req: ClientOut.AnaMove): ClientIn =
    Monitor.time(_.chessMoveTime) {
      try {
        lazy val fullCaptureFields =
          req.uci
            .flatMap(m =>
              Uci.Move.apply(
                req.variant.gameLogic,
                req.variant.gameFamily,
                m
              )
            )
            .flatMap(_.capture)
        Game(req.variant.gameLogic, req.variant.some, Some(req.fen))(
          orig = req.orig,
          dest = req.dest,
          promotion = req.promotion,
          finalSquare = fullCaptureFields.isDefined,
          captures = fullCaptureFields,
          partialCaptures = req.fullCapture.getOrElse(false)
        ).toOption flatMap { case (game, move) =>
          game.pgnMoves.lastOption map { san =>
            {
              val movable = game.situation playable false
              val captLen = (game.situation, req.dest) match {
                case (Situation.Draughts(sit), Pos.Draughts(dest)) =>
                  if (sit.ghosts > 0) sit.captureLengthFrom(dest)
                  else sit.allMovesCaptureLength.some
                case _ => None
              }
              val validMoves: Map[strategygames.draughts.Pos, List[strategygames.draughts.Move]] =
                (game.situation, req.dest) match {
                  case (Situation.Draughts(sit), Pos.Draughts(dest)) =>
                    getValidMoves(
                      sit,
                      if (sit.ghosts > 0) dest.some else None,
                      req.fullCapture.getOrElse(false)
                    )
                  case _ => Map.empty
                }
              val truncatedMoves =
                if (req.fullCapture.getOrElse(false) && captLen.getOrElse(0) > 1)
                  truncateMoves(validMoves).some
                else None
              makeNode(
                game,
                Uci.WithSan(
                  req.variant.gameLogic,
                  Uci(
                    req.variant.gameLogic,
                    move,
                    req.variant.gameLogic match {
                      case GameLogic.Draughts() => fullCaptureFields.isDefined
                      case _                    => false
                    }
                  ),
                  san
                ),
                req.path,
                req.chapterId,
                dests = req.variant match {
                  case Variant.Draughts(variant) => {
                    val truncatedDests = truncatedMoves.map {
                      _ mapValues { _ flatMap (uci => variant.boardSize.pos.posAt(uci.takeRight(2))) }
                    }
                    val draughtsDests: Map[strategygames.Pos, List[strategygames.Pos]] =
                      truncatedDests
                        .getOrElse(validMoves.view.mapValues { _ map (_.dest) })
                        .to(Map)
                        .map { case (p, m) => (Pos.Draughts(p), m.map(Pos.Draughts)) }
                    if (movable) draughtsDests else Map.empty
                  }
                  case _ => if (movable) game.situation.destinations else Map.empty
                },
                destsUci = req.variant.gameLogic match {
                  case GameLogic.Draughts() =>
                    if (movable) truncatedMoves.map(_.values.toList.flatten) else None
                  case _ => None
                },
                captureLength = if (movable) captLen else None
              )
            }
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
        Game(
          req.variant.gameLogic,
          req.variant.some,
          Some(req.fen)
        ).drop(req.role, req.pos).toOption flatMap { case (game, drop) =>
          game.pgnMoves.lastOption map { san =>
            makeNode(
              game,
              Uci.WithSan(req.variant.gameLogic, Uci(req.variant.gameLogic, drop), san),
              req.path,
              req.chapterId,
              if (game.situation playable false) game.situation.destinations else Map.empty
            )
          }
        } getOrElse ClientIn.StepFailure
      } catch {
        case e: java.lang.ArrayIndexOutOfBoundsException =>
          logger.warn(s"${req.fen} ${req.variant} ${req.role}@${req.pos}", e)
          ClientIn.StepFailure
      }
    }

  def apply(req: ClientOut.AnaDests): ClientIn.Dests =
    Monitor.time(_.chessDestTime) {
      val sit = Game(req.variant.gameLogic, req.variant.some, Some(req.fen)).situation
      val isInitial = (req.variant.standardVariant) &&
        req.fen == Forsyth.initial(req.variant.gameLogic) &&
        req.path.value.isEmpty
      ClientIn.Dests(
        path = req.path,
        dests = {
          (sit, req.variant) match {
            case (Situation.Draughts(sit), Variant.Draughts(variant)) =>
              if (isInitial) initialDraughtsDests
              else if (sit.playable(false)) {
                val orig: Option[strategygames.draughts.Pos] =
                  if (req.lastUci.exists(_.length >= 4) && sit.ghosts > 0)
                    req.lastUci.flatMap { uci =>
                      variant.boardSize.pos.posAt(uci.substring(uci.length - 2))
                    }
                  else None
                val validMoves: Map[strategygames.draughts.Pos, List[strategygames.draughts.Move]] =
                  getValidMoves(sit, orig, req.fullCapture.getOrElse(false))
                val captureLength: Int =
                  orig.fold(sit.allMovesCaptureLength)(sit.captureLengthFrom(_).getOrElse(0))
                val truncatedMoves: Option[MapView[strategygames.draughts.Pos, List[String]]] =
                  if (!isInitial && req.fullCapture.getOrElse(false) && captureLength > 1)
                    Some(truncateMoves(validMoves))
                  else None
                val truncatedDests = truncatedMoves.map {
                  _ mapValues { _ flatMap (uci => variant.boardSize.pos.posAt(uci.takeRight(2))) }
                }
                val destsToConvert: Map[Pos, List[Pos]] =
                  truncatedDests
                    .getOrElse(validMoves.view.mapValues { _ map (_.dest) })
                    .to(Map)
                    .map { case (p, lp) => (Pos.Draughts(p), lp.map(Pos.Draughts)) }
                val destStr = json.destString(destsToConvert)
                if (captureLength > 0) s"#$captureLength $destStr"
                else destStr
              } else ""
            case (Situation.Chess(_), Variant.Chess(_)) =>
              if (isInitial) initialChessDests
              else if (sit.playable(false)) json.destString(sit.destinations)
              else ""
            case _ =>
              if (sit.playable(false)) json.destString(sit.destinations)
              else ""
          }
        },
        opening = {
          if (Variant.openingSensibleVariants(req.variant.gameLogic)(req.variant))
            FullOpeningDB.findByFen(req.variant.gameLogic, req.fen)
          else None
        },
        chapterId = req.chapterId,
        destsUci = {
          (sit, req.variant) match {
            case (Situation.Draughts(sit), Variant.Draughts(variant)) =>
              //heavily duplicated from above
              val orig: Option[strategygames.draughts.Pos] =
                if (req.lastUci.exists(_.length >= 4) && sit.ghosts > 0)
                  req.lastUci.flatMap { uci =>
                    variant.boardSize.pos.posAt(uci.substring(uci.length - 2))
                  }
                else None
              val validMoves: Map[strategygames.draughts.Pos, List[strategygames.draughts.Move]] =
                getValidMoves(sit, orig, req.fullCapture.getOrElse(false))
              val captureLength: Int =
                orig.fold(sit.allMovesCaptureLength)(sit.captureLengthFrom(_).getOrElse(0))
              val truncatedMoves: Option[MapView[strategygames.draughts.Pos, List[String]]] =
                if (!isInitial && req.fullCapture.getOrElse(false) && captureLength > 1)
                  Some(truncateMoves(validMoves))
                else None
              truncatedMoves.map(_.values.toList.flatten)
            case _ => None
          }
        }
      )
    }

  def apply(req: ClientOut.Opening): Option[ClientIn.Opening] =
    if (Variant.openingSensibleVariants(req.variant.gameLogic)(req.variant))
      FullOpeningDB.findByFen(req.variant.gameLogic, req.fen) map { ClientIn.Opening(req.path, _) }
    else None

  private def makeNode(
      game: Game,
      move: Uci.WithSan,
      path: Path,
      chapterId: Option[ChapterId],
      dests: Map[Pos, List[Pos]],
      destsUci: Option[List[String]] = None,
      captureLength: Option[Int] = None
  ): ClientIn.Node = {
    val movable = game.situation playable false
    val fen     = Forsyth.>>(game.board.variant.gameLogic, game)
    ClientIn.Node(
      path = path,
      id = UciCharPair(game.board.variant.gameLogic, move.uci),
      ply = game.turns,
      move = move,
      fen = fen,
      check = game.situation.check,
      dests = dests,
      destsUci = destsUci,
      captureLength = captureLength,
      opening =
        if (
          game.turns <= 30 && Variant
            .openingSensibleVariants(game.board.variant.gameLogic)(game.board.variant)
        )
          FullOpeningDB.findByFen(game.board.variant.gameLogic, fen)
        else None,
      drops = if (movable) game.situation.drops else Some(Nil),
      dropsByRole = game.situation match {
        case (Situation.FairySF(_)) =>
          game.situation.dropsByRole
        case _ => None
      },
      pocketData = game.situation.board.pocketData,
      chapterId = chapterId
    )
  }

  //TODO: push this into strategygames. So much copy/paste from lila.socket.AnaDests
  //this continues until `object json`
  private val initialChessDests    = "iqy muC gvx ltB bqs pxF jrz nvD ksA owE"
  private val initialDraughtsDests = "HCD GBC ID FAB EzA"

  //draughts
  private type BoardWithUci = (Option[draughts.Board], String)

  //draughts
  private def uniqueUci(otherUcis: List[BoardWithUci], uci: BoardWithUci) = {
    var i      = 2
    var unique = uci._2.slice(0, i)
    while (i + 2 <= uci._2.length && otherUcis.exists(_._2.startsWith(unique))) {
      i += 2
      unique = uci._2.slice(0, i)
    }
    if (i == uci._2.length) uci
    else if (i == 2) (none, uci._2.slice(0, 4))
    else (none, unique)
  }

  //draughts
  private def getValidMoves(
      sit: draughts.Situation,
      from: Option[draughts.Pos],
      fullCapture: Boolean
  ): Map[strategygames.draughts.Pos, List[strategygames.draughts.Move]] =
    from.fold(if (fullCapture) sit.validMovesFinal else sit.validMoves) { pos =>
      Map(pos -> sit.movesFrom(pos, fullCapture))
    }

  //draughts
  private def truncateMoves(validMoves: Map[draughts.Pos, List[draughts.Move]]) = {
    var truncated = false
    val truncatedMoves = validMoves map { case (pos, moves) =>
      if (moves.size <= 1) pos -> moves.map(m => (m.after.some, m.toUci.uci))
      else
        pos -> moves.foldLeft(List[BoardWithUci]()) { (acc, move) =>
          val sameDestUcis = moves
            .filter(m => m != move && m.dest == move.dest && (m.orig == m.dest || m.after != move.after))
            .map(m => (m.after.some, m.toUci.uci))
          val uci = (move.after.some, move.toUci.uci)
          val newUci =
            if (sameDestUcis.isEmpty && move.orig != move.dest) uci else uniqueUci(sameDestUcis, uci)
          if (!acc.contains(newUci)) {
            if (newUci._2.length != uci._2.length) truncated = true
            newUci :: acc
          } else {
            truncated = true
            acc
          }
        }
    }
    (if (truncated) truncateUcis(truncatedMoves) else truncatedMoves) mapValues { _ map { _._2 } }
  }

  //draughts
  @scala.annotation.tailrec
  private def truncateUcis(
      validUcis: Map[draughts.Pos, List[BoardWithUci]]
  ): Map[draughts.Pos, List[BoardWithUci]] = {
    var truncated = false
    val truncatedUcis = validUcis map { case (pos, uciList) =>
      if (uciList.size <= 1) pos -> uciList
      else
        pos -> uciList.foldLeft(List[BoardWithUci]()) { (acc, uci) =>
          val dest = uci._2.takeRight(2)
          val sameDestUcis = uciList.filter(u =>
            u != uci && u._2.takeRight(2) == dest && (u._2.startsWith(
              dest
            ) || (u._1.isEmpty && uci._1.isEmpty) || u._1 != uci._1)
          )
          val newUci = if (sameDestUcis.isEmpty) uci else uniqueUci(sameDestUcis, uci)
          if (!acc.contains(newUci)) {
            if (newUci._2.length != uci._2.length) truncated = true
            newUci :: acc
          } else {
            truncated = true
            acc
          }
        }
    }
    if (truncated) truncateUcis(truncatedUcis)
    else truncatedUcis
  }

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

    implicit val pocketWriter: OWrites[Pocket] = OWrites { v =>
      JsObject(
        Role
          .storable(v.roles.headOption match {
            case Some(r) =>
              r match {
                case Role.ChessRole(_)   => GameLogic.Chess()
                case Role.FairySFRole(_) => GameLogic.FairySF()
                case _                   => sys.error("Pocket not implemented for GameLogic")
              }
            case None => GameLogic.Chess()
          })
          .flatMap { role =>
            Some(v.roles.count(role == _)).filter(0 < _).map { count => role.groundName -> JsNumber(count) }
          }
      )
    }
    implicit val pocketDataWriter: OWrites[PocketData] = OWrites { v =>
      Json.obj("pockets" -> List(v.pockets.p1, v.pockets.p2))
    }
  }
}
