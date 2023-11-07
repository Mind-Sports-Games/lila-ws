package lila.ws
package ipc

import scala.util.chaining._
import strategygames.format.{ FEN, Uci }
import strategygames.variant.Variant
import strategygames.{
  Centis,
  Player => PlayerIndex,
  GameFamily,
  GameLogic,
  MoveMetrics,
  Pos,
  PromotableRole,
  Role
}
import lila.ws.util.LilaJsObject.augment
import play.api.libs.json._
import scala.util.{ Success, Try }

sealed trait ClientOut extends ClientMsg

sealed trait ClientOutSite  extends ClientOut
sealed trait ClientOutLobby extends ClientOut
sealed trait ClientOutStudy extends ClientOut
sealed trait ClientOutRound extends ClientOut
sealed trait ClientOutRacer extends ClientOut

object ClientOut {

  case class Ping(lag: Option[Int]) extends ClientOutSite

  case class Watch(ids: Set[Game.Id]) extends ClientOutSite

  case object MoveLat extends ClientOutSite

  case object Notified extends ClientOutSite

  case object FollowingOnline extends ClientOutSite

  case class Opening(variant: Variant, path: Path, fen: FEN) extends ClientOutSite

  case class Move(orig: Pos, dest: Pos)

  case class AnaMove(
      orig: Pos,
      dest: Pos,
      fen: FEN,
      path: Path,
      variant: Variant,
      chapterId: Option[ChapterId],
      promotion: Option[PromotableRole],
      uci: Option[String],
      fullCapture: Option[Boolean],
      payload: JsObject
  ) extends ClientOutSite

  case class AnaDrop(
      role: Role,
      pos: Pos,
      fen: FEN,
      path: Path,
      variant: Variant,
      chapterId: Option[ChapterId],
      payload: JsObject,
      halfMove: Option[Move] = None
  ) extends ClientOutSite

  case class AnaPass(
      fen: FEN,
      path: Path,
      variant: Variant,
      chapterId: Option[ChapterId],
      payload: JsObject
  ) extends ClientOutSite

  case class AnaDests(
      fen: FEN,
      path: Path,
      variant: Variant,
      chapterId: Option[ChapterId],
      uci: Option[String],
      lastUci: Option[String],
      fullCapture: Option[Boolean]
  ) extends ClientOutSite

  case class MsgType(dest: User.ID) extends ClientOutSite

  case class SiteForward(payload: JsObject) extends ClientOutSite

  case class UserForward(payload: JsObject) extends ClientOutSite

  case class Unexpected(msg: JsValue) extends ClientOutSite

  case object WrongHole extends ClientOutSite

  case object Ignore extends ClientOutSite

  // lobby

  case class Idle(value: Boolean, payload: JsValue) extends ClientOutLobby
  case class LobbyForward(payload: JsValue)         extends ClientOutLobby

  // study

  case class StudyForward(payload: JsValue) extends ClientOutStudy

  // round

  case class RoundPlayerForward(payload: JsValue) extends ClientOutRound
  case class RoundMove(
      gf: GameFamily,
      uci: Uci,
      blur: Boolean,
      lag: MoveMetrics,
      ackId: Option[Int]
  ) extends ClientOutRound
  case class RoundSelectSquaresOffer(
      gf: GameFamily,
      squares: String
  )                                              extends ClientOutRound
  case class RoundHold(mean: Int, sd: Int)       extends ClientOutRound
  case class RoundBerserk(ackId: Option[Int])    extends ClientOutRound
  case class RoundSelfReport(name: String)       extends ClientOutRound
  case class RoundFlag(playerIndex: PlayerIndex) extends ClientOutRound
  case object RoundBye                           extends ClientOutRound

  // chat

  case class ChatSay(msg: String)                                       extends ClientOut
  case class ChatTimeout(suspect: String, reason: String, text: String) extends ClientOut

  // challenge

  case object ChallengePing extends ClientOut

  // palantir

  case object PalantirPing extends ClientOut

  // storm

  case class StormKey(key: String, pad: String) extends ClientOutSite

  // racer

  case class RacerScore(score: Int) extends ClientOutRacer
  case object RacerJoin             extends ClientOutRacer

  // Strategy games has few issues right now, so we have to implement our own
  // lookup that turns some role into a promotable role.
  // Step 1, turn a ground name into a "Role". Note that we can't currently match
  //         between Role and Promotable role, because the wrapper layer
  //         currently wraps them all as a role
  def groundToRole(lib: GameLogic, variant: Variant)(groundName: Option[String]): Option[Role] =
    groundName.flatMap(
      Role
        .allByGroundName(lib, variant.gameFamily)
        .get(_)
    )

  // Step 2, Turn a role into a Promotable role by looping through all of the promotable
  //         roles of a given given gamelogic and seeing if one exists by that name.
  // TODO: I would try and make this more efficient, but it would result in
  //       some leaking of abstractions here, which I don't want.
  def roleToPromotable(lib: GameLogic)(role: Option[Role]): Option[PromotableRole] =
    role.flatMap(r => Role.allPromotable(lib).filter(_.name == r.name).headOption)

  // impl
  def parse(str: String): Try[ClientOut] =
    if (str == "null" || str == """{"t":"p"}""") emptyPing
    else
      Try(Json parse str) map {
        case o: JsObject =>
          o str "t" flatMap {
            case "p" => Some(Ping(o int "l"))
            case "startWatching" =>
              o str "d" map { d =>
                Watch(d.split(" ").take(16).map(Game.Id.apply).toSet)
              } orElse Some(Ignore) // old apps send empty watch lists
            case "moveLat"           => Some(MoveLat)
            case "notified"          => Some(Notified)
            case "following_onlines" => Some(FollowingOnline)
            case "opening" =>
              for {
                d    <- o obj "d"
                path <- d str "path"
                fen  <- d str "fen"
                lib     = dataGameLogic(d)
                variant = dataVariant(d, lib)
              } yield Opening(variant, Path(path), FEN(lib, fen))
            case "anaMove" =>
              for {
                d <- o obj "d"
                lib = dataGameLogic(d)
                orig <- d str "orig" flatMap (p => Pos.fromKey(lib, p))
                dest <- d str "dest" flatMap (p => Pos.fromKey(lib, p))
                path <- d str "path"
                fen  <- d str "fen"
                variant   = dataVariant(d, lib)
                chapterId = d.str("ch").map(ChapterId.apply)
                promotion = d
                  .str("promotion")
                  .pipe(groundToRole(lib, variant))
                  .pipe(roleToPromotable(lib))
                uci         = d str "uci"
                fullCapture = d boolean "fullCapture"
              } yield AnaMove(
                orig,
                dest,
                FEN(lib, fen),
                Path(path),
                variant,
                chapterId,
                promotion,
                uci,
                fullCapture,
                o
              )
            case "anaDrop" =>
              for {
                d <- o obj "d"
                lib     = dataGameLogic(d)
                variant = dataVariant(d, lib)
                role <- d str "role" flatMap Role.allByGroundName(lib, variant.gameFamily).get
                pos  <- d str "pos" flatMap (p => Pos.fromKey(lib, p))
                path <- d str "path"
                fen  <- d str "fen"
                chapterId = d str "ch" map ChapterId.apply
              } yield AnaDrop(
                role,
                pos,
                FEN(lib, fen),
                Path(path),
                variant,
                chapterId,
                o,
                (d obj "halfMove").flatMap(halfMove => {
                  for {
                    orig <- halfMove str "orig" flatMap (p => Pos.fromKey(lib, p))
                    dest <- halfMove str "dest" flatMap (p => Pos.fromKey(lib, p))
                  } yield Move(orig, dest)
                })
              )
            case "anaPass" =>
              for {
                d <- o obj "d"
                lib     = dataGameLogic(d)
                variant = dataVariant(d, lib)
                path <- d str "path"
                fen  <- d str "fen"
                chapterId = d str "ch" map ChapterId.apply
              } yield AnaPass(
                FEN(lib, fen),
                Path(path),
                variant,
                chapterId,
                o
              )
            case "anaDests" =>
              for {
                d    <- o obj "d"
                path <- d str "path"
                fen  <- d str "fen"
                lib         = dataGameLogic(d)
                variant     = dataVariant(d, lib)
                chapterId   = d str "ch" map ChapterId.apply
                uci         = d str "uci"
                lastUci     = d str "lastUci"
                fullCapture = d boolean "fullCapture"
              } yield AnaDests(
                FEN(lib, fen),
                Path(path),
                variant,
                chapterId,
                uci,
                lastUci,
                fullCapture
              )
            case "evalGet" | "evalPut" => Some(SiteForward(o))
            case "msgType"             => o str "d" map MsgType.apply
            case "msgSend" | "msgRead" => Some(UserForward(o))
            // lobby
            case "idle" => o boolean "d" map { Idle(_, o) }
            case "join" | "cancel" | "joinSeek" | "cancelSeek" | "poolIn" | "poolOut" | "hookIn" |
                "hookOut" =>
              Some(LobbyForward(o))
            // study
            case "like" | "setPath" | "deleteNode" | "promote" | "forceVariation" | "setRole" | "kick" |
                "leave" | "shapes" | "addChapter" | "setChapter" | "editChapter" | "descStudy" |
                "descChapter" | "deleteChapter" | "clearAnnotations" | "sortChapters" | "editStudy" |
                "setTag" | "setComment" | "deleteComment" | "setGamebook" | "toggleGlyph" | "explorerGame" |
                "requestAnalysis" | "invite" | "relaySync" | "setTopics" =>
              Some(StudyForward(o))
            // round
            case "move" =>
              for {
                d <- o obj "d"
                lib     = dataGameLogic(d)
                variant = dataVariant(d, lib)
                move <- d str "u" flatMap (m =>
                  Uci.Move.apply(lib, variant.gameFamily, m)
                ) orElse parseOldMove(d, lib, variant)
                blur  = d int "b" contains 1
                ackId = d int "a"
              } yield RoundMove(variant.gameFamily, move, blur, parseMetrics(d), ackId)
            case "drop" =>
              for {
                d <- o obj "d"
                lib     = dataGameLogic(d)
                variant = dataVariant(d, lib)
                role <- d str "role" flatMap { g =>
                  Role.allByGroundName(lib, variant.gameFamily).get(g)
                }
                pos  <- d str "pos"
                drop <- Uci.Drop.fromStrings(lib, variant.gameFamily, role.name, pos)
                blur  = d int "b" contains 1
                ackId = d int "a"
              } yield RoundMove(variant.gameFamily, drop, blur, parseMetrics(d), ackId)
            case "pass" =>
              for {
                d <- o obj "d"
                lib     = dataGameLogic(d)
                variant = dataVariant(d, lib)
                pass <- Uci.Pass.apply(lib, variant.gameFamily)
                blur  = d int "b" contains 1
                ackId = d int "a"
              } yield RoundMove(variant.gameFamily, pass, blur, parseMetrics(d), ackId)
            case "selectSquares" =>
              for {
                d <- o obj "d"
                lib     = dataGameLogic(d)
                variant = dataVariant(d, lib)
                squares <- (d str "s").map(_.split(",").flatMap(p => Pos.fromKey(lib, p)))
                ss      <- Uci.SelectSquares.fromSquares(lib, variant.gameFamily, squares.toList)
                blur  = d int "b" contains 1
                ackId = d int "a"
              } yield RoundMove(variant.gameFamily, ss, blur, parseMetrics(d), ackId)
            case "hold" =>
              for {
                d    <- o obj "d"
                mean <- d int "mean"
                sd   <- d int "sd"
              } yield RoundHold(mean, sd)
            case "berserk"      => Some(RoundBerserk(o obj "d" flatMap (_ int "a")))
            case "rep"          => o obj "d" flatMap (_ str "n") map RoundSelfReport.apply
            case "flag"         => o str "d" flatMap PlayerIndex.fromName map RoundFlag.apply
            case "bye2"         => Some(RoundBye)
            case "palantirPing" => Some(PalantirPing)
            case "moretime" | "rematch-yes" | "rematch-no" | "takeback-yes" | "takeback-no" | "draw-yes" |
                "draw-no" | "draw-claim" | "resign" | "resign-force" | "draw-force" | "abort" | "outoftime" |
                "select-squares-accept" | "select-squares-decline" =>
              Some(RoundPlayerForward(o))
            case "select-squares-offer" =>
              for {
                d <- o obj "d"
                lib     = dataGameLogic(d)
                variant = dataVariant(d, lib)
                squares <- d str "s"
              } yield RoundSelectSquaresOffer(variant.gameFamily, squares)
            // chat
            case "talk" => o str "d" map { ChatSay.apply }
            case "timeout" =>
              for {
                data   <- o obj "d"
                userId <- data str "userId"
                reason <- data str "reason"
                text   <- data str "text"
              } yield ChatTimeout(userId, reason, text)
            case "ping" => Some(ChallengePing)
            // storm
            case "sk1" =>
              o str "d" flatMap { s =>
                s split '!' match {
                  case Array(key, pad) => Some(StormKey(key, pad))
                  case _               => None
                }
              }
            // racer
            case "racerScore" => o int "d" map RacerScore.apply
            case "racerJoin"  => Some(RacerJoin)

            case "wrongHole" => Some(WrongHole)
            case _           => None
          } getOrElse Unexpected(o)
        case js => Unexpected(js)
      }

  private val emptyPing: Try[ClientOut] = Success(Ping(None))

  private def dataGameLogic(d: JsObject): GameLogic =
    GameLogic(d int "lib" getOrElse 0)

  private def dataVariant(d: JsObject, lib: GameLogic): Variant =
    Variant.orDefault(lib, d str "variant" getOrElse "")

  private def parseOldMove(d: JsObject, lib: GameLogic, variant: Variant) =
    for {
      orig <- d str "from"
      dest <- d str "to"
      prom = d str "promotion"
      move <- Uci.Move.fromStrings(lib, variant.gameFamily, orig, dest, prom)
    } yield move

  private def parseMetrics(d: JsObject) =
    MoveMetrics(
      d.int("l") map Centis.ofMillis,
      d.str("s") flatMap { v =>
        Try(Centis(Integer.parseInt(v, 36))).toOption
      }
    )
}
