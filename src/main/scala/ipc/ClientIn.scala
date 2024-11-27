package lila.ws
package ipc

import strategygames.{ Player => PlayerIndex, PocketData, Pos, Role }
import strategygames.format.{ FEN, Uci, UciCharPair }
import strategygames.opening.FullOpening
import lila.ws.Position
import lila.ws.util.LilaJsObject.augment
import play.api.libs.json._

sealed trait ClientIn extends ClientMsg {
  def write: String
}

private object DropsByRole {

  def json(drops: Map[Role, List[Pos]]) =
    if (drops.isEmpty) JsNull
    else {
      val sb    = new java.lang.StringBuilder(128)
      var first = true
      drops foreach { case (orig, dests) =>
        if (first) first = false
        else sb append " "
        sb append orig.forsyth
        dests foreach { sb append _.key }
      }
      JsString(sb.toString)
    }

}

object ClientIn {

  import Chess.json._

  case object Pong extends ClientIn {
    val write = "0"
  }

  case object Resync extends ClientIn {
    val write = cliMsg("resync")
  }

  // triggers actual disconnection
  case object Disconnect extends ClientIn {
    val write = cliMsg("bye") // not actually sent
  }

  case class LobbyPong(members: Int, rounds: Int) extends ClientIn {
    val write = Json stringify Json.obj(
      "t" -> "n",
      "d" -> members,
      "r" -> rounds
    )
  }

  case class Fen(gameId: Game.Id, position: Position) extends ClientIn {
    def write =
      cliMsg(
        "fen",
        Json
          .obj(
            "id"  -> gameId.value,
            "lm"  -> position.lastUci,
            "fen" -> position.fenWithPlayerIndex
          )
          .add("p1" -> position.clock.map(_.p1))
          .add("p2" -> position.clock.map(_.p2))
          .add("p1Pending" -> position.clock.map(_.p1Pending))
          .add("p2Pending" -> position.clock.map(_.p2Pending))
          .add("p1Delay" -> position.clock.map(_.p1Delay))
          .add("p2Delay" -> position.clock.map(_.p2Delay))
      )
  }

  case class Finish(gameId: Game.Id, winner: Option[PlayerIndex]) extends ClientIn {
    def write = cliMsg("finish", Json.obj("id" -> gameId.value, "win" -> winner.map(_.letter.toString)))
  }

  case class Mlat(millis: Double) extends ClientIn {
    lazy val write = cliMsg("mlat", millis)
  }

  sealed trait HasVersion extends ClientMsg {
    val version: SocketVersion
  }

  case class Versioned(json: JsonString, version: SocketVersion, troll: IsTroll) extends HasVersion {
    lazy val full = Payload(JsonString(s"""{"v":$version,${json.value drop 1}"""))
    lazy val skip = Payload(JsonString(s"""{"v":$version}"""))
  }

  case class Payload(json: JsonString) extends ClientIn {
    def write = json.value
  }
  def payload(js: JsValue)                 = Payload(JsonString(Json stringify js))
  def payload(tpe: String, js: JsonString) = Payload(JsonString(cliMsg(tpe, js)))

  case class Crowd(doc: JsObject) extends ClientIn {
    lazy val write = cliMsg("crowd", doc)
  }
  val emptyCrowd = Crowd(Json.obj())

  case class LobbyPairing(fullId: Game.FullId) extends ClientIn {
    def write =
      cliMsg(
        "redirect",
        Json.obj(
          "id"  -> fullId.value,
          "url" -> s"/$fullId"
        )
      )
  }

  case class LobbyNonIdle(payload: Payload) extends ClientIn {
    def write = payload.write
  }

  case class OnlyFor(endpoint: OnlyFor.Endpoint, payload: Payload) extends ClientMsg {
    def write = payload.write
  }
  object OnlyFor {
    sealed trait Endpoint
    case object Api             extends Endpoint
    case object Lobby           extends Endpoint
    case class Room(id: RoomId) extends Endpoint
  }
  def onlyFor(select: OnlyFor.type => OnlyFor.Endpoint, payload: Payload) =
    OnlyFor(select(OnlyFor), payload)

  case class TourReminder(tourId: Tour.ID, tourName: String) extends ClientIn {
    lazy val write = cliMsg(
      "tournamentReminder",
      Json.obj(
        "id"   -> tourId,
        "name" -> tourName
      )
    )
  }

  def tvSelect(data: JsonString) = payload("tvSelect", data)

  case class Opening(path: Path, opening: FullOpening) extends ClientIn {
    def write =
      cliMsg(
        "opening",
        Json.obj(
          "path"    -> path,
          "opening" -> opening
        )
      )
  }

  case object StepFailure extends ClientIn {
    def write = cliMsg("stepFailure")
  }

  case class Node(
      path: Path,
      id: UciCharPair,
      ply: Int,
      playerIndex: PlayerIndex,
      turnCount: Int,
      playedPlayerIndex: PlayerIndex,
      move: Uci.WithSan,
      fen: FEN,
      check: Boolean,
      dests: Map[Pos, List[Pos]],
      destsUci: Option[List[String]],
      captureLength: Option[Int],
      opening: Option[FullOpening],
      drops: Option[List[Pos]],
      dropsByRole: Option[Map[Role, List[Pos]]],
      pocketData: Option[PocketData],
      chapterId: Option[ChapterId]
  ) extends ClientIn {
    def write =
      cliMsg(
        "node",
        Json
          .obj(
            "path" -> path,
            "node" -> Json
              .obj(
                "ply"               -> ply,
                "playerIndex"       -> playerIndex.name,
                "turnCount"         -> turnCount,
                "playedPlayerIndex" -> playedPlayerIndex.name,
                "fen"               -> fen,
                "id"                -> id,
                "uci"               -> move.uci,
                "san"               -> move.san,
                "dests"             -> dests,
                "destsUci"          -> destsUci,
                "children"          -> JsArray(),
                "dropsByRole"       -> DropsByRole.json(dropsByRole.getOrElse(Map.empty))
              )
              .add("opening" -> opening)
              .add("captLen" -> captureLength)
              .add("check" -> check)
              .add("drops" -> drops.map { drops =>
                JsString(drops.map(_.key).mkString)
              })
              .add("crazy" -> pocketData)
          )
          .add("ch" -> chapterId)
      )
  }

  case class Dests(
      path: Path,
      dests: String,
      opening: Option[FullOpening],
      chapterId: Option[ChapterId],
      destsUci: Option[List[String]]
  ) extends ClientIn {
    def write =
      cliMsg(
        "dests",
        Json
          .obj(
            "dests" -> dests,
            "path"  -> path
          )
          .add("opening" -> opening)
          .add("ch" -> chapterId)
          .add("destsUci" -> destsUci)
      )
  }

  case class Ack(id: Option[Int]) extends ClientIn {
    def write = id.fold(cliMsg("ack")) { cliMsg("ack", _) }
  }

  case class RoundResyncPlayer(playerId: Game.PlayerId) extends ClientIn {
    def write = cliMsg("resync")
  }
  case class RoundGone(playerId: Game.PlayerId, v: Boolean) extends ClientIn {
    def write = cliMsg("gone", v)
  }
  case class RoundGoneIn(playerId: Game.PlayerId, seconds: Int) extends ClientIn {
    def write = cliMsg("goneIn", seconds)
  }
  case class RoundVersioned(
      version: SocketVersion,
      flags: RoundEventFlags,
      tpe: String,
      data: JsonString
  ) extends HasVersion {
    val full         = Payload(JsonString(cliMsg(tpe, data, version)))
    lazy val skip    = Payload(JsonString(s"""{"v":$version}"""))
    lazy val noDests = Payload(JsonString(destsRemover.replaceAllIn(full.write, "")))
  }
  def roundTourStanding(data: JsonString) = payload("tourStanding", data)

  case class Palantir(userIds: Iterable[User.ID]) extends ClientIn {
    def write = cliMsg("palantir", userIds)
  }

  case class MsgType(orig: User.ID) extends ClientIn {
    def write = cliMsg("msgType", orig)
  }

  object following {

    case class Onlines(users: List[FriendList.UserView]) extends ClientIn {
      def write =
        Json stringify Json.obj(
          "t"       -> "following_onlines",
          "d"       -> users.map(_.data.titleName),
          "playing" -> users.collect { case u if u.meta.playing => u.id },
          "patrons" -> users.collect { case u if u.data.patron => u.id }
        )
    }
    case class Enters(user: FriendList.UserView) extends ClientIn {
      // We use 'd' for backward compatibility with the mobile client
      def write =
        Json stringify Json.obj(
          "t" -> "following_enters",
          "d" -> user.data.titleName
        ) ++ {
          if (user.data.patron) Json.obj("patron" -> true)
          else Json.obj()
        }

    }
    abstract class Event(key: String) extends ClientIn {
      def user: User.ID
      def write = cliMsg(s"following_$key", user)
    }
    case class Leaves(user: User.ID)         extends Event("leaves")
    case class Playing(user: User.ID)        extends Event("playing")
    case class StoppedPlaying(user: User.ID) extends Event("stopped_playing")
  }

  case class StormKey(signed: String) extends ClientIn {
    def write = cliMsg("sk1", signed)
  }

  def racerState(data: JsonString) = payload("racerState", data)

  private val destsRemover = ""","dests":\{[^\}]+}""".r

  private def cliMsg[A: Writes](t: String, data: A): String =
    Json stringify Json.obj(
      "t" -> t,
      "d" -> data
    )
  private def cliMsg(t: String, data: JsonString): String = s"""{"t":"$t","d":${data.value}}"""
  private def cliMsg(t: String, data: JsonString, version: SocketVersion): String =
    s"""{"t":"$t","v":$version,"d":${data.value}}"""
  private def cliMsg(t: String, int: Int): String      = s"""{"t":"$t","d":$int}"""
  private def cliMsg(t: String, bool: Boolean): String = s"""{"t":"$t","d":$bool}"""
  private def cliMsg(t: String): String                = s"""{"t":"$t"}"""
}
