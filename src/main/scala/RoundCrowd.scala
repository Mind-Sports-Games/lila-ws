package lila.ws

import strategygames.{ Player => SGPlayer }
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

import ipc._

final class RoundCrowd(
    lila: Lila,
    json: CrowdJson,
    groupedWithin: util.GroupedWithin
)(implicit ec: ExecutionContext) {

  import RoundCrowd._

  private val rounds = new ConcurrentHashMap[RoomId, RoundState](32768)

  def connect(roomId: RoomId, user: Option[User], player: Option[SGPlayer]): Unit =
    publish(
      roomId,
      rounds.compute(roomId, (_, cur) => Option(cur).getOrElse(RoundState()).connect(user, player))
    )

  def disconnect(roomId: RoomId, user: Option[User], player: Option[SGPlayer]): Unit = {
    rounds.computeIfPresent(
      roomId,
      (_, round) => {
        val newRound = round.disconnect(user, player)
        publish(roomId, newRound)
        if (newRound.isEmpty) null else newRound
      }
    )
  }

  def botOnline(roomId: RoomId, sgPlayer: SGPlayer, online: Boolean): Unit =
    rounds.compute(
      roomId,
      (_, cur) => {
        Option(cur).getOrElse(RoundState()).botOnline(sgPlayer, online) match {
          case None => cur
          case Some(round) =>
            publish(roomId, round)
            if (round.isEmpty) null else round
        }
      }
    )

  def getUsers(roomId: RoomId): Set[User.ID] =
    Option(rounds get roomId).fold(Set.empty[User.ID])(_.room.users.keySet)

  def isPresent(roomId: RoomId, userId: User.ID): Boolean =
    Option(rounds get roomId).exists(_.room.users contains userId)

  private def publish(roomId: RoomId, round: RoundState): Unit =
    outputBatch(outputOf(roomId, round))

  private val outputBatch = groupedWithin[Output](512, 700.millis) { outputs =>
    val aggregated = outputs
      .foldLeft(Map.empty[RoomId, Output]) { case (crowds, crowd) =>
        crowds.updated(crowd.room.roomId, crowd)
      }
      .values
    lila.emit.round(LilaIn.RoundOnlines(aggregated))
    aggregated foreach { output =>
      json round output foreach {
        Bus.publish(_ room output.room.roomId, _)
      }
    }
  }

  def size = rounds.size
}

object RoundCrowd {

  case class Output(room: RoomCrowd.Output, players: SGPlayer.Map[Int]) {
    def isEmpty = room.members == 0 && players.p1 == 0 && players.p2 == 0
  }

  def outputOf(roomId: RoomId, round: RoundState) =
    Output(
      room = RoomCrowd.outputOf(roomId, round.room),
      players = round.players
    )

  case class RoundState(
      room: RoomCrowd.RoomState = RoomCrowd.RoomState(),
      players: SGPlayer.Map[Int] = SGPlayer.Map(0, 0)
  ) {
    def connect(user: Option[User], player: Option[SGPlayer]) =
      copy(
        room = if (player.isDefined) room else room connect user,
        players = player.fold(players)(c => players.update(c, _ + 1))
      )
    def disconnect(user: Option[User], player: Option[SGPlayer]) =
      copy(
        room = if (player.isDefined) room else room disconnect user,
        players = player.fold(players)(c => players.update(c, nb => Math.max(0, nb - 1)))
      )
    def botOnline(sgPlayer: SGPlayer, online: Boolean): Option[RoundState] = Some {
      if (online) connect(None, Some(sgPlayer))
      else disconnect(None, Some(sgPlayer))
    }

    def isEmpty = room.isEmpty && players.forall(1 > _)
  }
}
