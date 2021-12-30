package lila.ws

import akka.actor.typed.ActorRef
import strategygames.{ Player => SGPlayer, GameFamily }
import strategygames.format.{ FEN, Uci }
import java.util.concurrent.ConcurrentHashMap
import lila.ws.ipc._
import lila.ws.{ Clock, Position }

/* Manages subscriptions to FEN updates */
object Fens {

  case class Watched(position: Option[Position], clients: Set[ActorRef[ClientMsg]])

  private val games = new ConcurrentHashMap[Game.Id, Watched](1024)

  // client starts watching
  def watch(gameIds: Iterable[Game.Id], client: Client): Unit =
    gameIds foreach { gameId =>
      games
        .compute(
          gameId,
          {
            case (_, null)                  => Watched(None, Set(client))
            case (_, Watched(pos, clients)) => Watched(pos, clients + client)
          }
        )
        .position foreach { p =>
        client ! ClientIn.Fen(gameId, p)
      }
    }

  // when a client disconnects
  def unwatch(gameIds: Iterable[Game.Id], client: Client): Unit =
    gameIds foreach { gameId =>
      games.computeIfPresent(
        gameId,
        (_, watched) => {
          val newClients = watched.clients - client
          if (newClients.isEmpty) null
          else watched.copy(clients = newClients)
        }
      )
    }

  // a game finishes
  def finish(gameId: Game.Id, winner: Option[SGPlayer]) =
    games.computeIfPresent(
      gameId,
      (_, watched) => {
        watched.clients foreach { _ ! ClientIn.Finish(gameId, winner) }
        null
      }
    )

  // move coming from the server
  def move(gameId: Game.Id, json: JsonString, moveBy: Option[SGPlayer]): Unit =
    games.computeIfPresent(
      gameId,
      (_, watched) => {
        val turnSGPlayer = moveBy.fold(SGPlayer.p1)(c => !c)
        (json.value match {
          case MoveClockRegex(uciS, fenS, gf, wcS, bcS) => {
            val gfam = GameFamily(gf.toInt)
            for {
              uci <- Uci(gfam.gameLogic, gfam, uciS)
              wc  <- wcS.toIntOption
              bc  <- bcS.toIntOption
            } yield Position(uci, FEN(gfam.gameLogic, fenS), Some(Clock(wc, bc)), turnSGPlayer)
          }
          case MoveRegex(uciS, fenS, gf) => {
            val gfam = GameFamily(gf.toInt)
            Uci(gfam.gameLogic, gfam, uciS) map {
              Position(_, FEN(gfam.gameLogic, fenS), None, turnSGPlayer)
            }
          }
          case _                     => None
        }).fold(watched) { position =>
          val msg = ClientIn.Fen(gameId, position)
          watched.clients foreach { _ ! msg }
          watched.copy(position = Some(position))
        }
      }
    )

  // ...,"uci":"h2g2","san":"Rg2","fen":"r2qb1k1/p2nbrpn/6Np/3pPp1P/1ppP1P2/2P1B3/PP2B1R1/R2Q1NK1",...,"gf":0,...,"clock":{"p1":121.88,"p2":120.94}
  private val MoveRegex      = """uci":"([^"]+)".+fen":"([^"]+).+gf":(\d+)""".r.unanchored
  private val MoveClockRegex = """uci":"([^"]+)".+fen":"([^"]+).+gf":(\d+).+p1":(\d+).+p2":(\d+)""".r.unanchored

  def size = games.size
}
