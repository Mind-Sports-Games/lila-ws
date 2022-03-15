package lila.ws

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ Behavior, PostStop }
import play.api.libs.json.JsValue

import ipc._

object LobbyClientActor {

  import ClientActor._

  case class State(
      room: RoomActor.State,
      idle: Boolean = false,
      site: ClientActor.State = ClientActor.State()
  )

  def start(roomState: RoomActor.State, fromVersion: Option[SocketVersion])(
    deps: Deps
  ): Behavior[ClientMsg] =
    Behaviors.setup { ctx =>
      import deps._
      onStart(deps, ctx)
      req.user foreach { users.connect(_, ctx.self, silently = true) }
      services.lobby.connect(req.sri -> req.user.map(_.id))
      Bus.subscribe(Bus.channel.lobby, ctx.self)
      RoomActor.onStart(roomState, fromVersion, deps, ctx)
      apply(State(roomState), deps)
    }

  private def apply(state: State, deps: Deps): Behavior[ClientMsg] =
    Behaviors
      .receive[ClientMsg] { (ctx, msg) => {
        import deps._  

        def forward(payload: JsValue): Unit =
          lilaIn.lobby(LilaIn.TellSri(req.sri, req.user.map(_.id), payload))

        def receive: PartialFunction[ClientMsg, Behavior[ClientMsg]] = {

          case ClientCtrl.Broom(oldSeconds) =>
            if (state.site.lastPing < oldSeconds) Behaviors.stopped
            else {
              keepAlive.tour(state.room.id)
              Behaviors.same
            }

          case ctrl: ClientCtrl => socketControl(state.site, deps, ctrl)

          case ClientIn.LobbyNonIdle(payload) =>
            if (!state.idle) clientIn(payload)
            Behaviors.same

          case in: ClientIn =>
            clientInReceive(state.site, deps, in) match {
              case None    => Behaviors.same
              case Some(s) => apply(state.copy(site = s), deps)
            }

          case msg: ClientOut.Ping =>
            clientIn(services.lobby.pong.get)
            apply(state.copy(site = sitePing(state.site, deps, msg)), deps)

          case ClientOut.LobbyForward(payload) =>
            forward(payload)
            Behaviors.same

          case ClientOut.Idle(value, payload) =>
            forward(payload)
            apply(state.copy(idle = value), deps)

          // default receive (site)
          case msg: ClientOutSite =>
            val siteState = globalReceive(state.site, deps, ctx, msg)
            if (siteState == state.site) Behaviors.same
            else apply(state.copy(site = siteState), deps)

          case _ =>
            Monitor.clientOutUnhandled("lobby").increment()
            Behaviors.same
        }

        RoomActor.receive(state.room, deps).lift(msg).fold(receive(msg)) { case (newState, emit) =>
          emit foreach lilaIn.lobby
          newState.fold(Behaviors.same[ClientMsg]) { roomState =>
            apply(state.copy(room = roomState), deps)
          }
        }

      }}
      .receiveSignal { case (ctx, PostStop) =>
        onStop(state.site, deps, ctx)
        RoomActor.onStop(state.room, deps, ctx)
        Bus.unsubscribe(Bus.channel.lobby, ctx.self)
        deps.services.lobby.disconnect(deps.req.sri)
        Behaviors.same
      }
    
}
