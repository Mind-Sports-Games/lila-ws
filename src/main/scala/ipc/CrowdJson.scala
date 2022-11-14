package lila.ws
package ipc

import com.github.blemale.scaffeine.{ AsyncLoadingCache, Scaffeine }
import play.api.libs.json._
import scala.concurrent.duration._
import scala.concurrent.{ ExecutionContext, Future }

final class CrowdJson(
    mongo: Mongo,
    lightUserApi: LightUserApi
)(implicit ec: ExecutionContext) {

  def room(crowd: RoomCrowd.Output): Future[ClientIn.Crowd] = {
    if (crowd.users.sizeIs > 20) keepOnlyStudyMembers(crowd) map { users =>
      crowd.copy(users = users, anons = 0)
    }
    else Future successful crowd
  } flatMap spectatorsOf map ClientIn.Crowd.apply

  def round(crowd: RoundCrowd.Output): Future[ClientIn.Crowd] =
    inquirersCache.get {} flatMap { inquirers =>
      spectatorsOf(
        crowd.room.copy(
          users = if (crowd.room.users.sizeIs > 20) Nil else crowd.room.users.filterNot(inquirers.contains)
        )
      )
    } map { spectators =>
      ClientIn.Crowd(
        Json
          .obj(
            "p1"    -> (crowd.players.p1 > 0),
            "p2"    -> (crowd.players.p2 > 0),
            "watchers" -> spectators
          )
      )
    }

  private def spectatorsOf(crowd: RoomCrowd.Output): Future[JsObject] =
    if (crowd.users.isEmpty) Future successful Json.obj("nb" -> crowd.members)
    else
      Future.traverse(crowd.users)(lightUserApi.get) map { names =>
        Json.obj(
          "nb"    -> crowd.members,
          "users" -> names.filterNot(isBotName),
          "anons" -> crowd.anons
        )
      }

  private def isBotName(str: String) = str startsWith "BOT "

  private val inquirersCache: AsyncLoadingCache[Unit, Set[User.ID]] =
    Scaffeine()
      .expireAfterWrite(1.second)
      .buildAsyncFuture(_ => mongo.inquirers)

  private val isStudyCache: AsyncLoadingCache[String, Boolean] =
    Scaffeine()
      .expireAfterWrite(20.minutes)
      .buildAsyncFuture(mongo.studyExists)

  private def keepOnlyStudyMembers(crowd: RoomCrowd.Output): Future[Iterable[User.ID]] =
    isStudyCache.get(crowd.roomId.value) flatMap {
      case false => Future successful Nil
      case true  => mongo.studyMembers(crowd.roomId.value) map crowd.users.toSet.intersect
    }
}
