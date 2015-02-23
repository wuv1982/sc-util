package sc.util.db

import reactivemongo.api._
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import reactivemongo.core.commands.LastError
import play.api.Logger
import reactivemongo.api.collections.GenericQueryBuilder
import play.api.libs.json.Reads
import play.api.libs.json.Writes
import sc.ma.Json._
import play.api.mvc.Controller

trait DBHelper {

	def update(collection: JSONCollection)(f: JSONCollection => Future[LastError]): Future[Boolean] = {
		f(collection)
			.map {
				_.err.map { e =>
					Logger.error(e)
					false
				} getOrElse {
					true
				}
			}
	}

	def query(collection: JSONCollection)(
		selector: JsValue,
		projection: JsValue = $(),
		filter: GenericQueryBuilder[JsObject, Reads, Writes] => GenericQueryBuilder[JsObject, Reads, Writes] = { identity _ },
		upTo: Int = Int.MaxValue): Future[Option[Seq[JsObject]]] = {

		Logger.debug(s"[${collection.name}] query: $selector")
		filter(collection.find(selector, projection))
			.cursor[JsObject]
			.collect[Seq]()
			.map { list =>
				if (list.isEmpty) {
					Logger.debug(s"[${collection.name}] query empty")
					None
				} else {
					Logger.debug(s"[${collection.name}] query result length: ${list.length}")
					Some(list)
				}
			}
			.recover {
				case ex =>
					Logger.error(s"[${collection.name}] query error", ex)
					None
			}
	}
}

object DBHelper extends DBHelper with MongoController with Controller {
	def getCollection(name: String) = db.collection[JSONCollection](name)
}
