package sc.models

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import play.api.libs.json.Writes
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.modules.reactivemongo.json.collection.JSONCollection
import sc.ma.Json.$
import sc.util.db.DBHelper
import play.api.libs.json.JsValue
import play.api.libs.json.Reads
import play.api.Logger
import play.api.libs.json.Json
import play.api.libs.json.JsError
import play.modules.reactivemongo.json.collection.JSONCollection
import reactivemongo.api.collections.GenericQueryBuilder
import play.api.libs.json.JsObject

trait MongoModel {
	def collection: JSONCollection
}

trait Model[T] extends MongoModel {
	def _id: Oid

	def save(implicit exec: ExecutionContext, writes: Writes[T]): Future[Boolean] = {
		DBHelper.update(collection)(_.save(
			writes.writes(this.asInstanceOf[T])))
	}

	def update(implicit exec: ExecutionContext, writes: Writes[T]): Future[Boolean] = {
		DBHelper.update(collection)(_.update(
			$("_id" -> _id),
			$("$set" -> writes.writes(this.asInstanceOf[T]))))
	}

	def remove(implicit exec: ExecutionContext): Future[Boolean] = {
		DBHelper.update(collection)(_.remove($("_id" -> _id)))
	}
}

trait ModelQuery[T] extends MongoModel {

	def find(selector: JsValue,
		filter: GenericQueryBuilder[JsObject, Reads, Writes] => GenericQueryBuilder[JsObject, Reads, Writes] = { identity _ },
		upTo: Int = Int.MaxValue)(implicit exec: ExecutionContext, reads: Reads[T]): Future[Seq[T]] = {
		DBHelper.query(collection)(selector, $(), filter, upTo).map { mayList =>
			mayList.map { list =>
				list.map { js =>
					reads.reads(js).fold(
						invalid => {
							Logger.warn(JsError.toFlatJson(invalid).toString())
							None
						},
						valid => Some(valid))
				}.collect {
					case Some(obj) => obj
				}
			} getOrElse (Seq[T]())
		}
	}

	def findOne(objectId: Oid)(implicit exec: ExecutionContext, reads: Reads[T]): Future[Option[T]] = {
		find($("_id" -> objectId)).map { _.headOption }
	}
}
