package sc.models

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import play.api.libs.json.Writes
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.modules.reactivemongo.json.collection.JSONCollection
import sc.ma.Json.$
import sc.util.db.DBHelper
import play.api.libs.json.JsValue

trait Model[T] {

	def _id: Oid
	def collection: JSONCollection

	def save(implicit exec: ExecutionContext, writes: Writes[T]): Future[Boolean] = {
		DBHelper.update(collection)(_.save(
			writes.writes(this.asInstanceOf[T])))
	}

	def update(implicit exec: ExecutionContext, writes: Writes[T]): Future[Boolean] = {
		DBHelper.update(collection)(_.update(
			$("_id" ->_id),
			$("$set" -> writes.writes(this.asInstanceOf[T]))))
	}
}
