package sc.models

import scala.annotation.implicitNotFound
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import play.api.libs.json.Format
import play.modules.reactivemongo.json.collection.JSONCollection

trait Model[T] {

	def collection: JSONCollection

	def save(implicit exec: ExecutionContext, format: Format[T]): Future[Boolean] = {
		AuthDB.update(AuthDB.auth)(_.save(format.writes(this.asInstanceOf[T])))
	}
}
