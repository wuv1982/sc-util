package sc.models

import play.api.libs.json.Format
import play.api.libs.json.Json
import sc.util.db.DBHelper
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection

case class UserActive(
	_id: Oid = Oid.newOid,
	authId: Oid,
	uid: String,
	name: String,
	action: Seq[UserAction]) extends Model[UserActive] {

	override def collection = UserActive.userActive

}

case class UserAction(
	div: String,
	timestamp: Long,
	duration: Long = 0)

object UserActive {
	implicit val userActionFmt: Format[UserAction] = Json.format[UserAction]
	implicit val activeFmt: Format[UserActive] = Json.format[UserActive]

	lazy val userActive = DBHelper.getCollection("userActive")
}

