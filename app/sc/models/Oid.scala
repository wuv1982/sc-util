package sc.models

import play.api.libs.json.Json
import reactivemongo.bson.BSONObjectID
import reactivemongo.bson.BSONValue

case class Oid($oid: String) {
	def toBsonObj: BSONValue = BSONObjectID($oid)
}

object Oid {
	implicit val oidFmt = Json.format[Oid]

	def newOid: Oid = {
		Oid(reactivemongo.bson.BSONObjectID.generate.stringify)
	}
}
