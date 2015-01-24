package sc.models

import play.api.libs.json.Json

case class Oid($oid: String)

object Oid {
	implicit val oidFmt = Json.format[Oid]

	def newOid: Oid = {
		Oid(reactivemongo.bson.BSONObjectID.generate.stringify)
	}
}
