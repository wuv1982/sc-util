package sc.models

import play.api.libs.json.Format
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import sc.util.db.DBHelper

case class GridFsFile(
		_id: Oid, 
		contentType: Option[String], 
		filename: String, 
		uploadDate: Option[Long], 
		chunkSize: Int, 
		length: Int, 
		md5: Option[String], 
		metadata: JsValue) extends Model[GridFsFile] {

	override def collection = GridFsFile.files
}

object GridFsFile {
	val files = DBHelper.getCollection("fs.files")

	implicit val gridFsFmt: Format[GridFsFile] = Json.format[GridFsFile]
}