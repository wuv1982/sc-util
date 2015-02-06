package sc.models

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import play.api.libs.json.Format
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import reactivemongo.api.gridfs.GridFS
import reactivemongo.api.gridfs.Implicits.DefaultReadFileReader
import reactivemongo.bson.BSONDocument
import reactivemongo.bson.BSONDocumentReader
import reactivemongo.bson.BSONDocumentWriter
import reactivemongo.bson.BSONObjectID
import sc.util.db.DBHelper
import play.api.Logger

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

	def remove(oid: String)(implicit exec: ExecutionContext): Future[Boolean] = {
		val gridFs = GridFS(DBHelper.db)
		gridFs.remove(BSONObjectID(oid)).map {
			_.err.map { e =>
				Logger.error(e)
				false
			} getOrElse {
				true
			}
		}
	}
}
