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
import reactivemongo.api.Cursor
import play.modules.reactivemongo.json.BSONFormats
import play.modules.reactivemongo.json.BSONFormats.BSONDocumentFormat
import reactivemongo.api.gridfs.ReadFile
import reactivemongo.bson.BSONValue

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

	override def remove(implicit exec: ExecutionContext): Future[Boolean] = {
		GridFsFile.gridFs.remove(_id.toBsonObj).map {
			_.err.map { e =>
				Logger.error(e)
				false
			} getOrElse {
				true
			}
		}
	}
}

object GridFsFile {
	val files = DBHelper.getCollection("fs.files")
	val gridFs = GridFS(DBHelper.db)

	implicit val gridFsFmt: Format[GridFsFile] = Json.format[GridFsFile]

	def findOne(oid: Oid)(implicit exec: ExecutionContext): Future[Option[(ReadFile[BSONValue], GridFsFile)]] = {
		gridFs.find(BSONDocument("_id" -> oid.toBsonObj)).headOption.map {
			_.map { file =>
				(file, GridFsFile(
					Oid(file.id.asInstanceOf[BSONObjectID].stringify),
					file.contentType,
					file.filename,
					file.uploadDate,
					file.chunkSize,
					file.length,
					file.md5,
					BSONDocumentFormat.writes(file.metadata)))
			}
		}
	}

}
