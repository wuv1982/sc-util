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
import java.io.OutputStream
import play.api.libs.Files.TemporaryFile
import java.util.zip.ZipOutputStream
import java.io.FileOutputStream
import java.util.zip.ZipEntry
import play.api.libs.iteratee.Iteratee
import java.io.File

case class GridFsFile(
		_id: Oid,
		contentType: Option[String],
		filename: String,
		uploadDate: Option[Long],
		chunkSize: Int,
		length: Int,
		md5: Option[String],
		metadata: JsValue) extends ModelEntity[GridFsFile] {

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

	def fromFile(file: ReadFile[BSONValue], meta: JsValue): GridFsFile = {
		GridFsFile(
			Oid(file.id.asInstanceOf[BSONObjectID].stringify),
			file.contentType,
			file.filename,
			file.uploadDate,
			file.chunkSize,
			file.length,
			file.md5,
			meta)
	}

	def findOne(oid: Oid)(implicit exec: ExecutionContext): Future[Option[(ReadFile[BSONValue], GridFsFile)]] = {
		Logger.debug("find " + oid.$oid)
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
		}.recover {
			case ex =>
				ex.printStackTrace()
				None
		}
	}

	def write(fileId: Oid, output: OutputStream)(implicit exec: ExecutionContext): Future[Unit] = {
		GridFsFile.findOne(fileId).flatMap {
			case Some((file, model)) => {
				val fileEnumerator = gridFs.enumerate(file)
				(fileEnumerator |>>> Iteratee.foreach { chunk =>
					Logger.debug("write chunk of " + file.filename)
					output.write(chunk)
				})
			}
			case _ => Future.successful(Unit)
		}
	}

	def zipGridFsFile(fileIds: Seq[Oid], output: OutputStream)(implicit exec: ExecutionContext): Future[Unit] = {
		val zipOutStream = new ZipOutputStream(output)

		Future.fold(fileIds.map(GridFsFile.findOne))(Future.successful(Unit)) {
			case (acc, Some((file, model))) =>
				acc.flatMap { _ =>
					zipOutStream.putNextEntry(new ZipEntry(file.filename))
					Logger.debug("new entry " + file.filename)

					val fileEnumerator = gridFs.enumerate(file)
					(fileEnumerator |>>> Iteratee.foreach { chunk =>
						Logger.debug("write chunk of " + file.filename)
						zipOutStream.write(chunk)
					}).map { _ =>
						Logger.debug("close entry " + file.filename)
						zipOutStream.closeEntry()
						Unit
					}
				}.recover {
					case ex =>
						Logger.error("entry write failed", ex)
						Unit
				}
			case _ =>
				Logger.debug("not find file")
				Future.successful(Unit)
		}.flatMap {
			_.map { _ =>
				Logger.debug("close stream")
				zipOutStream.close()
			}
		}
	}
}
