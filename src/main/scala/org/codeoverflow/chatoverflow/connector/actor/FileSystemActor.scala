package org.codeoverflow.chatoverflow.connector.actor

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}

import akka.actor.Actor
import org.codeoverflow.chatoverflow.connector.actor.FileSystemActor._

import scala.io.Source

/**
  * The file system actor provides simple utility methods to read and write files.
  */
class FileSystemActor extends Actor {

  // TODO: Should be an startup option in the CLI
  private val dataFilePath = "data"

  // Create data folder if non existent
  private val dataFolder = new File(dataFilePath)
  if (!dataFolder.exists()) {
    dataFolder.mkdir()
  }

  /**
    * Receives either LoadFile or SaveFile object, acts accordingly.
    *
    * @return a loaded file or a boolean if the saving process was successful
    */
  override def receive: Receive = {
    case LoadFile(pathInResources) =>
      try {
        sender ! Some(Source.fromFile(s"$dataFilePath${fixPath(pathInResources)}").mkString)
      } catch {
        case _: Exception => None
      }
    case LoadBinaryFile(pathInResources) =>
      try {
        sender ! Some(Files.readAllBytes(new File(s"$dataFilePath${fixPath(pathInResources)}").toPath))
      } catch {
        case _: Exception => None
      }
    case SaveFile(pathInResources, content) =>
      try {
        val writer = new PrintWriter(s"$dataFilePath${fixPath(pathInResources)}")
        writer.write(content)
        writer.close()
        sender ! true
      } catch {
        case _: Exception => sender ! false
      }
    case SaveBinaryFile(pathInResources, content) =>
      try {
        Files.write(new File(s"$dataFilePath${fixPath(pathInResources)}").toPath, content)
        sender ! true
      } catch {
        case _: Exception => sender ! false
      }
    case CreateDirectory(folderName) =>
      try {
        sender ! new File(s"$dataFilePath${fixPath(folderName)}").mkdir()
      } catch {
        case _: Exception => sender ! false
      }
  }

  private def fixPath(path: String): String = {
    var fixedPath = Paths.get(File.pathSeparator, path).normalize().toString
    if(fixedPath.startsWith(";")){
      fixedPath = fixedPath.replaceFirst(";", "")
    }
    fixedPath
  }
}

object FileSystemActor {

  /**
    * Send a LoadFile-object to the FileSystemActor to load a specific file and return a string.
    *
    * @param pathInResources the relative Path in the resource folder
    */
  case class LoadFile(pathInResources: String) extends ActorMessage

  /**
    * Send a LoadFile-object to the FileSystemActor to load a specific file and return a byte array.
    *
    * @param pathInResources the relative Path in the resource folder
    */
  case class LoadBinaryFile(pathInResources: String) extends ActorMessage

  /**
    * Send a SaveFile-object to the FileSystemActor to save a file with given content.
    *
    * @param pathInResources the relative Path in the resource folder
    * @param content         the content to save
    */
  case class SaveFile(pathInResources: String, content: String) extends ActorMessage

  /**
    * Send a SaveFile-object to the FileSystemActor to save a file with given content.
    *
    * @param pathInResources the relative Path in the resource folder
    * @param content         the content to save
    */
  case class SaveBinaryFile(pathInResources: String, content: Array[Byte]) extends ActorMessage

  /**
    * Send a CreateDirectory-object to the FileSystemActor to create a new sub directory.
    *
    * @param folderName the folder name. Note: Parent folder has to exist!
    */
  case class CreateDirectory(folderName: String) extends ActorMessage

}
