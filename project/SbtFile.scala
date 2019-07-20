import java.io.{BufferedWriter, File, FileWriter, IOException}

/**
  * Represents a simple sbt file's content and methods to create a new sbt file. This is used
  * to create the scala build information necessary to start a new project. 
  * Not intended to (it can't) open/read sbt files.
  *
  * @constructor          Creates a new sbt file.
  * @param name           the name of a sbt project
  * @param version        the version of a sbt project
  * @param plugins        list of paths of sub projects
  * @param apiProjectPath the path of a base api project which every project depends on
  * @param defineRoot     true, if a root project (".") should be defined in the sbt file
  */
class SbtFile(var name: String, var version: String, var plugins: List[Plugin], var apiProjectPath: String, var defineRoot: Boolean) {
  /**
    * Creates a new sbt file. This file will be created without sub projects, an empty api path and no root project.
    *
    * @param name    the name of a sbt project
    * @param version the version of a sbt project
    */
  def this(name: String, version: String) = this(name, version, List(), "", false)

  /**
    * Creates a new sbt file. This file will contain empty name and version strings, no sub projects, an empty api path and no root project.
    */
  def this() = this("", "")

  /**
    * Tries to save the sbt file's content into a defined directory.
    *
    * @param pathAndFileName the path of the sbt file (incl. file name)
    * @return true, if the save process was successful
    */
  def save(pathAndFileName: String): Boolean = {

    val buildFile = new File(pathAndFileName)

    // Write the build file using the SbtFile's toString method.
    val writer = new BufferedWriter(new FileWriter(buildFile))
    try {
      writer.write(this.toString)
      true
    } catch {
      case _: IOException => false
    } finally {
      writer.close()
    }
  }

  /**
    * Returns a string representation of the sbt file's content in valid sbt/scala syntax
    *
    * @return a multiline string containing all defined attributes
    */
  override def toString: String = {

    val sbtContent = new StringBuilder("// GENERATED FILE USING THE CHAT OVERFLOW PLUGIN FRAMEWORK\n")

    if (name != "") {
      sbtContent append "\nname := \"%s\"".format(name)
    }

    if (version != "") {
      sbtContent append "\nversion := \"%s\"".format(version)
    }

    if (plugins.nonEmpty) {
      for (plugin <- plugins) {
        var pluginLine = "\nlazy val %s = (project in file(\"%s\"))".format(plugin.normalizedName, plugin.pluginDirectoryPath)

        if (apiProjectPath != "") {
          pluginLine += ".dependsOn(apiProject)"
        }

        sbtContent append pluginLine
      }
    }

    if (apiProjectPath != "") {
      sbtContent append "\n\nlazy val apiProject = project in file(\"%s\")".format(apiProjectPath)
    }

    if (defineRoot) {
      var rootLine = "\n\nlazy val root = (project in file(\".\")).aggregate(%s)"
        .format(("apiProject" +: plugins.map(_.normalizedName)).mkString(", "))

      if (apiProjectPath != "") {
        rootLine += ".dependsOn(apiProject)"
      }

      sbtContent append rootLine
    }

    sbtContent.mkString
  }
}
