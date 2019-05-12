package org.codeoverflow.chatoverflow.ui.web.rest

object DTOs {

  case class PluginType(name: String, author: String, description: String,
                        majorAPIVersion: Int, minorAPIVersion: Int, state: String)

  case class PluginInstance(instanceName: String, pluginName: String, pluginAuthor: String, isRunning: Boolean, requirementIDs: Seq[String])

  case class ConfigInfo(name: String, apiMajorVersion: Int, apiMinorVersion: Int, pluginFolderPath: String,
                        configFolderPath: String, requirementPackage: String)

  case class Requirement(uniqueRequirementId: String, name: String, isOptional: Boolean, isSet: Boolean, value: Any, targetType: String)

  case class Types(pluginTypes: Seq[PluginType], requirementTypes: RequirementTypes, connectorTypes: Seq[String])

  case class RequirementTypes(input: Seq[String], output: Seq[String], parameter: Seq[String])

}
