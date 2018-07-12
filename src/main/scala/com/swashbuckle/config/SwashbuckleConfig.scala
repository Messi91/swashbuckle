package com.swashbuckle.config

import com.typesafe.config.{Config, ConfigFactory}
import scala.collection.JavaConverters._

trait SwashbuckleConfigComponent {
  val swashbuckleConfig: SwashbuckleConfig
}

case class SwashbuckleConfig(
  description: String,
  version: String,
  title: String,
  termsOfService: String,
  email: String,
  licenseName: String,
  licenseUrl: String,
  host: String,
  schemes: Seq[String],
  entrypoint: String
)

object SwashbuckleConfig {
  val default: SwashbuckleConfig = apply(ConfigFactory.load)

  def apply(config: Config): SwashbuckleConfig = {
    SwashbuckleConfig(
      description = config.getString("swashbuckle.info.description"),
      version = config.getString("swashbuckle.info.version"),
      title = config.getString("swashbuckle.info.title"),
      termsOfService = config.getString("swashbuckle.info.termsOfService"),
      email = config.getString("swashbuckle.info.contact.email"),
      licenseName = config.getString("swashbuckle.info.license.name"),
      licenseUrl = config.getString("swashbuckle.info.license.url"),
      host = config.getString("swashbuckle.host"),
      schemes = config.getStringList("swashbuckle.schemes").asScala,
      entrypoint = config.getString("swashbuckle.entrypoint")
    )
  }
}
