package com.swashbuckle.config

import com.typesafe.config.{Config, ConfigFactory}

trait MicroServiceConfigComponent {
  val config: MicroServiceConfig
}

class MicroServiceConfig(config: Config) {
  val port: Int = config.getInt("http.port")
  val interface: String = config.getString("http.interface")
}

object MicroServiceConfig extends MicroServiceConfig(ConfigFactory.load())
