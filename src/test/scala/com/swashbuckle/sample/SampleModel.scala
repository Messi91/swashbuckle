package com.swashbuckle.sample

object SampleModel {
  case class Message (
    id: Long,
    sender: String,
    content: String
  )

  object Message {
    val example = Message(
      id = 0L,
      sender = "Drax",
      content = "Why is Gamora?"
    )
  }
}
