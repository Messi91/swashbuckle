package com.swashbuckle.service

case class Address(
  houseNumber: Int,
  streetName: String,
  postcode: String
)

case class Person(
  firstName: String,
  lastName: String,
  address: Address
)

case class Message (
  id: Long,
  sender: Person,
  content: String
)

object Message {
  val example = Message(
    id = 0L,
    sender = Person(
      firstName = "Drax",
      lastName = "Baculla",
      address = Address(
        houseNumber = 100,
        streetName = "Bumblebee Avenue",
        postcode = "WC1N 3QA"
      )
    ),
    content = "Why is Gamora?"
  )
}
