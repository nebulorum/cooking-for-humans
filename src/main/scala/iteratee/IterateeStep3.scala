/*
 * Copyright (C) 2013-2013 - Thomas Santana <tms@exnebula.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */
package iteratee

object IterateeStep3 {

  sealed trait ConsumerState[I, T]

  case class Done[I, T](value: T) extends ConsumerState[I, T]

  case class Continue[I, T](next: Consumer[I, T]) extends ConsumerState[I, T]

  case class Error[I, T](error: Throwable) extends ConsumerState[I, T]

  trait Consumer[I, T] {
    self =>
    def consume(input: I): ConsumerState[I, T]

    def consumeAll(inputs: List[I]): Either[Throwable, T] = {
      if (inputs.isEmpty) {
        Left(new RuntimeException("Premature end of stream"))
      } else {
        self.consume(inputs.head) match {
          case Done(value) => Right(value)
          case Error(error) => Left(error)
          case Continue(next) => next.consumeAll(inputs.tail)
        }
      }
    }

    def flatMap[S](f: T => Consumer[I, S])(implicit manifest: Manifest[S]): Consumer[I, S] = {
      new Consumer[I, S] {
        def consume(guestCount: I): ConsumerState[I, S] = self.consume(guestCount) match {
          case Error(e) => Error(e)
          case Done(value) => Continue(f(value))
          case Continue(nextCook) => Continue(nextCook flatMap f)
        }
      }
    }

    def map[S](f: T => S)(implicit manifest: Manifest[S]): Consumer[I, S] = {
      new Consumer[I, S] {
        def consume(guestCount: I): ConsumerState[I, S] = self.consume(guestCount) match {
          case Done(value) => Done(f(value))
          case Error(e) => Error(e)
          case Continue(nextCook) => Continue(nextCook map f)
        }
      }
    }
  }

  val readHeader = new Consumer[String, String] {
    def consume(input: String): ConsumerState[String, String] =
      if (input.startsWith("# ")) Done(input.substring(2))
      else Error(new Exception("Not header line"))
  }
  val readBody = recurseBody(Nil)

  def recurseBody(accumulated: List[String]): Consumer[String, List[String]] = new Consumer[String, List[String]] {
    def consume(input: String): ConsumerState[String, List[String]] =
      if (input.startsWith(". ")) Continue(recurseBody(input.substring(2) :: accumulated))
      else Done(accumulated)
  }

  val readTrailer = new Consumer[String, String] {
    def consume(input: String): ConsumerState[String, String] =
      if (input.startsWith("! ")) Done(input.substring(2))
      else Error(new Exception("Not trailer line"))
  }

  val readMessage = for {
    head <- readHeader
    body <- readBody
    tail <- readTrailer
  } yield (head, body, tail)

  def main(args: Array[String]) {
    val message = List(
      "# Header",
      ". Body 1",
      ". Body 2",
      ". Body 3",
      "! Trailer"
    )
    println("Reading a messages will fail")
    println(readMessage.consumeAll(message))
    println("Two trailer fixes problem")
    println(readMessage.consumeAll(message ::: List("! Trailer 2")))
  }

}