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

object IterateeStep1 {

  sealed trait ConsumerState[I, T]

  case class Done[I, T](value: T) extends ConsumerState[I, T]

  case class Continue[I, T](next: Consumer[I, T]) extends ConsumerState[I, T]

  trait Consumer[I, T] {
    self =>
    def consume(input: I): ConsumerState[I, T]

    def consumeAll(inputs: List[I]): Option[T] = {
      self.consume(inputs.head) match {
        case Done(value) => Some(value)
        case Continue(next) => next.consumeAll(inputs.tail)
      }
    }

    def flatMap[S](f: T => Consumer[I, S])(implicit manifest: Manifest[S]): Consumer[I, S] = {
      new Consumer[I, S] {
        def consume(guestCount: I): ConsumerState[I, S] = self.consume(guestCount) match {
          case Done(value) => Continue(f(value))
          case Continue(nextCook) => Continue(nextCook flatMap f)
        }
      }
    }

    def map[S](f: T => S)(implicit manifest: Manifest[S]): Consumer[I, S] = {
      new Consumer[I, S] {
        def consume(guestCount: I): ConsumerState[I, S] = self.consume(guestCount) match {
          case Done(value) => Done(f(value))
          case Continue(nextCook) => Continue(nextCook map f)
        }
      }
    }
  }

  val readHeader = new Consumer[String, String] {
    def consume(input: String): ConsumerState[String, String] = Done(input.substring(2))
  }
  val readBody = new Consumer[String, String] {
    def consume(input: String): ConsumerState[String, String] = Done(input.substring(2))
  }
  val readTrailer = new Consumer[String, String] {
    def consume(input: String): ConsumerState[String, String] = Done(input.substring(2))
  }

  val readMessage = for {
    head <- readHeader
    body <- readBody
    tail <- readTrailer
  } yield (head, body, tail)

  def main(args: Array[String]) {
    val msg = List(
      "# Header",
      ". Body",
      "! Trailer"
    )
    println(readHeader.consume(msg(0)))
    println(readBody.consume(msg(1)))
    println(readTrailer.consume(msg(2)))
    println(readMessage.consumeAll(msg))
    println(readMessage.consumeAll(msg.reverse))
  }

}