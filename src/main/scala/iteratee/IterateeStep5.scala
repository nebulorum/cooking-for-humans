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

object IterateeStep5 {

  sealed trait ConsumerState[I, T]

  case class Done[I, T](value: T, remainder: Input[I]) extends ConsumerState[I, T]

  case class Continue[I, T](next: Consumer[I, T], remainder: Input[I]) extends ConsumerState[I, T]

  case class Error[I, T](error: Throwable) extends ConsumerState[I, T]

  sealed trait Input[+I]

  case class Chunk[I](input: I) extends Input[I]

  case object Empty extends Input[Nothing]

  case object EOF extends Input[Nothing]

  trait Consumer[I, T] {
    self =>
    def consume(input: Input[I]): ConsumerState[I, T]

    def consumeAll(inputs: List[I]): Either[Throwable, T] = {
      self.consume(if (inputs.isEmpty) EOF else Chunk(inputs.head)) match {
        case Done(value, _) => Right(value)
        case Error(error) => Left(error)
        case Continue(next, EOF) => next.consumeAll(Nil)
        case Continue(next, Empty) => next.consumeAll(inputs.tail)
        case Continue(next, Chunk(c)) => next.consumeAll(c :: inputs.tail)
      }
    }

    def flatMap[S](f: T => Consumer[I, S])(implicit manifest: Manifest[S]): Consumer[I, S] = {
      new Consumer[I, S] {
        def consume(input: Input[I]): ConsumerState[I, S] = self.consume(input) match {
          case Error(e) => Error(e)
          case Done(value, remainder) => Continue(f(value), remainder)
          case Continue(nextCook, remainder) => Continue(nextCook flatMap f, remainder)
        }
      }
    }

    def map[S](f: T => S)(implicit manifest: Manifest[S]): Consumer[I, S] = {
      new Consumer[I, S] {
        def consume(input: Input[I]): ConsumerState[I, S] = self.consume(input) match {
          case Done(value, r) => Done(f(value), r)
          case Error(e) => Error(e)
          case Continue(nextCook, remainder) => Continue(nextCook map f, remainder)
        }
      }
    }
  }

  val readHeader = new Consumer[String, String] {
    def consume(input: Input[String]): ConsumerState[String, String] =
      input match {
        case Chunk(c) =>
          if (c.startsWith("# ")) Done(c.substring(2), Empty)
          else Error(new Exception("Not header line"))
        case Empty => Continue(this, Empty)
        case EOF => Error(new Exception("Expected header"))
      }
  }
  val readBody = recursiveBodyRead(Nil)

  def recursiveBodyRead(accumulated: List[String]): Consumer[String, List[String]] = new Consumer[String, List[String]] {
    def consume(input: Input[String]): ConsumerState[String, List[String]] =
      input match {
        case Chunk(c) =>
          if (c.startsWith(". ")) Continue(recursiveBodyRead(c.substring(2) :: accumulated), Empty)
          else Done(accumulated, input)
        case Empty => Continue(this, Empty)
        case EOF => Done(accumulated, EOF)
      }
  }

  val readTrailer = new Consumer[String, String] {
    def consume(input: Input[String]): ConsumerState[String, String] =
      input match {
        case Chunk(c) =>
          if (c.startsWith("! ")) Done(c.substring(2), Empty)
          else Error(new Exception("Not trailer line"))
        case Empty => Continue(this, Empty)
        case EOF => Error(new Exception("Trailer expected"))
      }
  }

  val readOptionalTrailer = new Consumer[String, Option[String]] {
    def consume(input: Input[String]): ConsumerState[String, Option[String]] =
      input match {
        case Chunk(c) =>
          if (c.startsWith("! ")) Done(Some(c.substring(2)), Empty)
          else Error(new Exception("Not trailer line"))
        case Empty => Continue(this, Empty)
        case EOF => Done(None, EOF)
      }
  }

  val readMessage = for {
    head <- readHeader
    body <- readBody
    tail <- readTrailer
  } yield (head, body, tail)

  val readMessageWithOptional = for {
    head <- readHeader
    body <- readBody
    tail <- readOptionalTrailer
  } yield (head, body, tail)

  def main(args: Array[String]) {
    val msg = List(
      "# Header",
      ". Body 1",
      ". Body 2",
      ". Body 3",
      "! Trailer"
    )

    println(readMessage.consumeAll(msg))
    println(readMessage.consumeAll(msg.init))
    println(readMessageWithOptional.consumeAll(msg.init))
    println(readMessageWithOptional.consumeAll(msg.take(1)))
    println(readMessageWithOptional.consumeAll(Nil))
  }

}