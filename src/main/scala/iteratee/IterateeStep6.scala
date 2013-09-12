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

object IterateeStep6 {

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

  val readHeader = matchLine("Header", "# ")

  val readAlternateHeader = matchLine("Header 2", "## ")

  val readSingleBody = matchLine("Body", ". ")

  val readTrailer = matchLine("Trailer", "! ")

  def matchLine(name: String, prefix: String) = new Consumer[String, String] {
    def consume(input: Input[String]): ConsumerState[String, String] =
      input match {
        case Chunk(c) if c.startsWith(prefix) => Done(c.substring(prefix.length), Empty)
        case Chunk(c) => Error(new Exception(s"Not $name line"))
        case Empty => Continue(this, Empty)
        case EOF => Error(new Exception(s"$name expected"))
      }
  }

  def repeat[I, T](consumer: Consumer[I, T]) = repeatRecursive(consumer, consumer, Nil)

  def repeatRecursive[I, T](consumer: Consumer[I, T], next: Consumer[I, T], accumulated: List[T]): Consumer[I, List[T]] =
    new Consumer[I, List[T]] {
      def consume(input: Input[I]): ConsumerState[I, List[T]] =
        next.consume(input) match {
          case Error(e) if consumer == next => Done(accumulated.reverse, input)
          case Error(e) => Error(e)
          case Continue(step, remainder) => Continue(repeatRecursive(consumer, step, accumulated), remainder)
          case Done(value, remainder) => Continue(repeatRecursive(consumer, consumer, value :: accumulated), remainder)
        }
    }

  def optional[I, T](consumer: Consumer[I, T]): Consumer[I, Option[T]] = new Consumer[I, Option[T]] {
    def consume(input: Input[I]): ConsumerState[I, Option[T]] = consumer.consume(input) match {
      case Done(value, remainder) => Done(Some(value), remainder)
      case Error(e) => Done(None, input)
      case Continue(next, remainder) => Continue(optional(next), remainder)
    }
  }

  def alternate[I, T](consumer1: Consumer[I, T], consumer2: Consumer[I, T]): Consumer[I, T] = new Consumer[I, T] {
    def consume(input: Input[I]): ConsumerState[I, T] =
      consumer1.consume(input) match {
        case done@Done(value, remainder) => done
        case Error(_) => Continue(consumer2, input)
        case Continue(next, remainder) => Continue(alternate(next, consumer2), remainder)
      }
  }

  val readMessage = for {
    head <- readHeader
    body <- repeat(readSingleBody)
    tail <- readTrailer
  } yield (head, body, tail)

  val readMessageWithOptional = for {
    head <- readHeader
    body <- repeat(readSingleBody)
    tail <- optional(readTrailer)
  } yield (head, body, tail)

  val readMessageWithOptionalAlternateHeader = for {
    head <- alternate(readHeader, readAlternateHeader)
    body <- repeat(readSingleBody)
    tail <- optional(readTrailer)
  } yield (head, body, tail)

  def main(args: Array[String]) {
    val msg = List(
      "# Header",
      ". Body 1",
      ". Body 2",
      ". Body 3",
      "! Trailer")

    println(readMessage.consumeAll(msg))
    println(readMessage.consumeAll(msg.init))
    println(readMessageWithOptional.consumeAll(msg))
    println(readMessageWithOptional.consumeAll(msg.init))
    println(readMessageWithOptional.consumeAll(msg.take(1)))
    println(readMessageWithOptional.consumeAll(Nil))
    println("---- Alternate ----")
    println(readMessageWithOptionalAlternateHeader.consumeAll(msg))
    println(readMessageWithOptionalAlternateHeader.consumeAll("## Cabecalho" :: msg.tail))
  }

}