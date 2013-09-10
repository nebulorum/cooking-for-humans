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

  case class Done[I, T](food: T) extends ConsumerState[I, T]

  case class Continue[I, T](nextConsumer: Consumer[I, T]) extends ConsumerState[I, T]

  trait Consumer[I, T] {
    self =>
    def consume(input: I): ConsumerState[I, T]

    def consumeAll(inputs: List[I]): Option[T] = {
      self.consume(inputs.head) match {
        case Done(value) => Some(value)
        case Continue(station) => station.consumeAll(inputs.tail)
      }
    }

    def flatMap[S](f: T => Consumer[I, S])(implicit manifest: Manifest[S]): Consumer[I, S] = {
      new Consumer[I, S] {
        def consume(guestCount: I): ConsumerState[I, S] = self.consume(guestCount) match {
          case Done(food) => Continue(f(food))
          case Continue(nextCook) => Continue(nextCook flatMap f)
        }
      }
    }

    def map[S](f: T => S)(implicit manifest: Manifest[S]): Consumer[I, S] = {
      new Consumer[I, S] {
        def consume(guestCount: I): ConsumerState[I, S] = self.consume(guestCount) match {
          case Done(food) => Done(f(food))
          case Continue(nextCook) => Continue(nextCook map f)
        }
      }
    }
  }

  //TODO serve x
  val appetizerCook = new Consumer[Int, List[Int]] {
    def consume(guestCount: Int): ConsumerState[Int, List[Int]] = Done((1 to guestCount).map(_ + guestCount).toList)
  }
  val dessertCook = new Consumer[Int, String] {
    def consume(guestCount: Int): ConsumerState[Int, String] = Done("(> " * guestCount)
  }
  val mainCourseCook = new Consumer[Int, List[String]] {
    def consume(guestCount: Int) = Done(List("Meat", "Fish", "Chicken", "Shrimp", "Vegan").take(guestCount))
  }

  def cookSeveral[I, T](count: Int, accumulated: List[T], cook: Consumer[I, T]): Consumer[I, List[T]] = new Consumer[I, List[T]] {
    def consume(input: I): ConsumerState[I, List[T]] =
      cook.consume(input) match {
        case Done(v) =>
          if (count > 1)
            Continue[I, List[T]](cookSeveral(count - 1, accumulated ::: List(v), cook))
          else
            Done(accumulated ::: List(v))
        case Continue(next) => Continue(cookSeveral(count, accumulated, next))
      }
  }

  val mealCook = for {
    appetizer <- appetizerCook
    mainCourse <- mainCourseCook
    dessert <- dessertCook
  } yield (appetizer, mainCourse, dessert)

  val greedyCooks = for {
    appetizer <- cookSeveral(3, Nil, appetizerCook)
    mainCourse <- mainCourseCook
    dessert <- dessertCook
  } yield (appetizer, mainCourse, dessert)

  def main(args: Array[String]) {
    println(appetizerCook.consume(4))
    println(dessertCook.consume(4))
    println(mainCourseCook.consume(4))
    println(mealCook.consume(4))
    println(mealCook.consumeAll(List(2, 4, 5, 3, 5)))
    println(greedyCooks.consumeAll(List(2, 4, 5, 3, 5, 2)))
  }

}