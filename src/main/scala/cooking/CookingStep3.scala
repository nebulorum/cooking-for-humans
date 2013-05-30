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
package cooking

object CookingStep3 {

  sealed trait Job[T]

  case class Done[T](food: T) extends Job[T]

  case class NextStation[T](nextCook: Cook[T]) extends Job[T]

  trait Cook[T] {
    self =>
    def cookFor(guestCount: Int): Job[T]

    def flatMap[S](f: T => Cook[S])(implicit manifest: Manifest[S]): Cook[S] = {
      new Cook[S] {
        def cookFor(guestCount: Int): Job[S] = self.cookFor(guestCount) match {
          case Done(food) => NextStation(f(food))
          case NextStation(nextCook) => NextStation(nextCook flatMap f)
        }
      }
    }

    def map[S](f: T => S)(implicit manifest: Manifest[S]): Cook[S] = {
      new Cook[S] {
        def cookFor(guestCount: Int): Job[S] = self.cookFor(guestCount) match {
          case Done(food) => Done(f(food))
          case NextStation(nextCook) => NextStation(nextCook map f)
        }
      }
    }
  }

  val appetizerCook = new Cook[List[Int]] {
    def cookFor(guestCount: Int): Job[List[Int]] = Done((1 to guestCount).map(_ + guestCount).toList)
  }
  val dessertCook = new Cook[String] {
    def cookFor(guestCount: Int): Job[String] = Done("(> " * guestCount)
  }
  val mainCourseCook = new Cook[List[String]] {
    def cookFor(guestCount: Int) = Done(List("Meat", "Fish", "Chicken", "Shrimp", "Vegan").take(guestCount))
  }

  val mealCook = for {
    appetizer <- appetizerCook
    mainCourse <- mainCourseCook
    dessert <- dessertCook
  } yield (appetizer, mainCourse, dessert)

  def main(args: Array[String]) {
    println(appetizerCook.cookFor(4))
    println(dessertCook.cookFor(4))
    println(mainCourseCook.cookFor(4))
    println(mealCook.cookFor(4))
    val x:AnyRef = mealCook.cookFor(4) match {
      case NextStation(next) => next.cookFor(3) match {
        case NextStation(next2) => next2.cookFor(5)
        case _ => null
      }
      case _ => null
    }
    println(x)
  }

}