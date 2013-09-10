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

object IterateeFromCooking {

  trait Consumer[I, T] {
    self =>
    def consume(input: I): T

    def flatMap[S](f: T => Consumer[I, S])(implicit manifest: Manifest[S]): Consumer[I, S] = {
      new Consumer[I, S] {
        def consume(input: I): S = f(self.consume(input)).consume(input)
      }
    }

    def map[S](f: T => S)(implicit manifest: Manifest[S]): Consumer[I, S] = {
      new Consumer[I, S] {
        def consume(input: I): S = f(self.consume(input))
      }
    }
  }

  val appetizerCook = new Consumer[Int, List[Int]] {
    def consume(guestCount: Int): List[Int] = (1 to guestCount).map(_ + guestCount).toList
  }
  val dessertCook = new Consumer[Int, String] {
    def consume(guestCount: Int): String = "(> " * guestCount
  }
  val mainCourseCook = new Consumer[Int, List[String]] {
    def consume(guestCount: Int) = List("Meat", "Fish", "Chicken", "Shrimp", "Vegan").take(guestCount)
  }

  val mealCook = for {
    appetizer <- appetizerCook
    mainCourse <- mainCourseCook
    dessert <- dessertCook
  } yield (appetizer, mainCourse, dessert)

  def main(args: Array[String]) {
    println(appetizerCook.consume(4))
    println(dessertCook.consume(4))
    println(mainCourseCook.consume(4))
    println(mealCook.consume(4))
  }

}