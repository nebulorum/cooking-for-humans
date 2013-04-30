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

object CookingStep2Variant2 {

  trait Cook[T] {
    self =>
    def cookFor(guestCount: Int): T

    def cookName: String = this.getClass.getCanonicalName

    def acknowledge(message: String) {
      println(cookName + ": " + message)
    }

    def flatMap[S](f: Int => Cook[S])(implicit manifest: Manifest[S]): Cook[S] = {
      acknowledge("flatMapping to " + manifest)

      new Cook[S] {
        def cookFor(guestCount: Int): S = f((guestCount)).cookFor(guestCount)
      }
    }

    def map[S](f: Float => S)(implicit manifest: Manifest[S]): Cook[S] = {
      acknowledge("mapping to " + manifest)
      new Cook[S] {
        def cookFor(guestCount: Int): S = f((guestCount))
      }
    }
  }

  val appetizerCook = new Cook[List[Int]] {
    def cookFor(guestCount: Int): List[Int] = (1 to guestCount).map(_ + guestCount).toList

    override def cookName: String = "Appetizer"
  }
  val dessertCook = new Cook[String] {
    def cookFor(guestCount: Int): String = "(> " * guestCount

    override def cookName: String = "Dessert"
  }
  val mainCourseCook = new Cook[List[String]] {
    def cookFor(guestCount: Int) = List("Meat", "Fish", "Chicken", "Shrimp", "Vegan").take(guestCount)

    override def cookName: String = "MainCourse"
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
  }

}