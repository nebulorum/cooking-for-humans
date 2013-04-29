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

object CookingStep1 {

  trait Cook[T] {
    def cookFor(guestCount: Int): T

    def flatMap[S](f: T => Cook[S]):Cook[S] = ???
    def map[S](f: T => S):Cook[S] = ???
  }

    val appetizerCook = new Cook[List[Int]] {
      def cookFor(guestCount: Int): List[Int] = ???
    }
    val dessertCook = new Cook[String] {
      def cookFor(guestCount: Int): String = ???
    }
    val mainCourseCook = new Cook[List[String]] {
      def cookFor(guestCount: Int) = ???
    }

    val mealCook = for {
      appetizer <- appetizerCook
      mainCourse <- mainCourseCook
      dessert <- dessertCook
    } yield (appetizer, mainCourse, dessert)
    println(mealCook)
  }