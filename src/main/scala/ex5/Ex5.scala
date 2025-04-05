package ex5

import util.Sequences.*
import Sequence.*

import ex.Course

object SameCategory:
  def unapply(courses: Sequence[Course]): Option[String] = courses match
    case Cons(h, t) => if t.filter(_.category != h.category) == Nil() then Some(h.category) else None
    case _ => None

@main def main(): Unit =
  val scalaCourse = Course("SCALA01", "Functional Programming in Scala", "Prof. Odersky", "Programming")
  val pythonCourse = Course("PYTHON01", "Introduction to Python", "Prof. van Rossum", "Programming")
  val designCourse = Course("DESIGN01", "UI/UX Design Fundamentals", "Prof. Norman", "Design")
  val javaCourse = Course("JAVA01", "OOP in Java", "Prof. Odersky", "Programming")

  val coursesDiffCat = Sequence(scalaCourse, pythonCourse, designCourse)
  coursesDiffCat match
    case SameCategory(cat) => println(s"$coursesDiffCat have same category $cat")
    case _ => println(s"$coursesDiffCat have different categories")

  val coursesSameCat = Sequence(scalaCourse, pythonCourse, javaCourse)
  coursesSameCat match
    case SameCategory(cat) => println(s"$coursesSameCat have same category $cat")
    case _ => println(s"$coursesSameCat have different categories")