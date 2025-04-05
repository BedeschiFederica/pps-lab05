package ex4

import org.junit.*
import org.junit.Assert.*

import util.Sequences.*
import Sequence.*

class Ex4SchoolModelTest:
  val school: School = School()
  val john: Teacher = Teacher("John")
  val math: Course = Course("Math")
  val italian: Course = Course("Italian")
  val school2: School = school.setTeacherToCourse(john, math)
  val school3: School = school2.setTeacherToCourse(john, italian)

  @Test def testCourses(): Unit =
    assertEquals(Cons("Italian", Cons("Math", Nil())), school3.courses)

  @Test def testTeachers(): Unit =
    assertEquals(Cons("John", Nil()), school3.teachers)

  @Test def testCoursesOfATeacher(): Unit =
    assertEquals(Cons(Course("Italian"), Cons(Course("Math"), Nil())), school3.coursesOfATeacher(john))

  @Test def testHasTeacher(): Unit =
    assertTrue(school3.hasTeacher("John"))
    assertFalse(school3.hasTeacher("Jeff"))

  @Test def testHasCourse(): Unit =
    assertTrue(school3.hasCourse("Math"))
    assertTrue(school3.hasCourse("Italian"))
    assertFalse(school3.hasCourse("Science"))