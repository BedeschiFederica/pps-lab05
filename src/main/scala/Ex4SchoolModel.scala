import util.*
import Optionals.*
import Optional.*
import Sequences.*
import Sequence.*
import SetADT.*

trait Teacher:
  def name: String

object Teacher:
  /**
   * This a factory method for create a teacher from a name
   * e.g.,
   * teacher("John") // => Teacher("John")
   * Note!! The internal representation of a teacher may vary, decide what is the best for you
   *
   * @param name the name of the teacher
   * @return the teacher created
   */
  def apply(name: String): Teacher = TeacherImpl(name)

  private case class TeacherImpl(override val name: String) extends Teacher

trait Course:
  def name: String

object Course:
  /**
   * This a factory method for create a course from a name
   * e.g.,
   * course("Math") // => Course("Math")
   * Note!! The internal representation of a course may vary, decide what is the best for you
   *
   * @param name the name of the course
   * @return the course created
   * */
  def apply(name: String): Course = CourseImpl(name)

  private case class CourseImpl(override val name: String) extends Course

trait School:
  /**
   * This method should return the list of courses
   * e.g.,
   * emptySchool.courses // => Nil()
   * emptySchool.setTeacherToCourse(teacher("John"), course("Math")).courses // => Cons("Math", Nil())
   * emptySchool
   *  .setTeacherToCourse(teacher("John"), course("Math"))
   *  .setTeacherToCourse(teacher("John"), course("Italian")).courses // => Cons("Math", Cons("Italian", Nil()))
   * Note!! If there are duplicates, just return them once
   * @return the list of courses
   */
  def courses: Sequence[String]
  /**
   * This method should return the list of teachers
   * e.g.,
   * emptySchool.teachers // => Nil()
   * emptySchool.setTeacherToCourse(teacher("John"), course("Math")).teachers // => Cons("John", Nil())
   * val john = teacher("John")
   * emptySchool
   *  .setTeacherToCourse(john, course("Math"))
   *  .setTeacherToCourse(john, course("Italian")).teachers // => Cons("John", Nil())
   * Note!! If there are duplicates, just return them once
   * @return the list of teachers
   */
  def teachers: Sequence[String]
  /**
   * This method should return a new school with the teacher assigned to the course
   * e.g.,
   * emptySchool
   *   .setTeacherToCourse(teacher("John"), course("Math")) // => School(courses = Cons("Math", Nil()), teachers = Cons("John", Nil()), teacherToCourses = Cons(("John", "Math"), Nil()))
   *  */
  def setTeacherToCourse(teacher: Teacher, course: Course): School
  /**
   * This method should return the list of courses assigned to a teacher
   * e.g.,
   * emptySchool.coursesOfATeacher(teacher("John")) // => Nil()
   * emptySchool
   *   .setTeacherToCourse(teacher("John"), course("Math"))
   *   .coursesOfATeacher(teacher("John")) // => Cons("Math", Nil())
   * emptySchool
   *   .setTeacherToCourse(teacher("John"), course("Math"))
   *   .setTeacherToCourse(teacher("John"), course("Italian"))
   *   .coursesOfATeacher(teacher("John")) // => Cons("Math", Cons("Italian", Nil()))
   * @return the list of courses assigned to a teacher
   */
  def coursesOfATeacher(teacher: Teacher): Sequence[Course]
  /**
   * This method should return true if the teacher is present in the school
   * e.g.,
   * emptySchool.hasTeacher("John") // => false
   * emptySchool
   *  .setTeacherToCourse(teacher("John"), course("Math"))
   *  .hasTeacher("John") // => true
   *
   */
  def hasTeacher(name: String): Boolean
  /**
   * This method should return true if the course is present in the school
   * e.g.,
   * emptySchool.hasCourse("Math") // => false
   * emptySchool
   *  .setTeacherToCourse(teacher("John"), course("Math"))
   *  .hasCourse("Math") // => true
   *
   */
  def hasCourse(name: String): Boolean

object School:
  /**
   * This method should return an empty school, namely a school without any teacher and course
   * e.g.,
   * emptySchool // => School(courses = Nil(), teachers = Nil(), teacherToCourses = Nil())
   * NOTE!! The above is just an example, the internal representation may vary, decide what is the best for you
   * You can store just the teacherToCourses, or having a case class for the school, or whatever you think is the best
   *
   * @return the empty school
   */
  def apply(): School = SchoolImpl(fromSequence(empty))

  // new methods for Set
  extension [A](s: Set[A])
    def add(a: A): Set[A] = if s.contains(a) then s else fromSequence(Cons(a, s.toSequence()))

    def find(pred: A => Boolean): Optional[A] = s.toSequence() match
      case Cons(h, _) if pred(h) => Just(h)
      case Cons(_, t) => t.find(pred)
      case _ => Empty()

  private case class TeacherInfo(teacher: Teacher, var courses: Set[Course]):
    def add(course: Course): Unit = courses = courses.add(course)

  private case class SchoolImpl(private var teacherInfos: Set[TeacherInfo]) extends School:
    private def find(t: Teacher): Optional[TeacherInfo] = teacherInfos.find(_.teacher == t)
    private def get(t: Teacher): TeacherInfo = find(t).orElse(TeacherInfo(Teacher(""), fromSequence(empty)))

    override def courses: Sequence[String] = teacherInfos.toSequence().flatMap(_.courses.toSequence()).map(_.name)
    override def teachers: Sequence[String] = teacherInfos.toSequence().map(_.teacher.name)

    override def setTeacherToCourse(teacher: Teacher, course: Course): School =
      if find(teacher) == Empty() then
        teacherInfos = teacherInfos.add(TeacherInfo(teacher, fromSequence(Sequence(course))))
      else
        get(teacher).add(course)
      this

    override def coursesOfATeacher(teacher: Teacher): Sequence[Course] = get(teacher).courses.toSequence()
    override def hasTeacher(name: String): Boolean = !find(Teacher(name)).isEmpty
    override def hasCourse(name: String): Boolean = !courses.find(_ == name).isEmpty

@main def examples(): Unit =
  val school = School()
  println(school.teachers) // Nil()
  println(school.courses) // Nil()
  println(school.hasTeacher("John")) // false
  println(school.hasCourse("Math")) // false
  val john = Teacher("John")
  val math = Course("Math")
  val italian = Course("Italian")
  val school2 = school.setTeacherToCourse(john, math)
  println(school2.teachers) // Cons("John", Nil())
  println(school2.courses) // Cons("Math", Nil())
  println(school2.hasTeacher("John")) // true
  println(school2.hasCourse("Math")) // true
  println(school2.hasCourse("Italian")) // false
  val school3 = school2.setTeacherToCourse(john, italian)
  println(school3.teachers) // Cons("John", Nil())
  println(school3.courses) // Cons("Math", Cons("Italian", Nil()))
  println(school3.hasTeacher("John")) // true
  println(school3.hasCourse("Math")) // true
  println(school3.hasCourse("Italian")) // true
  println(school3.coursesOfATeacher(john)) // Cons("Math", Cons("Italian", Nil()))


