object StudentManaging {

  case class Student(
      name: String,
      marks: Int,
      totalMarks: Int,
      percentage: Double,
      grade: Char
  )

  def getStudentInfo(): Student = {
    val (name, marks, totalMarks) = getStudentInput()

    val percentage = (marks.toDouble / totalMarks) * 100

    val grade = percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _            => 'D'
    }

    Student(name, marks, totalMarks, percentage, grade)
  }

  def printStudentRecord(student: Student): Unit = {
    println(s"Name: ${student.name}")
    println(s"Marks Obtained: ${student.marks}")
    println(s"Total Marks: ${student.totalMarks}")
    println(s"Percentage: ${student.percentage}")
    println(s"Grade: ${student.grade}")
  }

  def validateInput(
      name: String,
      marks: Int,
      totalMarks: Int
  ): (Boolean, Option[String]) = {
    if (name.trim.isEmpty) {
      (false, Some("Name cannot be empty."))
    } else if (marks < 0 || marks > totalMarks) {
      (
        false,
        Some("Marks should be positive and not exceed total possible marks.")
      )
    } else if (totalMarks <= 0) {
      (false, Some("Total possible marks should be greater than zero."))
    } else {
      (true, None)
    }
  }

  def getStudentInput(): (String, Int, Int) = {
    val name = readInput("Enter student name: ")
    val marks = readInput("Enter marks obtained: ").toInt
    val totalMarks = readInput("Enter total possible marks: ").toInt

    val (isValid, errorMessage) = validateInput(name, marks, totalMarks)
    if (!isValid) {
      println(errorMessage.getOrElse("Invalid input. Please try again."))
      return getStudentInput()
    }

    (name, marks, totalMarks)
  }

  def readInput(prompt: String): String = {
    print(prompt)
    scala.io.StdIn.readLine()
  }

  def main(args: Array[String]): Unit = {
    val studentRecord = getStudentInfo()
    printStudentRecord(studentRecord)
  }
}
