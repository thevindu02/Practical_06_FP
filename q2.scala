import scala.io.StdIn.readLine

object StudentRecords {

    def getStudentaInfo(): (String,Int,Int,Double,Char) = {

        val name = readLine("Enter student's name:")
        val marks = readLine("Enter student's marks:").toInt
        val totalMarks = readLine("Enter total possible marks").toInt

        val percentage = (marks.toDouble/totalMarks) * 100;
        val grade = percentage match {

            case p if p >= 90 => 'A'
            case p if p >= 75 => 'B'
            case p if p >= 50 => 'C'
            case _            => 'D'
        }

        (name, marks, totalMarks, percentage, grade)

    }

    def printStudentRecord(record:(String,Int,Int,Double,Char)): Unit = {

        val (name,marks,totalMarks,percentage,grade) = record
        println(s"Student Name:$name")
        println(s"Marks obtained:$marks")
        println(s"Total Possible Marks: $totalMarks")
        println(s"Percentage: $percentage%R")
        println(s"Grade: $grade")
    }

    def validateInput(name:String,marks:Int,totalMarks:Int):(Boolean,Option[String]) = {

        if(name.isEmpty) {

            (false,Some("Name cannot be empty"))
        }
        if( marks < 0 || marks > totalMarks) {

            (false,Some("Marks should be a positive integer and not exceed total marks"))
        }
        if(totalMarks <= 0)
        {
            (false, Some("Total possible marks should be a positive integer"))

        }
        if((name.isEmpty) && (marks < 0 || marks > totalMarks) )
        {
            (false,Some("Name cannot be empty and Marks should be a positive integer and not exceed total marks"))
        }
        if((name.isEmpty) && (totalMarks <= 0))
        {
            (false,Some("Name cannot be empty and Total possible marks should be a positive integer"))
        }
        if((marks < 0 || marks > totalMarks)&&(totalMarks <= 0))
        {
            (false,Some("marks should be a positive integer and not exceed total marks and Total marks should be a positive integer"))
        }
        if((name.isEmpty)&&( marks < 0 || marks > totalMarks)&&(totalMarks <= 0))
        {
            (false,Some("Name should not be empty,Total possible marks should be a positive integer,Marks should be a positive integer and not exceed total marks"))
        }
        else
        {
            (true,None)
        }
    }

    def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
        var isValid = false
        var record: (String, Int, Int, Double, Char) = ("", 0, 0, 0.0, 'D')

        while (!isValid) {
            val name = readLine("Enter student's name: ")
            val marks = readLine("Enter student's marks: ").toInt
            val totalMarks = readLine("Enter total possible marks: ").toInt

            val (valid, errorMessage) = validateInput(name, marks, totalMarks)

                if (valid) {
                    isValid = true
                    val percentage = (marks.toDouble / totalMarks) * 100
                    val grade = percentage match {
                        case p if p >= 90 => 'A'
                        case p if p >= 75 => 'B'
                        case p if p >= 50 => 'C'
                        case _            => 'D'
                }
                    record = (name, marks, totalMarks, percentage, grade)
                } 
                else 
                {
                    println(s"Invalid input: ${errorMessage.getOrElse("Unknown error")}. Please try again.")
                }

            }

                record
        }

    def main(args:Array[String]):Unit = {

        val studentRecord = getStudentInfoWithRetry()
        printStudentRecord(studentRecord)
    }

}