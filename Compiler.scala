package edu.towson.cosc455.bcaven1.project1
import scala.io.Source

object Compiler {

  var currentToken : String = ""
  var fileContents : String = ""

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySemanticAnalyzer

  def main(args: Array[String]): Unit = {

    checkFile(args)
    readFile(args(0))

    Scanner.getNextToken()
    Parser.gittex()
    SemanticAnalyzer.check()
    SemanticAnalyzer.convert()

  }

    def readFile(file : String) = {
      val source = scala.io.Source.fromFile(file)
      fileContents = try source.mkString finally source.close()
    }

    def checkFile(args : Array[String]) = {
      if (args.length != 1) {
        println("USAGE ERROR: wrong number of args fool!")
        System.exit(1)
      }
      else if (! args(0).endsWith(".mkd")) {
        println("USAGE ERROR: wrong extension fool!")
        System.exit(1)
      }
    }
}
