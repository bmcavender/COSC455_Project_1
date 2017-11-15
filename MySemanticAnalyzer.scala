package edu.towson.cosc455.bcaven1.project1

import java.awt.Desktop
import java.io.{File, IOException}
import java.io._
import scala.collection.mutable.Stack


class MySemanticAnalyzer extends {

  var s = new scala.collection.mutable.Stack[String]

  /* * Hack Scala/Java function to take a String filename and open in default web browser. */
  def openHTMLFileInBrowser(htmlFileStr : String) = {
    val file : File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }

  def check() : Unit = {
    var temp : String = ""

    while(!temp.equalsIgnoreCase(CONSTANTS.DOCB) && Compiler.Parser.parseTree.nonEmpty) {
      temp = Compiler.Parser.parseTree.pop()
      temp match {
        case CONSTANTS.DOCE => s.push(temp)
        case CONSTANTS.DOCB => s.push(temp)
        case CONSTANTS.DEFB => s.push(temp)
        case CONSTANTS.USEB => s.push(temp) //add more
        case _ => s.push(temp)
      }
    }
    if (temp.equalsIgnoreCase(CONSTANTS.DOCB) && !s.top.equalsIgnoreCase(CONSTANTS.DOCB)) {
      s.push(temp)
    }
  }

  def convert() : Unit = {
    val newFile = new PrintWriter(new File("output.html"))

    while (s.nonEmpty) {
      var outString = s.pop()
      outString match {
        case CONSTANTS.DOCB => newFile.append("<html>\n")
        case CONSTANTS.DOCE => newFile.append("\n</html>")
        case CONSTANTS.TITLEB =>
          newFile.append("<head>\n<title> ")
          outString = s.pop()
          while (!outString.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
            newFile.append(outString + " ")
            outString = s.pop()
          }
          newFile.append(" </title>\n</head>\n")

        case CONSTANTS.HEADING =>
          newFile.append("<h1> ")
          outString = s.pop()
          while (!CONSTANTS.allTerms.contains(outString)) {
            newFile.append(outString + " ")
            outString = s.pop()
          }
          s.push(outString)
          newFile.append(" </h1>\n")

        case CONSTANTS.PARAB => newFile.append("<p> ")

        case CONSTANTS.PARAE => newFile.append(" </p>\n")

        case CONSTANTS.LINKB =>
          var linkName: String = ""
          var url: String = ""
          outString = s.pop()
          while (!outString.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
            linkName = linkName + s + " "
            outString = s.pop()
          }
          s.pop()
          url += s.pop()
          s.pop()
          newFile.append("<a href=\"" + url + "\">" + linkName + "</a> ")

        case CONSTANTS.LISTITEM =>
          newFile.append("\n<li> ")

          outString = s.pop()
          if (s.contains("\n")) {
            newFile.append(outString + " </li>")
          }
          else if (outString.equalsIgnoreCase(CONSTANTS.USEB)) {
            s.push(outString)
          }
          else {
            if (!outString.equalsIgnoreCase(CONSTANTS.USEB)) {
              newFile.append(outString + " ")
              outString = s.top
              if (outString.contains("\n")) {
                outString = s.pop()
                newFile.append(outString + " </li>")
              }
            }
          }

        case CONSTANTS.NEWLINE => newFile.append("<br>\n")

        case CONSTANTS.IMAGEB =>

          var linkText: String = ""
          var url: String = ""

          outString = s.pop()
          while (!outString.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
            linkText = linkText + s + " "
            outString = s.pop()
          }
          s.pop()
          url += s.pop()
          s.pop()

          newFile.append("<img src=\"" + url + "\" alt=" + linkText + "\">")

        case CONSTANTS.BOLD =>
          var text: String = ""

          outString = s.pop()
          while (!outString.equalsIgnoreCase(CONSTANTS.BOLD)) {
            text += outString
            outString = s.pop()
          }
          newFile.append("<b> " + text + " </b>")

        case CONSTANTS.DEFB =>
          s.pop()
          s.pop()

        case CONSTANTS.USEB => s.pop()

        case _ => newFile.append(outString + " ")
      }
    }
    newFile.close()
    openHTMLFileInBrowser("outputFile.html")
  }
}
