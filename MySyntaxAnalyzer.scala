package edu.towson.cosc455.bcaven1.project1

class MySyntaxAnalyzer extends SyntaxAnalyzer{

  var parseTree = new scala.collection.mutable.Stack[String]

  override def gittex(): Unit = {

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)){
      // add to parse tree / stack
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      title()
      variableDefine()
      body()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        parseTree.push(Compiler.currentToken)
      }
      else {
        println("SYNTAX ERROR 1 - Received " + Compiler.currentToken + " when " + CONSTANTS.DOCE + " expected!")
        System.exit(1)
      }
    }
    else {
      println("SYNTAX ERROR 2 - Received " + Compiler.currentToken + " when " + CONSTANTS.DOCB + " expected!")
      System.exit(1)
    }
  }

  override def paragraph(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
        variableDefine()
      }
      innerText()

      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("SYNTAX ERROR 3 - Received " + Compiler.currentToken + " when " + CONSTANTS.PARAB + " expected!")
        System.exit(1)
      }
    }
    else {
      println("SYNTAX ERROR 4 - Received " + Compiler.currentToken + " when " + CONSTANTS.PARAE + " expected!")
      System.exit(1)
    }
  }

  override def innerItem(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      variableUse()
      innerItem()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      bold()
      innerItem()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      link()
      innerItem()
    }
    else if (Compiler.Scanner.endOfFile.equals(true)) { }
    else if (isitText()) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      innerItem()
    }
  }

  override def innerText(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      variableUse()
      innerText()
    }
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      heading()
      innerText()
    }
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      bold()
      innerText()
    }
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      listItem()
      innerText()
    }
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      image()
      innerText()
    }
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      link()
      innerText()
    }
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      newline()
      innerText()
    }
    else if (Compiler.Scanner.endOfFile.equals(true)) { }
    else if (isitText()) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      innerText()
    }
  }

  override def link(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      justText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
            parseTree.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
          }
          else {
            println("SYNTAX ERROR 5 - Received " + Compiler.currentToken + " when " + CONSTANTS.ADDRESSE + " expected!")
            System.exit(1)
          }
        }
        else {
          println("SYNTAX ERROR 6 - Received " + Compiler.currentToken + " when " + CONSTANTS.ADDRESSB + " expected!")
          System.exit(1)
        }
      }
      else {
        println("SYNTAX ERROR 7 - Received " + Compiler.currentToken + " when " + CONSTANTS.BRACKETE + " expected!")
        System.exit(1)
      }
    }
  }

  override def body(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)){
      paragraph()
      body()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      newline()
      body()
    }
    else if (Compiler.Scanner.endOfFile.equals(true)) {}
    else {
      innerText()
      body()
    }
  }

  override def bold(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      justText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("SYNTAX ERROR 8 - Received " + Compiler.currentToken + " when " + CONSTANTS.BOLD + " expected!")
        System.exit(1)
      }
    }
  }

  override def newline(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
  }

  override def title(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      justText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("SYNTAX ERROR 9 - Received " + Compiler.currentToken + " when " + CONSTANTS.BRACKETE + " expected!")
        System.exit(1)
      }
    }
    else {
      println("SYNTAX ERROR 10 - Received " + Compiler.currentToken + " when " + CONSTANTS.TITLEB + " expected!")
      System.exit(1)
    }
  }

  override def variableDefine(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      justText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        justText()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          variableDefine()
        }
        else {
          println("SYNTAX ERROR 11 - Received " + Compiler.currentToken + " when " + CONSTANTS.BRACKETE + " expected!")
          System.exit(1)
        }
      }
      else {
        println("SYNTAX ERROR 12 - Received " + Compiler.currentToken + " when " + CONSTANTS.EQSIGN + " expected!")
        System.exit(1)
      }
    }
  }

  override def image(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      justText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
            parseTree.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
          }
          else {
            println("SYNTAX ERROR 13 - Received " + Compiler.currentToken + " when " + CONSTANTS.ADDRESSE + " expected!")
            System.exit(1)
          }
        }
        else {
          println("SYNTAX ERROR 14 - Received " + Compiler.currentToken + " when " + CONSTANTS.ADDRESSB + " expected!")
          System.exit(1)
        }
      }
      else {
        println("SYNTAX ERROR 15 - Received " + Compiler.currentToken + " when " + CONSTANTS.BRACKETE + " expected!")
        System.exit(1)
      }
    }
  }

  override def variableUse(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      justText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("SYNTAX ERROR 16 - Received " + Compiler.currentToken + " when " + CONSTANTS.BRACKETE + " expected!")
        System.exit(1)
      }
    }
  }

  override def heading(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      justText()
    }
  }

  override def listItem(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
  }


  //if the current token is a symbol that is used within text, then return true
  //if the current token is a newline, filter the length of the token to only text
  def isitText(): Boolean = {
    if (Compiler.currentToken.contains(':') || Compiler.currentToken.contains(',') || Compiler.currentToken.contains('.')) {
      return true
    }
    if (Compiler.currentToken.contains("\n")) {
      return (Compiler.currentToken.length == Compiler.currentToken.filter(_.isLetterOrDigit).length+1)
    }
    return (Compiler.currentToken.length == Compiler.currentToken.filter(_.isLetterOrDigit).length)
  }

  //checks to see if the current token is plain text or not, if it is, gets next token until it isn't text
  def justText(): Unit = {
    if (isitText()) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      justText()
    }
    else if (Compiler.Scanner.endOfFile.equals(true)) {}
  }
}
