package edu.towson.cosc455.bcaven1.project1

class MyLexicalAnalyzer extends LexicalAnalyzer {

  var index : Int = -1
  var nextChar : Char = ' '
  var endOfFile : Boolean = false
  var token : String = ""
  //adds the next character to the token string
  override def addChar(): Unit = {
    token += nextChar
  }
  //looks to see if potential token matches a constant, returns true if yes
  override def lookup(token : String): Boolean = { return CONSTANTS.allTerms.contains(token.toUpperCase()) }

  /* getNextToken reads the string character by character and evaluates which tokens match CONSTANTS,
  if they are not constants and not plain text, then an error is thrown.
  */
  override def getNextToken(): Unit = {
    token = ""
    getChar()
    getNonBlank()

    if (endOfFile.equals(true)) {
      //do nothing, end of file
    }
    else if (CONSTANTS.symbols.contains(nextChar)){
      token = checkSymbol()
      token = token.map(_.toUpper)

      if (token.endsWith("\n")) {
        token = token.substring(0, token.length-1)
      }
      if (lookup(token)) {
        Compiler.currentToken = token
      }
      else {
        println("LEXICAL ERROR 1 - Illegal token: " + token)
        System.exit(1)
      }
    }
    else if (nextChar.isLetterOrDigit || nextChar.equals(':') || nextChar.equals(',') || nextChar.equals('.')) { // plain text case
      addChar()
      token += itsText()

      if (nextChar.equals(CONSTANTS.slash) || nextChar.equals(CONSTANTS.brackete) || nextChar.equals(CONSTANTS.addresse) || nextChar.equals(CONSTANTS.equals)) {
        index -= 1
      }
      Compiler.currentToken = token
    }
    else if (tabOrSpace()) {
      getNextToken()
    }
    else {
      println("LEXICAL ERROR 2 - Illegal character: " + token)
      System.exit(1)
    }
  }
  //gets char from string, if there is no more chars to read in string, then endOfFile becomes true
  override def getChar(): Unit = {
    index +=1
    if (index < Compiler.fileContents.length()) {
      nextChar = Compiler.fileContents.charAt(index)
      endOfFile = false
    }
    else {
      endOfFile = true
    }
  }

  def getNonBlank(): Unit = { //makes sure that all blank spaces are skipped
    while ((nextChar.equals(' ') || tabOrSpace()) && endOfFile.equals(false))
      getChar()
  }

  //returns if next char is a newline, tab
  def tabOrSpace() : Boolean = { return (nextChar.equals('\n') || nextChar.equals('\t')) } //

  def itsText() : String = { //reads text until sentence ends, or next token appears
    var t : String = ""

    getChar()

    while (!(nextChar.isSpaceChar && CONSTANTS.symbols.contains(nextChar)) && endOfFile.equals(true)) {
      t += nextChar
      getChar()
    }

    if (nextChar.equals('\n')) {
      t += nextChar
    }

    return t //returns text
  }
  //processes through the String and checks to see if they match with the correct tokens in CONSTANTS, if not throw err.
  def checkSymbol() : String = {
    if (nextChar.equals(CONSTANTS.slash)) { // for /
      addChar()
      token += itsText()
      if (nextChar.equals(CONSTANTS.linkb)) {
        addChar()
      }
      if (token.equalsIgnoreCase(CONSTANTS.DOCE)) {
        getNonBlank()
        if (index - Compiler.fileContents.length != 0) {
          index -= 1
          getNextToken()
          println("SYNTAX ERROR 3 - Tokens received after \\END : " + Compiler.currentToken)
          System.exit(1)
        }
      }
    }
    else if (nextChar.equals(CONSTANTS.listitem)) { //for +
      addChar()
      token += itsText()
    }
    else if (nextChar.equals(CONSTANTS.heading)) { // for #
      addChar()
      token += itsText()
    }
    else if (nextChar.equals(CONSTANTS.bold)) { // for *
      addChar()
      getChar()
    }
    else if (nextChar.equals(CONSTANTS.exclamation)) { // for ![address]
      addChar()
      getChar()
      if (nextChar.equals(CONSTANTS.linkb)) {
        addChar()
      }
      else {
        println("LEXICAL ERROR 4 - Illegal character received after '!' : " + nextChar)
        System.exit(1)
      }
    }
    else if (nextChar.equals(CONSTANTS.linkb)) { // [
      addChar()
    }
    else if (nextChar.equals(CONSTANTS.brackete)) { // ]
      addChar()
    }
    else if (nextChar.equals(CONSTANTS.addressb)) { // (
      addChar()
    }
    else if (nextChar.equals(CONSTANTS.addresse)) { // )
      addChar()
    }
    else if (nextChar.equals(CONSTANTS.equals)) { // =
      addChar()
    }

    return token
  }
}