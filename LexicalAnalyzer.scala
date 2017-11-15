package edu.towson.cosc455.bcaven1.project1

trait LexicalAnalyzer {
  def addChar() : Unit
  def lookup(token : String) : Boolean
  def getNextToken() : Unit
  def getChar() : Unit
  def getNonBlank() : Unit
}
