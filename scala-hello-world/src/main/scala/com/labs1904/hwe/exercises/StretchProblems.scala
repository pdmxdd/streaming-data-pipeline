package com.labs1904.hwe.exercises

object StretchProblems {

  /*
  Checks if a string is palindrome.
 */
  def isPalindrome(s: String): Boolean = {
    var (begin, end) = s.splitAt((s.length / 2).toInt)
    if (begin.length != end.length) {
      end = end.substring(1)
    }
    begin == end.reverse
  }

  /*
For a given number, return the next largest number that can be created by rearranging that number's digits.
If no larger number can be created, return -1
 */
  def getNextBiggestNumber(theInt: Integer): Int = {
    // I've been close on this problem a couple of times, I want to give it another shot
    // part of the issue with this problem is the multiple step process
    // I think breaking the problem down into smaller functions (that are tested) will help tremendously

    // Step Zero: covert Int into StringArray
    val numAsStringArray = theInt.toString.split("")
    // Step One: determine swap one value and index
    val (swapOneValue, swapOneIndex) = determineSwapOneValueAndIndex(numAsStringArray)

    if (swapOneIndex == -1) {
      return -1
    }
    // Step Two: determine swap two value and index
    val (swapTwoValue, swapTwoIndex) = determineSwapTwoValueAndIndex(numAsStringArray)
    // Step Three: make swap
    val newArray = swapValuesByIndex(swapOneIndex, swapTwoIndex, numAsStringArray)

    // Step Four: split array on swap position: hold and reOrder
    val finalArray = splitAndReorder(swapOneIndex, numAsStringArray)
    // Step Five: reorder the reOrder array from largest to smallest

    // Step Six: rebuild the array & return
    finalArray.mkString.toInt
  }

  def determineSwapOneValueAndIndex(numStringArray: Array[String]): (Int, Int) = {
    var theValue = -1
    var theIndex = -1
    var i = numStringArray.length - 2
    var notIdentified = true
    while (i >= 0 && notIdentified) {
      if (numStringArray(i + 1).toInt > numStringArray(i).toInt) {
        theValue = numStringArray(i).toInt
        theIndex = i
        notIdentified = false
      }
      i -= 1
    }
    (theValue, theIndex)
  }

  def determineSwapTwoValueAndIndex(numStringArray: Array[String]): (Int, Int) = {
    // find initial swap two value and index
    var swapOneValue = -1
    var swapOneIndex = -1
    var theValue = -1
    var theIndex = -1
    var i = numStringArray.length - 2
    var notIdentified = true
    while (i >= 0 && notIdentified) {
      if (numStringArray(i + 1).toInt > numStringArray(i).toInt) {
        theValue = numStringArray(i + 1).toInt
        theIndex = i + 1
        swapOneValue = numStringArray(i).toInt
        swapOneIndex = i
        notIdentified = false
      }
      i -= 1
    }
    if (theIndex == -1) {
      return (-1, -1)
    }
    // check that there aren't any better swap two candidates in the numbers after swapTwo position
    i = theIndex;
    while (i < numStringArray.length) {
      if(numStringArray(i).toInt > swapOneValue && numStringArray(i).toInt < theValue) {
        theValue = numStringArray(i).toInt
        theIndex = i
      }
      i += 1
    }
    (theValue, theIndex)
  }

  def swapValuesByIndex(indexOne: Int, indexTwo: Int, numStringArray: Array[String]): Array[String] = {
    val tempVal = numStringArray(indexOne)
    numStringArray(indexOne) = numStringArray(indexTwo)
    numStringArray(indexTwo) = tempVal
    numStringArray
  }

  def splitAndReorder(indexOne: Int, numStringArray: Array[String]): Array[String] = {
    val (hold, reorder) = numStringArray.splitAt(indexOne + 1)
    val reordered = reorder.sorted
    hold ++ reordered
  }
}
