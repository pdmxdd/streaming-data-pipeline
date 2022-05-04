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

    // reverse array
    val numAsStringArray = theInt.toString.split("").reverse
    // determine swap target one index and value from left to right
    var i = 0;
    var swapTargetOneIndex = -1
    var swapTargetOneValue = -1
    var swapTargetTwoIndex = -1
    var swapTargetTwoValue = -1
    var notIdentified = true;
    while (i < numAsStringArray.length - 1 && notIdentified) {
      if (numAsStringArray(i).toInt > numAsStringArray(i + 1).toInt) {
        swapTargetOneIndex = i
        swapTargetOneValue = numAsStringArray(i).toInt
        notIdentified = false
        swapTargetTwoIndex = i + 1;
        swapTargetTwoValue = numAsStringArray(i+1).toInt
      }
      i += 1
    }
    if (swapTargetOneIndex == -1) {
      return -1
    }
    // determine swap target two index and value of remaining values to the right of swap target one index
    while (i < numAsStringArray.length - 1) {
      if (numAsStringArray(i).toInt > swapTargetOneValue && numAsStringArray(i).toInt < swapTargetTwoValue ) {
        swapTargetTwoValue = numAsStringArray(i).toInt
        swapTargetTwoIndex = i
      }
      i += 1
    }
    // make swap
    numAsStringArray(swapTargetOneIndex) = swapTargetTwoValue.toString
    numAsStringArray(swapTargetTwoIndex) = swapTargetOneValue.toString
    // split into two at swap target one index
    var first = numAsStringArray.slice(0, swapTargetTwoIndex)
    val second = numAsStringArray.slice(swapTargetTwoIndex, numAsStringArray.length)
    // order from largest to smallest in group one
    val first_sorted = first.sorted.reverse
    // rebuild all the pieces and return
    val recombined = first_sorted ++ second
    val recombined_number = recombined.reverse.mkString.toInt
    recombined_number
  }

}
