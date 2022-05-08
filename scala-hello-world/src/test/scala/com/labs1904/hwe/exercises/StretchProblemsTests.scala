package com.labs1904.hwe.exercises

import org.scalatest.FunSpec

class StretchProblemsTests extends FunSpec {

  describe("isPalindrome - Working with Strings"){
    it("Returns true if a word is spelled the same forward and backward"){
      val input = "hannah"
      val expected = true

      val actual = StretchProblems.isPalindrome(input)

      assert(actual === expected)
    }

    it("Returns false if a word is not spelled the same forward and backward"){
      val input = "Hello"
      val expected = false

      val actual = StretchProblems.isPalindrome(input)

      assert(actual === expected)
    }

    it("Returns true for 'racecar'") {
      val input = "racecar"
      val expected = true

      val actual = StretchProblems.isPalindrome(input)

      assert(actual === expected)
    }
  }
  describe("Testing determineSwapOneValueAndIndex") {
    it("should work for 54998") {
      assert(StretchProblems.determineSwapOneValueAndIndex(54998.toString.split("")) === (4, 1))
    }
    it("should work for 45071") {
      assert(StretchProblems.determineSwapOneValueAndIndex(45071.toString.split("")) === (0, 2))
    }
    it("should work for 31233") {
      assert(StretchProblems.determineSwapOneValueAndIndex(31233.toString.split("")) === (2, 2))
    }
    it("should work for 22437") {
      assert(StretchProblems.determineSwapOneValueAndIndex(22437.toString.split("")) === (3, 3))
    }
    it("should work for straightforward examples") {
      assert(StretchProblems.determineSwapOneValueAndIndex(12.toString.split("")) === (1, 0))
      assert(StretchProblems.determineSwapOneValueAndIndex(123.toString.split("")) === (2, 1))
      assert(StretchProblems.determineSwapOneValueAndIndex(67809.toString.split("")) === (0, 3))
    }

    it("should return -1 for straightforward examples") {
      assert(StretchProblems.determineSwapOneValueAndIndex(21.toString.split("")) === (-1, -1))
      assert(StretchProblems.determineSwapOneValueAndIndex(54321.toString.split("")) === (-1, -1))
    }
  }
  describe("Testing determineSwapTwoValueAndIndex") {
    it("should work for 54998") {
      assert(StretchProblems.determineSwapTwoValueAndIndex(54998.toString.split("")) === (8, 4))
    }
    it("should work for 45071") {
      assert(StretchProblems.determineSwapTwoValueAndIndex(45071.toString.split("")) === (1, 4))
    }
    it("should work for 31233") {
      assert(StretchProblems.determineSwapTwoValueAndIndex(31233.toString.split("")) === (3, 3))
    }
    it("should work for 22437") {
      assert(StretchProblems.determineSwapTwoValueAndIndex(22437.toString.split("")) === (7, 4))
    }
    it("should work for straightforward examples") {
      assert(StretchProblems.determineSwapTwoValueAndIndex(12.toString.split("")) === (2, 1))
      assert(StretchProblems.determineSwapTwoValueAndIndex(123.toString.split("")) === (3, 2))
      assert(StretchProblems.determineSwapTwoValueAndIndex(67809.toString.split("")) === (9, 4))
    }

    it("should return -1 for straightforward examples") {
      assert(StretchProblems.determineSwapOneValueAndIndex(21.toString.split("")) === (-1, -1))
      assert(StretchProblems.determineSwapOneValueAndIndex(54321.toString.split("")) === (-1, -1))
    }
  }
  describe("Testing swapValuesByIndex") {
    it("should work for 54998") {
      assert(StretchProblems.swapValuesByIndex(1, 4, 54998.toString.split("")) === 58994.toString.split(""))
    }
    it("should work for 45071") {
      assert(StretchProblems.swapValuesByIndex(2, 4, 45071.toString.split("")) === 45170.toString.split(""))
    }
    it("should work for 31233") {
      assert(StretchProblems.swapValuesByIndex(2, 3, 31233.toString.split("")) === 31323.toString.split(""))
    }
    it("should work for 22437") {
      assert(StretchProblems.swapValuesByIndex(4, 3, 22437.toString.split("")) === 22473.toString.split(""))
    }
  }
  describe("Testing splitAndReorder") {
    it("should work for 54998") {
      assert(StretchProblems.splitAndReorder(1, 58994.toString.split("")) === 58499.toString.split(""))
    }
    it("should work for 45071") {
      assert(StretchProblems.splitAndReorder(2, 45170.toString.split("")) === 45107.toString.split(""))
    }
    it("should work for 31233") {
      assert(StretchProblems.splitAndReorder(2, 31323.toString.split("")) === 31323.toString.split(""))
    }
    it("should work for 22437") {
      assert(StretchProblems.splitAndReorder(4, 22473.toString.split("")) === 22473.toString.split(""))
    }
  }
  describe("Testing NextBiggestNumber - Working with numbers, strings, and lists") {
    it("should return the next biggest number for straightforward examples") {
      assert(StretchProblems.getNextBiggestNumber(12) === 21)
      assert(StretchProblems.getNextBiggestNumber(123) === 132)
      assert(StretchProblems.getNextBiggestNumber(67809) === 67890)
    }

    it("should return -1 for straightforward examples") {
      assert(StretchProblems.getNextBiggestNumber(21) === -1)
      assert(StretchProblems.getNextBiggestNumber(54321) === -1)
    }

    it("should work for 52210") {
      assert(StretchProblems.getNextBiggestNumber(52210) === -1)
    }

    it("should work for 95701") {
      assert(StretchProblems.getNextBiggestNumber(95701) === 95710)
    }

    it("should work for 71305") {
      assert(StretchProblems.getNextBiggestNumber(71305) === 71350)
    }

    it("should work for 6358") {
      assert(StretchProblems.getNextBiggestNumber(6358) === 6385)
    }

    it("should work for 25437") {
      assert(StretchProblems.getNextBiggestNumber(25437) === 25473)
    }

    it("should work for 49893") {
      assert(StretchProblems.getNextBiggestNumber(49893) === 49938)
    }

    it("should work for 76778") {
      assert(StretchProblems.getNextBiggestNumber(76778) === 76787)
    }

    it("should work for 2372") {
      assert(StretchProblems.getNextBiggestNumber(2372) === 2723)
    }

    it("should work for 45071") {
      assert(StretchProblems.getNextBiggestNumber(45071) === 45107)
    }

    it("should work for 31233") {
      assert(StretchProblems.getNextBiggestNumber(31233) === 31323)
    }

    it("should work for 50401") {
      assert(StretchProblems.getNextBiggestNumber(50401) === 50410)
    }

    it("should work for 57067") {
      assert(StretchProblems.getNextBiggestNumber(57067) === 57076)
    }

    it("should work for 40272") {
      assert(StretchProblems.getNextBiggestNumber(40272) === 40722)
    }

    it("should work for 54998") {
      assert(StretchProblems.getNextBiggestNumber(54998) === 58499)
    }

    it("should work for 22437") {
      assert(StretchProblems.getNextBiggestNumber(22437) === 22473)
    }
  }

}
