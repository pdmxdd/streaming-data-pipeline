package com.labs1904.hwe.practice

case class Item(description: String, price: Option[Int])

case class WeatherStation(name: String, temperature: Option[Int])

object OptionEither {
  /*
    Returns age of a dog when given a human age.
    Returns None if the input is None.
  */
  def dogAge(humanAge: Option[Int]): Option[Int] = Some(humanAge.getOrElse(return None) * 7)

  /*
    Returns the total cost af any item.
    If that item has a price, then the price + 7% of the price should be returned.
  */
  def totalCost(item: Item): Option[Double] = Some(item.price.getOrElse(return None) * 1.07)

  /*
    Given a list of weather temperatures, calculates the average temperature across all weather stations.
    Some weather stations don't report temperature
    Returns None if the list is empty or no weather stations contain any temperature reading.
   */
  def averageTemperature(temperatures: List[WeatherStation]): Option[Int] = if (temperatures.count(_.temperature.isDefined) > 0) return Some(temperatures.filter(_.temperature.isDefined).map(_.temperature.get).sum / temperatures.count(_.temperature.isDefined)) else return None
//  def averageTemperature(temperatures: List[WeatherStation]): Option[Int] = {
//
//    val filteredStations = temperatures.filter(_.temperature.isDefined)
//    val filteredStationsAmount = filteredStations.size
//    if (filteredStationsAmount == 0) {
//      return None
//    }
//    val totalTemp = filteredStations.map(_.temperature.get).sum
//    println(totalTemp)
//    Some(totalTemp / filteredStationsAmount)
//
//  }
}
