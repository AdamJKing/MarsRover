package mars.rover.challenge

import scala.math.floorMod

object Direction extends Enumeration {

  type Direction = Value

  /**
   * Direction represented as a vector.
   *
   * @param xChange the x component (positive left, negative right)
   * @param yChange the y component (positive up, negative down)
   */
  protected class Val(val xChange: Int, val yChange: Int) extends super.Val

  implicit def valueToDirectionVal(v: Value): Val = v.asInstanceOf[Val]

  val North = new Val(xChange = 0, yChange = 1)

  val NorthEast = new Val(xChange = 1, yChange = 1)

  val East = new Val(xChange = 1, yChange = 0)

  val SouthEast = new Val(xChange = 1, yChange = -1)

  val South = new Val(xChange = 0, yChange = -1)

  val SouthWest = new Val(xChange = -1, yChange = -1)

  val West = new Val(xChange = -1, yChange = 0)

  val NorthWest = new Val(xChange = -1, yChange = 1)

  def rotateClockwise(startingDirection: Direction): Direction =
    // Increments the enum ID in order to select the next direction
    // Uses a modulo to wrap the ID when it gets too high,
    // could be replaced by if/else if performance is a concern
    Direction(floorMod(startingDirection.id + 1, maxId))

  def rotateAntiClockwise(startingDirection: Direction): Direction =
    // Decrements the enum ID in order to select the next direction
    // Uses a modulo to wrap the ID when it gets too high,
    // could be replaced by if/else if performance is a concern
    Direction(floorMod(startingDirection.id - 1, maxId))

}
