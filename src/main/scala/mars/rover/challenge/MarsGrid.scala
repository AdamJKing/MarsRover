package mars.rover.challenge

import mars.rover.challenge.Direction._

import scala.math.floorMod

/**
 * A simple representation of the Mars surface and the Mars rover itself.
 *
 * @param width the horizontal space inside the grid
 * @param height the vertical space inside the grid
 * @param roverDirection the direction the rover is currently facing
 * @param roverPosition the (x, y) position of the rover inside the grid
 */
final case class MarsGrid(width: Int, height: Int, roverDirection: Direction, roverPosition: (Int, Int))

/**
 * A series of functions representing the commands that can be issued to the Mars Rover.
 *
 */
object MarsGrid {
  /**
   * Update a given grid to move the rover forward by one in it's current direction
   *
   * @param grid the target grid
   * @return the updated grid after the command is applied
   */
  def moveRover(grid: MarsGrid): MarsGrid = {
    val (oldX, oldY) = grid.roverPosition

    // Using modulo to keep the location inside the range of the grid's width & height
    val newX = floorMod(oldX + grid.roverDirection.xChange, grid.width + 1)
    val newY = floorMod(oldY + grid.roverDirection.yChange, grid.height + 1)

    grid.copy(roverPosition = (newX, newY))
  }

  /**
   * Update a given grid to rotate the rover in-place clockwise by one
   *
   * @param grid the target grid
   * @return the updated grid after the command is applied
   */
  def rotateRoverClockwise(grid: MarsGrid): MarsGrid =
    grid.copy(roverDirection = Direction.rotateClockwise(grid.roverDirection))

  /**
   * Update a given grid to rotate the rover in-place anti-clockwise by one
   *
   * @param grid the target grid
   * @return the updated grid after the command is applied
   */
  def rotateRoverAntiClockwise(grid: MarsGrid): MarsGrid =
    grid.copy(roverDirection = Direction.rotateAntiClockwise(grid.roverDirection))
}
