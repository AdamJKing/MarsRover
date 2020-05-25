package mars.rover.challenge

import org.scalacheck.Gen
import Direction._
import org.scalatest.matchers.should.Matchers._
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks._

class BasicMovementSpec extends propspec.AnyPropSpec {

  private val DirectionGen = Gen.oneOf(Direction.values)

  private val GridGen: Gen[MarsGrid] = {
    val positionGen = for {
      x <- Gen.posNum[Int]
      y <- Gen.posNum[Int]
    } yield (x, y)

    for {
      width <- Gen.posNum[Int].suchThat(_ > 1)
      height <- Gen.posNum[Int].suchThat(_ > 1)
      roverDirection <- DirectionGen
      position <- positionGen
    } yield
      MarsGrid(width, height, roverDirection, position)
  }

  property("rotating clockwise and anti-clockwise has no effect") {
    forAll(DirectionGen) { direction =>
      val result = Direction.rotateClockwise(Direction.rotateAntiClockwise(direction))
      result shouldBe direction
    }
  }

  property("rotating clockwise continuously ends up facing the same direction") {
    forAll(DirectionGen) { direction =>
      val result = Seq.iterate(direction, Direction.values.size)(Direction.rotateClockwise).head
      result shouldBe direction
    }
  }

  property("rotating anti-clockwise continuously ends up facing the same direction") {
    forAll(DirectionGen) { direction =>
      val result = Seq.iterate(direction, Direction.values.size)(Direction.rotateAntiClockwise).head
      result shouldBe direction
    }
  }

  property("rotating clockwise changes the rover's direction") {
    forAll(DirectionGen) { startingDirection =>
      Direction.rotateClockwise(startingDirection) should not be startingDirection
    }
  }

  property("rotating anti-clockwise moves the rover's direction") {
    forAll(DirectionGen) { startingDirection =>
      Direction.rotateAntiClockwise(startingDirection) should not be startingDirection
    }
  }

  property("moving the rover inside the grid") {
    forAll(GridGen) { grid =>
      val (startX, startY) = grid.roverPosition
      val (endX, endY) = MarsGrid.moveRover(grid).roverPosition

      whenever(startX > 0 && startX < grid.width) {
        endX shouldBe (startX + grid.roverDirection.xChange)
      }

      whenever (startY > 0 && startY < grid.height) {
        endY shouldBe (startY + grid.roverDirection.yChange)
      }
    }
  }

  property("moving the rover towards the edge of the grid causes it to appear on the other side") {
    val gridWithRoverOnEdgeGen = for {
      grid <- GridGen
      posX <- Gen.oneOf(0, grid.width)
      posY <- Gen.oneOf(0, grid.height)
    } yield grid.copy(roverPosition = (posX, posY))

    forAll(gridWithRoverOnEdgeGen) { grid =>
      val (startX, startY) = grid.roverPosition
      val (endX, endY) = MarsGrid.moveRover(grid).roverPosition

      if (startX == 0 && grid.roverDirection.xChange == -1) {
        endX shouldBe grid.width
      }

      if (startX == grid.width && grid.roverDirection.xChange == 1) {
        endX shouldBe 0
      }

      if (startY == 0 && grid.roverDirection.yChange == -1) {
        endY shouldBe grid.height
      }

      if (startY == grid.height && grid.roverDirection.yChange == 1) {
        endY shouldBe 0
      }
    }
  }
}
