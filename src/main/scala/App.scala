import scala.io.Source
import scala.collection.mutable.ListBuffer

case class Coordinate(row: Int, col: Int) {
  override def toString: String = {
    "(" + row + "," + col + ")"
  }

}

class Group {

  var coords: ListBuffer[Coordinate] = new ListBuffer[Coordinate]()
  var doesOverlap: Boolean = false

  def getCoordinatesString: String = {
    val buf = new StringBuffer("[")
    for (coord <- coords) {
      buf.append(coord)
    }
    buf.append("]")
    buf.toString
  }

  def calculateArea: Int = {
    (getBottomRight.row - getTopLeft.row + 1) * (getBottomRight.col - getTopLeft.col + 1)
  }

  override def toString: String = {
    "[Top Left: " + this.getTopLeft + " | Top Right: " + this.getBottomRight +"] | " + this.getCoordinatesString
  }

  def simpleString: String = {
    this.getTopLeft.toString + this.getBottomRight.toString
  }

  def addCoord(coord: Coordinate): Unit = {
    coords.append(coord)
  }

  def getTopLeft: Coordinate = {
    var minRow: Option[Int] = None
    var minCol: Option[Int] = None
    for (coord <- coords) {
      if (minRow.isEmpty || coord.row < minRow.get) {
        minRow = Some(coord.row)
      }
      if (minCol.isEmpty || coord.col < minCol.get) {
        minCol = Some(coord.col)
      }
    }
    Coordinate(minRow.getOrElse(0), minCol.getOrElse(0))
  }

  def getBottomRight: Coordinate = {
    var maxRow: Option[Int] = None
    var maxCol: Option[Int] = None
    for (coord <- coords) {
      if (maxRow.isEmpty || coord.row > maxRow.get) {
        maxRow = Some(coord.row)
      }
      if (maxCol.isEmpty || coord.col > maxCol.get) {
        maxCol = Some(coord.col)
      }
    }
    Coordinate(maxRow.getOrElse(0), maxCol.getOrElse(0))
  }

  def overlapsWith(group: Group): Boolean = {
    val myTopLeft: Coordinate = this.getTopLeft
    val myBottomRight: Coordinate = this.getBottomRight
    val otherTopLeft: Coordinate= group.getTopLeft
    val otherBottomRight: Coordinate = group.getBottomRight

    val myTopLeftWithinOtherRowRange = myTopLeft.row >= otherTopLeft.row && myTopLeft.row <= otherBottomRight.row
    val otherTopLeftWithinMyRowRange = otherTopLeft.row >= myTopLeft.row && otherTopLeft.row <= myBottomRight.row
    val myTopLeftWithinOtherColRange = myTopLeft.col >= otherTopLeft.col && myTopLeft.col <= otherBottomRight.col
    val otherTopLeftWithinMyColRange = otherTopLeft.col >= myTopLeft.col && otherTopLeft.col <= myBottomRight.col

    val rowsOverlap = myTopLeftWithinOtherRowRange || otherTopLeftWithinMyRowRange
    val colsOverlap = myTopLeftWithinOtherColRange || otherTopLeftWithinMyColRange

    rowsOverlap && colsOverlap
  }

  def isAdjacentToCoord(coord: Coordinate): Boolean = {
    for (c <- coords) {
      if (isAdjacent(coord, c)) {
        return true
      }
    }
    false
  }

  private def isAdjacent(coordOne: Coordinate, coordTwo: Coordinate): Boolean = {
    val onSameRow = coordOne.row == coordTwo.row
    val onSameCol = coordOne.col == coordTwo.col
    val oneRowAway = Math.abs(coordOne.row - coordTwo.row) == 1
    val oneColAway = Math.abs(coordOne.col - coordTwo.col) == 1
    (onSameRow && oneColAway) || (onSameCol && oneRowAway)
  }

}

object Group {
  def apply(coord: Coordinate): Group = {
    val group = new Group()
    group.addCoord(coord)
    group
  }

  def apply(coordSet: Set[Coordinate]): Group = {
    val group = new Group()
    for (coord <- coordSet) {
      group.addCoord(coord)
    }
    group
  }

  def mergeGroups(groupsToMerge: ListBuffer[Group]): Group = {
    var coordSet: Set[Coordinate] = Set()
    for (group <- groupsToMerge) {
      for (c <- group.coords) {
        coordSet = coordSet + c
      }
    }
    Group(coordSet)
  }
}

class Grid {

  var gridGroups: ListBuffer[Group] = new ListBuffer[Group]()

  def processLine(line: String, rowNumber: Int): Unit = {
    for (i <- 0 until line.length) {
      val c = line.charAt(i)
      if (c == '*') {
        val coord = Coordinate(rowNumber, i+1)
        val newGridGroups: ListBuffer[Group] = ListBuffer()
        val groupsToMerge: ListBuffer[Group] = ListBuffer()
        for (group <- gridGroups) {
          if (group.isAdjacentToCoord(coord)) {
            groupsToMerge.append(group)
          } else {
            newGridGroups.append(group)
          }
        }
        groupsToMerge.length match {
          case 0 =>
            newGridGroups.append(Group(coord))
          case 1 =>
            groupsToMerge.head.addCoord(coord)
            newGridGroups.append(groupsToMerge.head)
          case _ =>
            val mergedGroup: Group = Group.mergeGroups(groupsToMerge)
            mergedGroup.addCoord(coord)
            newGridGroups.append(mergedGroup)
        }
        gridGroups = newGridGroups
      }
    }
  }

  def getLargestMinimumNonOverlappingBoundingBoxes: List[Group] = {
    val nonOverlappingBoxes: ListBuffer[Group] = ListBuffer[Group]()
    var largestBoxArea = 0
    for (i <- gridGroups.indices) {
      var j = i + 1
      while (j < gridGroups.length) {
        if (gridGroups(i).overlapsWith(gridGroups(j))) {
          gridGroups(i).doesOverlap = true
          gridGroups(j).doesOverlap = true
        }
        j = j + 1
      }
      if (!gridGroups(i).doesOverlap) {
        val nonOverlappingGroupToCheck = gridGroups(i)
        if (nonOverlappingGroupToCheck.calculateArea > largestBoxArea) {
          nonOverlappingBoxes.clear()
          nonOverlappingBoxes.append(nonOverlappingGroupToCheck)
          largestBoxArea = nonOverlappingGroupToCheck.calculateArea
        } else if (nonOverlappingGroupToCheck.calculateArea == largestBoxArea) {
          nonOverlappingBoxes.append(nonOverlappingGroupToCheck)
        }
      }
    }
    nonOverlappingBoxes.toList
  }

}

object App {

  def main(args: Array[String]): Unit = {
    val grid = new Grid()
    val filename = "input1.txt"
    var rowCounter = 1

    for (line <- Source.fromFile(filename).getLines) {
      if (line.nonEmpty) {
        grid.processLine(line, rowCounter)
        rowCounter = rowCounter + 1
      }
    }

    for (group <- grid.getLargestMinimumNonOverlappingBoundingBoxes) {
      println(group.simpleString)
    }

  }

}