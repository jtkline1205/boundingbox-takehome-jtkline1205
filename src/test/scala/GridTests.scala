import org.junit.Assert._
import org.junit.Test

class GridTests {

  @Test
  def testGrid1(): Unit = {
    val grid = new Grid()
    grid.processLine("----", 1)
    grid.processLine("-**-", 2)
    grid.processLine("-**-", 3)
    grid.processLine("----", 4)
    assertEquals(1, grid.gridGroups.length)
    for (group <- grid.gridGroups) {
      println(group.toString)
    }
  }

  @Test
  def testGrid2(): Unit = {
    val grid = new Grid()
    grid.processLine("**-------***", 1)
    grid.processLine("-*--**--***-", 2)
    grid.processLine("-----***--**", 3)
    grid.processLine("-------***--", 4)
    for (group <- grid.gridGroups) {
      println(group.toString)
    }
    assertEquals(3, grid.gridGroups.length)
    assertFalse(grid.gridGroups.head.overlapsWith(grid.gridGroups(1)))
    assertFalse(grid.gridGroups.head.overlapsWith(grid.gridGroups(2)))
    assertTrue(grid.gridGroups(2).overlapsWith(grid.gridGroups(1)))
    assertTrue(grid.gridGroups(1).overlapsWith(grid.gridGroups(2)))

    val groups = grid.getLargestMinimumNonOverlappingBoundingBoxes
    assertEquals(1, groups.length)
    assertEquals(1, groups.head.getTopLeft.row)
    assertEquals(1, groups.head.getTopLeft.col)
    assertEquals(2, groups.head.getBottomRight.row)
    assertEquals(2, groups.head.getBottomRight.row)

  }

  @Test
  def testGrid3(): Unit = {
    val grid = new Grid()
    grid.processLine("********-----------------********", 1)
    grid.processLine("****-------------------*******---", 2)
    grid.processLine("******---------*****--------*****", 3)
    grid.processLine("**----**------***----******-----*", 4)
    grid.processLine("------**--------***----****----**", 5)
    grid.processLine("****----****----***-------*----**", 6)
    grid.processLine("---***----*----*****----***---***", 7)
    grid.processLine("*******---------*******---------*", 8)
    grid.processLine("---------------------------------", 9)
    grid.processLine("------------*******--------------", 10)
    grid.processLine("---------------*--*--------------", 11)
    grid.processLine("------------****--*--------------", 12)

    for (group <- grid.gridGroups) {
      println(group.toString)
    }
    assertEquals(8, grid.gridGroups.length)
    assertEquals(32, grid.gridGroups.head.calculateArea)
    assertEquals(4, grid.gridGroups(1).calculateArea)
    assertEquals(8, grid.gridGroups(2).calculateArea)
    assertEquals(24, grid.gridGroups(3).calculateArea)
    assertEquals(21, grid.gridGroups(4).calculateArea)
    assertEquals(54, grid.gridGroups(5).calculateArea)
    assertEquals(80, grid.gridGroups(6).calculateArea)
    assertEquals(21, grid.gridGroups(7).calculateArea)


    println("Largest Non Overlapping Groups:")
    val largestNonOverlappingGroups = grid.getLargestMinimumNonOverlappingBoundingBoxes
    for (group <- largestNonOverlappingGroups) {
      println(group.toString)
    }

    assertEquals(2, largestNonOverlappingGroups.length)
    assertEquals(6, largestNonOverlappingGroups.head.getTopLeft.row)
    assertEquals(1, largestNonOverlappingGroups.head.getTopLeft.col)
    assertEquals(8, largestNonOverlappingGroups.head.getBottomRight.row)
    assertEquals(7, largestNonOverlappingGroups.head.getBottomRight.col)
    assertEquals(10, largestNonOverlappingGroups(1).getTopLeft.row)
    assertEquals(13, largestNonOverlappingGroups(1).getTopLeft.col)
    assertEquals(12, largestNonOverlappingGroups(1).getBottomRight.row)
    assertEquals(19, largestNonOverlappingGroups(1).getBottomRight.col)
  }

}
