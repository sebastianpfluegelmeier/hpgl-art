package hpgl

abstract sealed class HPGLPath ()

case class Line(linePoints: Array[Coordinates]) extends HPGLPath

case class Coordinates(x: Int, y: Int)

case class Combined(paths: Array[HPGLPath]) extends HPGLPath

case class Empty() extends HPGLPath

object HPGL {

  def line(linePoints: (Int, Int)*): Line = Line(linePoints.map(xy => Coordinates(xy._1, xy._2)).toArray)

  def line(linePoints: Array[(Int, Int)]): Line = line(linePoints:_*)

  def empty(): Empty = Empty()

  def joinLines(lines: Line*): Line = {
    Line(
      lines
        .map(line => line.linePoints)
        .flatMap(linePoints => linePoints)
        .toArray
    )
  }

  def joinLines(lines: Array[Line]): Line = joinLines(lines:_*)

  def combinePaths(paths: HPGLPath*): HPGLPath = Combined(paths.toArray)

  def combinePaths(paths: Array[HPGLPath]): HPGLPath = combinePaths(paths:_*)

  def getHPGLString(hpglPath: HPGLPath): String = {
    hpglPath match {
      case Line(linePoints) => {
        def firstX = linePoints(0).x
        def firstY = linePoints(0).y
        def firstPoint = s"$firstX,$firstY"
        def goToStartPosition = s"PU$firstPoint;\n"
        def lineCoordinates = linePoints
          .map(coords => {
            def x = coords.x
            def y = coords.y
            s"$x,$y"
          })
          .reduce((a, b) => a + "," + b)
        def draw = s"PD$lineCoordinates;\n"
        goToStartPosition + draw
      }
      case Combined(paths) => {
        paths
          .map(path => getHPGLString(path))
          .reduce((a, b) => s"$a$b")
      }
      case Empty() => ""
    }
  }
}