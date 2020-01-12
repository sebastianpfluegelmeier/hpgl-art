package hpgl.floaty

import hpgl.HPGL
import hpgl.HPGLPath

abstract sealed class FloatyPath ()

case class Line(linePoints: Array[Coordinates]) extends FloatyPath

case class Coordinates(x: Double, y: Double)

case class Combined(paths: Array[FloatyPath]) extends FloatyPath

case class Empty() extends FloatyPath

object Floaty {

    def translate(floatyPath: FloatyPath)(dx: Double, dy: Double) = 
        map ((x, y) => (x + dx, y + dy)) (floatyPath)

    def scale(floatyPath: FloatyPath)(factor: Double) = 
        map ((x, y) => (x*factor, y*factor)) (floatyPath)
    
    def rotate(floatyPath: FloatyPath)(angle: Double) = 
        map ((x, y) => (x * Math.cos(angle) - y * Math.sin(angle) ,x * Math.sin(angle) + y * Math.cos(angle))) (floatyPath)

    def map(callback: (Double, Double) => (Double, Double))(floatyPath: FloatyPath): FloatyPath = {
        floatyPath match {
            case Line(linePoints) => {
                line(
                    linePoints.map(
                        linePoint => callback(linePoint.x, linePoint.y)
                    )
                )
            }
            case Combined(paths) => {
                Combined(paths.map(path => map(callback)(path)))
            }
            case Empty() => Empty()
        }
    }

    def mapLine(callback: Line => FloatyPath)(floatyPath: FloatyPath): FloatyPath = {
        floatyPath match {
            case line: Line => {
                callback(line)
            }
            case Combined(paths) => {
                Combined(paths.map(path => mapLine(callback)(path)))
            }
            case Empty() => Empty()
        }
    }

    def line(linePoints: (Double, Double)*): Line = Line(linePoints.map(xy => Coordinates(xy._1, xy._2)).toArray)

    def line(linePoints: Array[(Double, Double)]): Line = line(linePoints:_*)

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

    def combinePaths(paths: FloatyPath*): FloatyPath = Combined(paths.toArray)

    def combinePaths(paths: Array[FloatyPath]): FloatyPath = combinePaths(paths:_*)

    def toHPGL(floatyPath: FloatyPath): HPGLPath = {
        floatyPath match {
            case Line(linePoints) => {
                HPGL.line(
                    linePoints.map(
                        linePoint => (linePoint.x.toInt, linePoint.y.toInt)
                    )
                )
            }
            case Combined(paths) => {
                HPGL.combinePaths(paths.map(path => toHPGL(path)).toArray)
            }

            case Empty() => HPGL.empty()
        }
    }

}