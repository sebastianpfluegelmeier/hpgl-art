package symbs

import hpgl.floaty.Floaty
import hpgl.floaty.FloatyPath
import hpgl.floaty.Coordinates
import scala.util.Random

object Symbs {

    def ring(x: Int, y: Int, inner: Double, outer: Double, rotation: Double, wavy: Double, random: Random): FloatyPath = {
        val paths = 
            (0 until x).map(xi =>
                (0 until y).map(yi => {
                    val inside = if (random.nextInt(x) > xi) 
                            insideSymbs(random.nextInt(insideSymbs.length)) 
                        else 
                            Floaty.empty

                    val outside = if (random.nextInt(y) > yi) 
                            outsideSymbs(random.nextInt(outsideSymbs.length)) 
                        else 
                            Floaty.empty

                    val combined = Floaty.combinePaths(inside, outside)
                    val translatedToCenter = Floaty.translate(combined)(-0.5, -0.5)
                    val scaled = Floaty.scale(translatedToCenter)(0.7)
                    val turned = Floaty.rotate(scaled)(0.5 * Math.PI * random.nextInt(4).toDouble)
                    val translatedBack = Floaty.translate(turned)(0.5, 0.5)
                    val offset = Floaty.translate(translatedBack)(xi.toDouble, yi.toDouble)
                    offset
                })
            )
        val combined = Floaty.combinePaths(paths.flatMap(identity).toArray)
        val distorted = Floaty.map((x, y) => 
            (x + 0.39 * wavy * Math.sin(x * 0.2442 + y * 0.2564) + 4.2 * wavy * Math.sin(x * 0.05324 + y * 0.02522),
             y + 0.41 * wavy * Math.sin(y * 0.2342 + x * 0.2433) + 4.3 * wavy * Math.sin(y * 0.03953 + x * 0.08953))
        )(combined)
        //combined
        val translatedDown = Floaty.translate(distorted)(x.toDouble * -0.5, y.toDouble * -0.5)
        val filtered = Floaty.mapLine(line => {
            if (line.linePoints.exists{case Coordinates(x_, y_) => Math.sqrt(x_ * x_ + y_ * y_) < x.toDouble * inner || Math.sqrt(x_ * x_ + y_ * y_) > x.toDouble * outer}) {
                Floaty.empty()
            } else {
                line
            }
        })(translatedDown)
        val rotated = Floaty.rotate(filtered)(rotation * Math.PI)
        val translatedBack = Floaty.translate(rotated)(x.toDouble * 0.5, y.toDouble * 0.5)
        translatedBack

    }

    def fill(x: Int, y: Int): FloatyPath = {
        var random = new Random(1)
        val rim = ring(x, y, 0.35, 2.0, 0.0, 1.3, random)
        val inner = Floaty.scale(ring((x.toDouble * 1.5).toInt, (y.toDouble * 1.5).toInt, 0.27, 0.32, 0.35, 0.2, random))(0.66)

        val combined = Floaty.combinePaths(rim, inner)
        val filteredRim = Floaty.mapLine(line => {
            if (line.linePoints.exists{case Coordinates(x_, y_) => x_ > x.toDouble * 0.95 || x_ < x.toDouble * 0.05 || y_ > y.toDouble * 0.95 || y_ < y.toDouble * 0.05}) {
                Floaty.empty()
            } else {
                line
            }
        })(combined)
        val rimline = Floaty.line((0.0, 0.0), (x.toDouble, 0.0), (x.toDouble, y.toDouble), (0.0, y.toDouble), (0.0, 0.0))
        Floaty.combinePaths(filteredRim, rimline)
    }

    def outsideSymbs = 
        Array(
            // Floaty.line((0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0), (0.0, 0.0)),
            Floaty.combinePaths(
                Floaty.line((0.25, 0.0), (0.0, 0.0), (0.0, 0.25)),
                Floaty.line((0.75, 1.0), (1.0, 1.0), (1.0, 0.75)),
            ),
            Floaty.combinePaths(
                Floaty.line((0.25, 0.0), (0.0, 0.0), (0.0, 0.25), (0.25, 0.0)),
                Floaty.line((0.75, 1.0), (1.0, 1.0), (1.0, 0.75), (0.75, 1.0)),
            ),
    
            Floaty.combinePaths(
                Floaty.line((0.25, 0.0), (0.75, 0.0)),
                Floaty.line((0.25, 1.0), (0.75, 1.0)),
                Floaty.line((1.0, 0.25), (1.0, 0.75)),
                Floaty.line((0.0, 0.25), (0.0, 0.75)),
            ),
            Floaty.combinePaths(
                Floaty.line((0.25, 0.0), (0.75, 0.0)),
                Floaty.line((0.25, 1.0), (0.75, 1.0)),
            ),
            Floaty.combinePaths(
                Floaty.line((1.0, 0.25), (1.0, 0.75)),
                Floaty.line((1.0, 0.5), (0.9, 0.5)),
                Floaty.line((0.0, 0.25), (0.0, 0.75)),
                Floaty.line((0.0, 0.5), (0.1, 0.5)),
            ),
            Floaty.combinePaths(
                Floaty.line((0.2, 0.0), (0.0, 0.2)),
                Floaty.line((0.8, 0.0), (0.0, 0.8)),
                Floaty.line((0.2, 1.0), (1.0, 0.2)),
                Floaty.line((0.8, 1.0), (1.0, 0.8)),
            ),
            Floaty.combinePaths(
                Floaty.line((0.2, 0.0), (0.0, 0.2)),
                Floaty.line((0.8, 1.0), (1.0, 0.8)),
                Floaty.line((0.8, 0.0), (1.0, 0.2)),
                Floaty.line((0.0, 0.8), (0.2, 1.0)),
            ),
            Floaty.combinePaths(
                Floaty.line((0.0, 0.0), (0.0, 0.2), (0.2, 0.2)),
                Floaty.line((0.0, 1.0), (0.0, 0.8), (0.2, 0.8)),
            ),
            Floaty.combinePaths(
                Floaty.line((0.0, 0.0), (0.0, 0.2), (0.2, 0.2)),
                Floaty.line((0.0, 1.0), (0.0, 0.8), (0.2, 0.8)),
                Floaty.line((1.0, 0.0), (1.0, 0.2), (0.8, 0.2)),
                Floaty.line((1.0, 1.0), (1.0, 0.8), (0.8, 0.8)),
            ),
            quadruple(
                Floaty.line((0.0, 0.0), (0.0, 0.2), (0.2, 0.2), (0.2, 0.0), (0.0, 0.0))
            ),
            quadruple(
                Floaty.combinePaths(
                    Floaty.line((0.0, 0.2), (0.05, 0.2)),
                    Floaty.line((0.0, 0.8), (0.05, 0.8)),
                )
            ),
            quadruple(
                Floaty.line((0.0, 0.5), (0.1, 0.5)),
            ),
            quadruple(
                Floaty.line((0.0, 0.0), (0.1, 0.1)),
            ),
            quadruple(
                Floaty.line((0.2, 0.0), (0.2, 0.2), (0.0, 0.2), (0.2, 0.0)),
            ),
            quadruple(
                Floaty.line((0.4, 0.0), (0.6, 0.0)),
            ),
            Floaty.line((0.0, 0.0), (0.0, 1.0), (1.0, 1.0)),
            Floaty.combinePaths(
                Floaty.line((0.0, 0.0), (0.0, 1.0)),
                Floaty.line((1.0, 0.0), (1.0, 1.0)),
            ),
            Floaty.combinePaths(
                Floaty.line((0.0, 0.2), (0.2, 0.4)),
                Floaty.line((0.0, 0.8), (0.2, 0.6)),
                Floaty.line((1.0, 0.2), (0.8, 0.4)),
                Floaty.line((1.0, 0.8), (0.8, 0.6)),
            ),
            quadruple(
                Floaty.combinePaths(
                    Floaty.line((0.0, 0.0), (0.2, 0.2)),
                    Floaty.line((0.0, 0.2), (0.0, 0.0), (0.2, 0.0)),
                ),
            ),
            Floaty.combinePaths(
                Floaty.line((0.2, 0.0), (0.0, 0.2), (0.0, 0.8), (0.2, 1.0)),
                Floaty.line((0.8, 0.0), (1.0, 0.2), (1.0, 0.8), (0.8, 1.0)),
            ),
        )

    def quadruple(path: FloatyPath): FloatyPath = {
        val centered = Floaty.translate(path)(-0.5, -0.5)
        val turned1 = Floaty.rotate(centered)(Math.PI * 0.5)
        val turned2 = Floaty.rotate(centered)(Math.PI * 1.0)
        val turned3 = Floaty.rotate(centered)(Math.PI * 1.5)
        val joined = Floaty.combinePaths(centered, turned1, turned2, turned3)
        val uncentered = Floaty.translate(joined)(0.5, 0.5)
        uncentered
    }

    def insideSymbs = 
        Array(
            Floaty.combinePaths(
                Floaty.line((0.125, 0.5), (0.5, 0.125)),
                Floaty.line((1.0 - 0.125, 0.125), (0.125, 1.0 - 0.125)),
                Floaty.line((1 - 0.125, 0.5), (0.5, 1 - 0.125))
            ),
            Floaty.combinePaths(
                Floaty.line((0.5, 0.25), (0.25, 0.25), (0.25, 0.5)),
                Floaty.line((0.5, 0.75), (0.75, 0.75), (0.75, 0.5)),
            ),
            Floaty.line((0.25, 0.25), (0.25, 0.75), (0.75, 0.75), (0.75, 0.25)),
            Floaty.line((0.25, 0.25), (0.25, 0.75), (0.75, 0.75), (0.75, 0.25), (0.25, 0.25)),
            Floaty.combinePaths(
                Floaty.line((0.25, 0.25), (0.25, 0.75)),
                Floaty.line((0.75, 0.25), (0.75, 0.75))
            ),
            Floaty.line((0.5, 0.0), (1.0, 0.5), (0.5, 1.0), (0.0, 0.5), (0.5, 0.0)),
            Floaty.combinePaths(
                Floaty.line((0.5, 0.4), (0.5, 0.6)),
                Floaty.line((0.4, 0.5), (0.6, 0.5))
            ),
            Floaty.line((0.5, 0.4), (0.5, 0.6)),
            Floaty.line((0.4, 0.5), (0.6, 0.5)),
        )
}