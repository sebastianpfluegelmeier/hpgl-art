
object Main extends App {

  import hpgl.HPGL
  import hpgl.floaty.Floaty
  import symbs.Symbs
  import java.io.PrintWriter

  
  val hpglString = HPGL.getHPGLString(Floaty.toHPGL(Floaty.scale(Symbs.fill(120, 120))(120.0)))
  println("write ...")
  new PrintWriter("test.plt") { write(hpglString); close }
  println("finished!\n\n")
}

