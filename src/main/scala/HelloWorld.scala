import parser.Parser

object HelloWorld {
  def main(args: Array[String]): Unit = {
    println(new Parser().parse("hey alala @of fl"))
  }
}
