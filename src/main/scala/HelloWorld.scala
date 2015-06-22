import parser.MyParser

object HelloWorld {
  def main(args: Array[String]): Unit = {
    println(new MyParser().parse("hey alala @of fl"))
  }
}
