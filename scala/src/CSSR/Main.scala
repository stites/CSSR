package CSSR

import CSSR.parse.Parse

object Main {
  def main(
            alphabet:String,
            dataSource:String,
            maxLength:Integer,
            sigLevel:Int = 0.5,
            useMultiline:Boolean = false, // unused
            useChiSquared:Boolean = false // unused
          ): Unit = {
    val parsetree:Parse = Parse(alphabet, dataSource)
  }
}

class Main {
}


