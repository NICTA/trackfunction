package com.nicta
package trackfunction

object Simple {
  import TrackFunction._

  // The function to be tracked.
  val function =
    22 + (_:Int)

  // The program as it looks without tracking.
  object Untracked {
    // Apply the function to values.
    def run(f: Int => Int) = {
      val x = f(8)
      val y = f(9)
      // Apply the function, depending on the result of a previous application.
      val z = f(10 + y)
      (x, y, z)
    }

    // Run the function.
    val result =
      run(function)
  }

  // The program with tracking of function arguments and return values.
  object Tracked {
    // Apply the function to values.
    val run =
      for {
        // The return type of the function is annotated.
        x <- track[Int](8)
        y <- track(9)
       // Apply the function, depending on the result of a previous application.
        z <- track(10 + y)
      } yield (x, y, z)

    // Run the function.
    val result =
      run(function)
  }

  def main(args: Array[String]) {
    import scalaz._, Scalaz._

    // Print the result of the untracked program.
    //     (30,31,63)
    Untracked.result.println
    // Print the result of the tracked program as well as tracked values.
    //     [ (30,31,63) ] <~ [( 41 ) => ( 63 ),( 9 ) => ( 31 ),( 8 ) => ( 30 )]
    Tracked.result.println
  }

}
