# TrackFunction

TrackFunction is a library written using the Scala programming language. The library gives clients the ability to transform a use of a function so that the arguments and return values of applications of that function are tracked in a data structure that can be used later for reporting. The addition of tracking is intended to be transparent if the user program is already executed in a monad environment. If not, a mechanical syntax transformation is required. An example of this is given below.

The TrackFunction library intends to maintain equational reasoning properties at all times i.e. there are no in-place updates in this library. Subsequent benefits of this can be exploited with higher-level libraries.

The TrackFunction library achieves a use-case whereby the argument that a function is applied to may have a value that is dependent on previous applications. In the absence of this use-case (of application dependent on values previously computed), it is trivial to track function application and does not require significant library support. For example:

    val function =
      22 + (_:Int)
    val result =
      List(8, 9, 10) map (a => (a, function(a))

The `result` here keeps the argument values and the return values of application by `function`. This is achieved by simply sequencing function application across argument values.

However, suppose the use-case of using as an argument, the result of application to a previous result:

    val function =
      22 + (_:Int)
    val result =
      List(8, 9, 10 + theResultOfApplicationToPrevious) … go no further!

This use-case quickly becomes messy without library support. Solving this use-case is the inspiration for TrackFunction.

### Transforming Scala Programs to use a for-comprehension

Scala's for-comprehensions are used for performing monadic computation. They are similar to Haskell's do-notation[^1].
In general, any Scala program can be transformed to use a for-comprehension.

Take a Scala code-block such as:

    {
      val a = function1(x)
      val b = function2(a, y)
      val c = function3(y)
      val d = function4(b, c)
      d
    }
    

In general, this can be transformed to:

    {
      for {
        a <- function1(x)
        b <- function2(a, y)
        c <- function3(y)
        d <- function4(b, c)
      } yield d
    }

Indeed, to obtain the equivalent program, we compute the for-comprehension inside the *identity monad*.

    case class Identity[A](value: A) {
      def map[B](f: A => B): Id[B] =
        Id(f(value))
      def flatMap[B](f: A => Id[B]): Id[B] =
        f(value)
    }

With some trivial shuffling in-and-out of the `Id` container, the two previous programs can be made equivalent.

This general technique applies to function tracking. A library user would perform the same program transformation (if not already), however, would not run in `Identity`. Instead, the for-comprehension runs in a different monad; the `Track` monad, which is supplied by this library.

### Documentation

Documentation for this library is provided by example. These can be found in the [`examples`](http://github.com/NICTA/trackfunction/tree/master/examples) source directory.




[^1]: http://en.wikibooks.org/wiki/Haskell/do_Notation

