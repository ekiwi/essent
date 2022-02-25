import org.scalatest.freespec.AnyFreeSpec

// sbt "testOnly *GCDTest"
class GCDTest extends AnyFreeSpec{
  // sbt "testOnly *GCDTest -- -z testZero"
  "testZero" in {
    println("hello world")
  }
}
