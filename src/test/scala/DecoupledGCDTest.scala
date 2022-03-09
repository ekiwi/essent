import essent.Driver
import org.scalatest.freespec.AnyFreeSpec
import firrtl.options.TargetDirAnnotation

class DecoupledGcdTest extends AnyFreeSpec{

    // val sim = args.headOption match {
    // case None => VerilatorBackendAnnotation.getSimulator
    //     case Some(name) => name.toLowerCase match {
    //         case "verilator" => VerilatorBackendAnnotation.getSimulator
    //         case "treadle" => TreadleBackendAnnotation.getSimulator
    //         case "iverilog" => IcarusBackendAnnotation.getSimulator
    //         case "vcd" => VcsBackendAnnotation.getSimulator
    //         case other => throw new RuntimeException(s"Unknown simulator option: $other")
    //     }
    // }
    // assert(sim.isAvailable)
    // println(s"Using ${sim.name}")

    

    val targetDir = TargetDirAnnotation(System.getProperty("user.dir") + "/examples")

    val (highFirrtl, _) = Compiler.elaborate(() => new DecoupledGcd(bitWidth = 60), Seq(targetDir))
    val lowFirrtl = Compiler.toLowFirrtl(highFirrtl)
    println(s"Compiled ${lowFirrtl.circuit.main}")
    // val dut = sim.createContext(lowFirrtl)

    // val repetitions = 6
    // val numMax = 200
    // val testValues = for {x <- 2 to numMax; y <- 2 to numMax} yield (BigInt(x), BigInt(y), computeGcd(x, y))
    // val t = new Timer
    // var cycles = 0L
    // (0 until repetitions).foreach { _ =>
    // t("sim-gcd-" + sim.name) {
    //     cycles += runTest(dut, testValues)
    // }
    // }

    // println(t.report())
    // println(s"$cycles cycles")

    Driver.main(Array("-O0", "-java", System.getProperty("user.dir") + "/examples/GCD.fir"))
    val sim : SimulatorWrapper = new SimulatorWrapper(JavaRuntimeCompiler.compile(System.getProperty("user.dir") + "/examples/GCD.java"))
}