circuit PrimOpTester :
  module PrimOpTester :
    input clock : Clock
    input reset : UInt<1>
    input io_UArg1 : UInt<40>
    input io_UArg2 : UInt<40>
    input io_SArg1 : SInt<40>
    input io_SArg2 : SInt<40>
    output io_UOut : UInt<80>
    output io_SOut : UInt<80>

    io_UOut <= cat(io_UArg1, io_UArg2)
    io_SOut <= cat(io_SArg1, io_SArg2)
