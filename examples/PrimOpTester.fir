circuit PrimOpTester :
  module PrimOpTester :
    input clock : Clock
    input reset : UInt<1>
    output io : { flip primOp : UInt<6>, flip UArg1 : UInt<32>, flip UArg2 : UInt<32>, flip SArg1 : SInt<32>, flip SArg2 : SInt<32>, UOut : UInt<32>, SOut : SInt<32>}

    node _T = eq(io.primOp, UInt<1>("h0")) @[main.scala 18:19]
    when _T : @[main.scala 18:28]
      node _io_UOut_T = add(io.UArg1, io.UArg2) @[main.scala 19:25]
      node _io_UOut_T_1 = tail(_io_UOut_T, 1) @[main.scala 19:25]
      io.UOut <= _io_UOut_T_1 @[main.scala 19:13]
      node _io_SOut_T = add(io.SArg1, io.SArg2) @[main.scala 20:25]
      node _io_SOut_T_1 = tail(_io_SOut_T, 1) @[main.scala 20:25]
      node _io_SOut_T_2 = asSInt(_io_SOut_T_1) @[main.scala 20:25]
      io.SOut <= _io_SOut_T_2 @[main.scala 20:13]
    else :
      node _io_UOut_T_2 = add(io.UArg1, io.UArg2) @[main.scala 22:25]
      node _io_UOut_T_3 = tail(_io_UOut_T_2, 1) @[main.scala 22:25]
      io.UOut <= _io_UOut_T_3 @[main.scala 22:13]
      node _io_SOut_T_3 = add(io.SArg1, io.SArg2) @[main.scala 23:25]
      node _io_SOut_T_4 = tail(_io_SOut_T_3, 1) @[main.scala 23:25]
      node _io_SOut_T_5 = asSInt(_io_SOut_T_4) @[main.scala 23:25]
      io.SOut <= _io_SOut_T_5 @[main.scala 23:13]