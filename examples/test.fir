circuit Test:
  module Test:
    input clock : Clock
    input reset : UInt<1>
    output out : UInt<4>

    reg register : UInt<4>, clock with :
      reset => (reset, UInt<4>("h0")) @[main.scala 7:25]
    node _register_T = addw(register, UInt<1>("h1")) @[main.scala 10:24]
    node _register_T_1 = tail(_register_T, 1) @[main.scala 10:24]
    register <= _register_T_1 @[main.scala 10:12]
    out <= register @[main.scala 11:7]