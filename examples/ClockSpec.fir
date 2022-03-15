circuit Stop0 :
  module DUT :
    input clock : Clock
    input reset : UInt<1>
    output out2 : UInt<8>

    reg reg2 : UInt<8>, clock with : (reset => (reset, UInt<8>("h07")))
    reg2 <= add(reg2, UInt<8>("h01"))
    out2 <= reg2

  module Stop0 :
    input clock : Clock
    input reset : UInt<1>
    output out1 : UInt<8>
    output out2 : UInt<8>

    reg reg1 : UInt<8>, clock with : (reset => (reset, UInt<8>("h03")))

    inst dut of DUT

    dut.clock <= clock
    dut.reset <= reset

    out1  <= reg1
    out2  <= dut.out2

    reg1 <= add(reg1, UInt<8>("h01"))
    when gt(reg1, UInt<8>("h08")) :
      stop(clock, UInt(1), 0) ; Done!
