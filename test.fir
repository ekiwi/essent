circuit test:
  module child:
     input clock: Clock
     input reset: UInt<1>
     input in: UInt<8>
     output out: UInt<8>

     reg r : UInt<8>, clock with :
       reset => (reset, UInt("h0"))
     r <= in
     out <= r

  module test:
     input clock: Clock
     input reset: UInt<1>
     input in: UInt<8>
     output out: UInt<8>

     reg r : UInt<8>, clock with :
       reset => (reset, UInt("h0"))

     inst c of child
     c.clock <= clock
     c.reset <= reset
     c.in <= in
     r <= not(c.out)
     out <= r


     
