#ifndef GCD_H_
#define GCD_H_

#include <array>
#include <cstdint>
#include <cstdlib>
#include <uint.h>
#include <sint.h>
#define UNLIKELY(condition) __builtin_expect(static_cast<bool>(condition), 0)

typedef struct gcd {
  UInt<1> busy;
  UInt<1> done;
  UInt<32> x;
  UInt<32> y;
  UInt<1> clock;
  UInt<1> reset;
  UInt<1> io_in_ready;
  UInt<1> io_in_valid;
  UInt<32> io_in_bits_a;
  UInt<32> io_in_bits_b;
  UInt<1> io_out_ready;
  UInt<1> io_out_valid;
  UInt<32> io_out_bits;

  gcd() {
    busy.rand_init();
    done.rand_init();
    x.rand_init();
    y.rand_init();
    reset.rand_init();
    io_in_ready.rand_init();
    io_in_valid.rand_init();
    io_in_bits_a.rand_init();
    io_in_bits_b.rand_init();
    io_out_ready.rand_init();
    io_out_valid.rand_init();
    io_out_bits.rand_init();
  }

  void eval(bool update_registers, bool verbose, bool done_reset) {
    UInt<1> T_42 = y == UInt<32>(0x0);
    UInt<1> T_43 = busy & T_42;
    UInt<1> _GEN_0 = T_43 | done;
    UInt<1> T_45 = done & io_out_ready;
    UInt<1> _GEN_1 = T_45 ? UInt<1>(0x0) : busy;
    io_in_ready = ~busy;
    UInt<1> start = io_in_valid & io_in_ready;
    UInt<1> _GEN_2 = start | _GEN_1;
    UInt<1> _GEN_3 = start ? UInt<1>(0x0) : _GEN_0;
    UInt<1> T_50 = x > y;
    UInt<33> T_51 = x - y;
    UInt<32> T_52 = T_51.tail<1>();
    UInt<32> _GEN_4 = T_50 ? T_52 : x;
    UInt<1> T_54 = ~T_50;
    UInt<33> T_55 = y - x;
    UInt<32> T_56 = T_55.tail<1>();
    UInt<32> _GEN_5 = T_54 ? T_56 : y;
    UInt<32> _GEN_6 = start ? io_in_bits_a : _GEN_4;
    UInt<32> _GEN_7 = start ? io_in_bits_b : _GEN_5;
    io_out_valid = done;
    io_out_bits = x;
    if (update_registers) busy = reset ? UInt<1>(0x0) : _GEN_2;
    if (update_registers) done = reset ? UInt<1>(0x0) : _GEN_3;
    if (update_registers) x = reset ? UInt<32>(0x0) : _GEN_6;
    if (update_registers) y = reset ? UInt<32>(0x0) : _GEN_7;
  }
} gcd;

#endif  // GCD_H_
