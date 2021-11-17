#ifndef REALGCD2_H_
#define REALGCD2_H_

#include <array>
#include <cstdint>
#include <cstdlib>
#include <uint.h>
#include <sint.h>
#define UNLIKELY(condition) __builtin_expect(static_cast<bool>(condition), 0)

typedef struct RealGCD2 {
  UInt<16> x;
  UInt<16> y;
  UInt<1> p;
  UInt<1> clock;
  UInt<1> reset;
  UInt<1> io_in_ready;
  UInt<1> io_in_valid;
  UInt<16> io_in_bits_a;
  UInt<16> io_in_bits_b;
  UInt<1> io_out_valid;
  UInt<16> io_out_bits;

  RealGCD2() {
    x.rand_init();
    y.rand_init();
    p.rand_init();
    reset.rand_init();
    io_in_ready.rand_init();
    io_in_valid.rand_init();
    io_in_bits_a.rand_init();
    io_in_bits_b.rand_init();
    io_out_valid.rand_init();
    io_out_bits.rand_init();
  }

  void eval(bool update_registers, bool verbose, bool done_reset) {
    UInt<1> _T_41 = ~p;
    UInt<1> _T_44 = io_in_valid & _T_41;
    UInt<16> _GEN_0 = _T_44 ? io_in_bits_a : x;
    UInt<16> _GEN_1 = _T_44 ? io_in_bits_b : y;
    UInt<1> _GEN_2 = _T_44 | p;
    UInt<1> _T_46 = x > y;
    UInt<16> _GEN_3 = _T_46 ? y : _GEN_0;
    UInt<16> _GEN_4 = _T_46 ? x : _GEN_1;
    UInt<1> _T_48 = ~_T_46;
    UInt<17> _T_49 = y - x;
    UInt<16> _T_50 = _T_49.tail<1>();
    UInt<16> _GEN_5 = _T_48 ? _T_50 : _GEN_4;
    UInt<1> _T_52 = y == UInt<16>(0x0);
    io_out_valid = _T_52 & p;
    UInt<1> _GEN_8 = io_out_valid ? UInt<1>(0x0) : _GEN_2;
    io_in_ready = ~p;
    io_out_bits = x;
    UInt<16> x$next = p ? _GEN_3 : _GEN_0;
    UInt<16> y$next = p ? _GEN_5 : _GEN_1;
    UInt<1> p$next = reset ? UInt<1>(0x0) : _GEN_8;
    if (update_registers) x = x$next;
    if (update_registers) y = y$next;
    if (update_registers) p = p$next;
  }
} RealGCD2;

#endif  // REALGCD2_H_
