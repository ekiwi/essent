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

  UInt<32> io_in_bits_a$old;
  UInt<1> io_out_ready$old;
  UInt<32> io_in_bits_b$old;
  UInt<1> reset$old;
  UInt<1> io_in_valid$old;
  std::array<bool,1> PARTflags;
  bool sim_cached = false;
  bool regs_set = false;
  bool update_registers;
  bool done_reset;
  bool verbose;

  void EVAL_0() {
    PARTflags[0] = false;
    UInt<1> T_50 = x > y;
    io_in_ready = ~busy;
    UInt<1> start = io_in_valid & io_in_ready;
    io_out_bits = x;
    UInt<32> x$next;
    if (UNLIKELY(reset)) {
      x$next = UInt<32>(0x0);
    } else {
      UInt<32> _GEN_6;
      if (start) {
        _GEN_6 = io_in_bits_a;
      } else {
        UInt<32> _GEN_4;
        if (T_50) {
          UInt<33> T_51 = x - y;
          UInt<32> T_52 = T_51.tail<1>();
          _GEN_4 = T_52;
        } else {
          _GEN_4 = x;
        }
        _GEN_6 = _GEN_4;
      }
      x$next = _GEN_6;
    }
    UInt<32> y$next;
    if (UNLIKELY(reset)) {
      y$next = UInt<32>(0x0);
    } else {
      UInt<32> _GEN_7;
      if (start) {
        _GEN_7 = io_in_bits_b;
      } else {
        UInt<1> T_54 = ~T_50;
        UInt<32> _GEN_5;
        if (T_54) {
          UInt<33> T_55 = y - x;
          UInt<32> T_56 = T_55.tail<1>();
          _GEN_5 = T_56;
        } else {
          _GEN_5 = y;
        }
        _GEN_7 = _GEN_5;
      }
      y$next = _GEN_7;
    }
    UInt<1> busy$next;
    if (UNLIKELY(reset)) {
      busy$next = UInt<1>(0x0);
    } else {
      UInt<1> T_45 = done & io_out_ready;
      UInt<1> _GEN_1 = T_45 ? UInt<1>(0x0) : busy;
      UInt<1> _GEN_2 = start | _GEN_1;
      busy$next = _GEN_2;
    }
    io_out_valid = done;
    UInt<1> done$next;
    if (UNLIKELY(reset)) {
      done$next = UInt<1>(0x0);
    } else {
      UInt<1> _GEN_3;
      if (start) {
        _GEN_3 = UInt<1>(0x0);
      } else {
        UInt<1> T_42 = y == UInt<32>(0x0);
        UInt<1> T_43 = busy & T_42;
        UInt<1> _GEN_0 = T_43 | done;
        _GEN_3 = _GEN_0;
      }
      done$next = _GEN_3;
    }
    PARTflags[0] |= x != x$next;
    PARTflags[0] |= y != y$next;
    PARTflags[0] |= busy != busy$next;
    PARTflags[0] |= done != done$next;
    if (update_registers) x = x$next;
    if (update_registers) y = y$next;
    if (update_registers) busy = busy$next;
    if (update_registers) done = done$next;
  }

  void eval(bool update_registers, bool verbose, bool done_reset) {
    if (reset || !done_reset) {
      sim_cached = false;
      regs_set = false;
    }
    if (!sim_cached) {
      PARTflags.fill(true);
    }
    sim_cached = regs_set;
    this->update_registers = update_registers;
    this->done_reset = done_reset;
    this->verbose = verbose;
    PARTflags[0] |= io_in_bits_a != io_in_bits_a$old;
    PARTflags[0] |= io_out_ready != io_out_ready$old;
    PARTflags[0] |= io_in_bits_b != io_in_bits_b$old;
    PARTflags[0] |= reset != reset$old;
    PARTflags[0] |= io_in_valid != io_in_valid$old;
    io_in_bits_a$old = io_in_bits_a;
    io_out_ready$old = io_out_ready;
    io_in_bits_b$old = io_in_bits_b;
    reset$old = reset;
    io_in_valid$old = io_in_valid;
    if (PARTflags[0]) EVAL_0();
    regs_set = true;
  }
} gcd;

#endif  // GCD_H_
