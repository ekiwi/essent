#ifndef TEST_H_
#define TEST_H_

#include <array>
#include <cstdint>
#include <cstdlib>
#include <uint.h>
#include <sint.h>
#define UNLIKELY(condition) __builtin_expect(static_cast<bool>(condition), 0)

typedef struct child {
  UInt<8> r;

  child() {
    r.rand_init();
  }
} child;

typedef struct test {
  UInt<8> r;
  UInt<1> clock;
  UInt<1> reset;
  UInt<8> in;
  UInt<8> out;
  child c;

  test() {
    r.rand_init();
    reset.rand_init();
    in.rand_init();
    out.rand_init();
  }

  UInt<8> in$old;
  UInt<1> reset$old;
  std::array<bool,1> PARTflags;
  bool sim_cached = false;
  bool regs_set = false;
  bool update_registers;
  bool done_reset;
  bool verbose;

  void EVAL_0() {
    PARTflags[0] = false;
    UInt<8> r$next;
    if (UNLIKELY(reset)) {
      r$next = UInt<8>(0x0);
    } else {
      r$next = ~c.r;
    }
    UInt<8> c$r$next;
    if (UNLIKELY(reset)) {
      c$r$next = UInt<8>(0x0);
    } else {
      c$r$next = in;
    }
    out = r;
    PARTflags[0] |= c.r != c$r$next;
    PARTflags[0] |= r != r$next;
    if (update_registers) c.r = c$r$next;
    if (update_registers) r = r$next;
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
    PARTflags[0] |= in != in$old;
    PARTflags[0] |= reset != reset$old;
    in$old = in;
    reset$old = reset;
    if (PARTflags[0]) EVAL_0();
    regs_set = true;
  }
} test;

#endif  // TEST_H_
