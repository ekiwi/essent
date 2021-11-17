#ifndef TOP_H_
#define TOP_H_

#include <array>
#include <cstdint>
#include <cstdlib>
#include <uint.h>
#include <sint.h>
#define UNLIKELY(condition) __builtin_expect(static_cast<bool>(condition), 0)

typedef struct Level1 {
  UInt<16> reg1;
  Level2 level2;

  Level1() {
    reg1.rand_init();
  }
} Level1;

typedef struct Level2 {
  UInt<16> reg2;
  Level3 level3;

  Level2() {
    reg2.rand_init();
  }
} Level2;

typedef struct Level3 {

  Level3() {
  }
} Level3;

typedef struct Top {
  UInt<1> clk;
  UInt<1> reset;
  UInt<16> in1;
  UInt<16> out1;
  UInt<16> out2;
  UInt<16> out3;
  Level1 level1;

  Top() {
    reset.rand_init();
    in1.rand_init();
    out1.rand_init();
    out2.rand_init();
    out3.rand_init();
  }

  void eval(bool update_registers, bool verbose, bool done_reset) {
    UInt<16> level1$out1 = level1.reg1;
    out1 = level1$out1;
    UInt<16> level1$level2$out2 = level1.level2.reg2;
    UInt<16> level1$out2 = level1$level2$out2;
    out2 = level1$out2;
    UInt<16> level1$in1 = in1;
    UInt<16> level1$level2$in1 = level1$in1;
    UInt<16> level1$level2$level3$in1 = level1$level2$in1;
    UInt<16> level1$level2$level3$out3 = level1$level2$level3$in1;
    UInt<16> level1$level2$out3 = level1$level2$level3$out3;
    UInt<16> level1$out3 = level1$level2$out3;
    out3 = level1$out3;
    UInt<16> level1$reg1$next = level1$in1;
    if (update_registers) level1.reg1 = level1$reg1$next;
    UInt<16> level1$level2$reg2$next = level1$level2$in1;
    if (update_registers) level1.level2.reg2 = level1$level2$reg2$next;
  }
} Top;

#endif  // TOP_H_
