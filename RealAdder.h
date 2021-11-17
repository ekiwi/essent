#ifndef REALADDER_H_
#define REALADDER_H_

#include <array>
#include <cstdint>
#include <cstdlib>
#include <uint.h>
#include <sint.h>
#define UNLIKELY(condition) __builtin_expect(static_cast<bool>(condition), 0)

typedef struct BBFAdd {
  UInt<64> out;
  UInt<64> in2;
  UInt<64> in1;
} BBFAdd;

typedef struct RealAdder {
  UInt<64> register1_node;
  UInt<1> clk;
  UInt<1> reset;
  UInt<64> io_a1_node;
  UInt<64> io_a2_node;
  UInt<64> io_c_node;
  BBFAdd BBFAdd_1;

  RealAdder() {
    register1_node.rand_init();
    reset.rand_init();
    io_a1_node.rand_init();
    io_a2_node.rand_init();
    io_c_node.rand_init();
  }

  void eval(bool update_registers, bool verbose, bool done_reset) {
    io_c_node = register1_node;
    UInt<64> register1_node$next = BBFAdd_1.out;
    BBFAdd_1.in2 = io_a2_node;
    BBFAdd_1.in1 = io_a1_node;
    if (update_registers) register1_node = register1_node$next;
  }
} RealAdder;

#endif  // REALADDER_H_
