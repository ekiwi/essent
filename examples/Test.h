#ifndef TEST_H_
#define TEST_H_

#include <array>
#include <cstdint>
#include <cstdlib>
#include <uint.h>
#include <sint.h>
#define UNLIKELY(condition) __builtin_expect(static_cast<bool>(condition), 0)

typedef struct Test {
  UInt<4> register$;
  UInt<1> clock;
  UInt<1> reset;
  UInt<4> out;

  Test() {
    register$.rand_init();
    reset.rand_init();
    out.rand_init();
  }

  void eval(bool update_registers, bool verbose, bool done_reset) {
    UInt<5> _register_T = register$ + UInt<4>(0x1);
    UInt<4> _register_T_1 = _register_T.tail<1>();
    out = register$;
    UInt<4> register$$next = reset ? UInt<4>(0x0) : _register_T_1;
    if (update_registers) register$ = register$$next;
  }
} Test;

#endif  // TEST_H_
