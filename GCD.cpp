#include <iostream>

class GCD {
  public:
    int x = 0;
    int y = 0;
    int clk = 0;
    bool reset = false;
    int io_a = 0;
    int io_b = 0;
    bool io_e = false;
    int io_z = 0;
    bool io_v = false;
    void eval (bool update_registers) {
      bool T_7 = x>y;
      int T_8 = x - y;
      int T_9 = T_8 & 0xffff;
      int _GEN_0 = T_7 ? T_9 : x;
      bool T_12 = T_7 ? false : true;
      int T_13 = y - x;
      int T_14 = T_13 & 0xffff;
      int _GEN_1 = T_12 ? T_14 : y;
      io_z = x;
      io_v = y == 0;
      int x$next = io_e ? io_a : _GEN_0;
      int y$next = io_e ? io_b : _GEN_1;
      if (update_registers) x = x$next;
      if (update_registers) y = y$next;
    }
};

