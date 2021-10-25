


public class gcd {
  boolean busy;
  boolean done;
  long x;
  long y;
  boolean clock;
  boolean reset;
  boolean io_in_ready;
  boolean io_in_valid;
  long io_in_bits_a;
  long io_in_bits_b;
  boolean io_out_ready;
  boolean io_out_valid;
  long io_out_bits;

  public gcd() {
    busy = rand.b();
    done = rand.b();
    x = rand.l(32);
    y = rand.l(32);
    reset = rand.b();
    io_in_ready = rand.b();
    io_in_valid = rand.b();
    io_in_bits_a = rand.l(32);
    io_in_bits_b = rand.l(32);
    io_out_ready = rand.b();
    io_out_valid = rand.b();
    io_out_bits = rand.l(32);
  }

  public void eval(boolean update_registers, boolean verbose, boolean done_reset) {
    boolean T_42 = y == 0;
    boolean T_43 = busy && T_42;
    boolean _GEN_0 = T_43 || done;
    boolean T_45 = done && io_out_ready;
    boolean _GEN_1 = T_45 ? false : busy;
    io_in_ready = !busy;
    boolean start = io_in_valid && io_in_ready;
    boolean _GEN_2 = start || _GEN_1;
    boolean _GEN_3 = start ? false : _GEN_0;
    boolean T_50 = x > y;
    long T_51 = x - y;
    long T_52 = T_51 & 0xffffffffL;
    long _GEN_4 = T_50 ? T_52 : x;
    boolean T_54 = !T_50;
    long T_55 = y - x;
    long T_56 = T_55 & 0xffffffffL;
    long _GEN_5 = T_54 ? T_56 : y;
    long _GEN_6 = start ? io_in_bits_a : _GEN_4;
    long _GEN_7 = start ? io_in_bits_b : _GEN_5;
    io_out_valid = done;
    io_out_bits = x;
    if (update_registers) busy = reset ? false : _GEN_2;
    if (update_registers) done = reset ? false : _GEN_3;
    if (update_registers) x = reset ? 0x0 : _GEN_6;
    if (update_registers) y = reset ? 0x0 : _GEN_7;
  }
}
