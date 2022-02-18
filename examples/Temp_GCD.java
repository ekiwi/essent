package examples;

public import java.math.BigInteger;
public class GCD implements Simulator {
  public long x = 0L;
  public long y = 0L;
  public boolean clk = false;
  public boolean reset = false;
  public long io_a = 0L;
  public long io_b = 0L;
  public boolean io_e = false;
  public long io_z = 0L;
  public boolean io_v = false;
  public void eval(boolean update_registers, boolean verbose, boolean done_reset) {
    boolean T_7 = x > y;
    long T_8 = x - y;
    long T_9 = T_8 & 0xffff;
    long _GEN_0 = T_7 ? T_9 : x;
    boolean T_12 = !T_7;
    long T_13 = y - x;
    long T_14 = T_13 & 0xffff;
    long _GEN_1 = T_12 ? T_14 : y;
    io_z = x;
    io_v = y == 0;
    long x$next = io_e ? io_a : _GEN_0;
    long y$next = io_e ? io_b : _GEN_1;
    if (update_registers) x = x$next;
    if (update_registers) y = y$next;
  }

  //public long peek(String var) {
  //}

  //public long poke(String var, long val) {
  //}

  //public long step() {
  //}
}
class Temp_GCD {
    
}
