import java.math.BigInteger;
import essent.Simulator;

public class Test extends Simulator {
  public long register$ = 0L;
  public boolean clock = false;
  public boolean reset = false;
  public long out = 0L;
  public Test() {
  }
  public void eval(boolean update_registers, boolean verbose, boolean done_reset) {
    long _register_T = register$ + 1;
    long _register_T_1 = _register_T & ((1L << 4) - 1);
    out = register$;
    long register$$next = reset ? 0 : _register_T_1;
    if (update_registers) register$ = register$$next;
  }

   @Override public BigInteger peek(String var) {
    switch (var) {
      case "register$": return BigInteger.valueOf(register$);
      case "reset": return BigInteger.valueOf(reset ? 1 : 0);
      case "out": return BigInteger.valueOf(out);
      default: return null;
    }
  }

   @Override public void poke(String var, BigInteger val) {
    switch (var) {
      case "register$": register$ = val.longValue(); return;
      case "reset": reset = !val.equals(BigInteger.ZERO); return;
      case "out": out = val.longValue(); return;
    }
  }

  @Override public void step(boolean update_registers) {
    eval(update_registers, false, false);
  }
}
