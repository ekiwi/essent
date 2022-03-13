package essent;

import java.math.BigInteger;

public abstract class Simulator {
    abstract public BigInteger peek(String var);

    abstract public void poke(String var, BigInteger val);

    abstract public void step(boolean update_registers);
}
