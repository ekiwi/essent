package essent;

import java.math.BigInteger;

public abstract class Simulator {
    abstract public BigInteger peek(String var);

    abstract public void poke(String var, BigInteger val);

    abstract public void step(boolean update_registers);

    private boolean xorr(long x) {
        int k = 0;
        long d = x;
        while (d != 0) {
            k = k + 1;
            d = d & (d - 1L);
        }
        return k % 2 == 1;
    }

    private boolean xorr(BigInteger x) {
        int k = 0;
        BigInteger d = x;
        while (!d.equals(BigInteger.ZERO)) {
            k = k + 1;
            d = d.and(d.subtract(BigInteger.ONE));
        }
        return k % 2 == 1;
    }
}
