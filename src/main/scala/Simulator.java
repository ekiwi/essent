package essent;

import java.math.BigInteger;

public abstract class Simulator {
    abstract public BigInteger peek(String var);

    abstract public void poke(String var, BigInteger val);

    abstract public void step(boolean update_registers);

    private long xorr(long x) {
        long k = 0;
        long d = x;
        while (d != 0) {
            k = k + 1;
            d = d & (d - 1);
        }
        return k % 2;
    }

    private long xorr(BigInteger x) {
        long k = 0;
        BigInteger d = x;
        while (!d.equals(BigInteger.ZERO)) {
            k = k + 1;
            d = d.and(d.subtract(BigInteger.ONE));
        }
        return k % 2;
    }
}
