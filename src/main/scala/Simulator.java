package essent;

import java.math.BigInteger;

public abstract class Simulator {
    abstract public BigInteger peek(String var);

    abstract public void poke(String var, BigInteger val);

    abstract public void step(boolean update_registers);

    public boolean xorr(long x) {
        int k = 0;
        long d = x;
        while (d != 0) {
            k = k + 1;
            d = d & (d - 1L);
        }
        return k % 2 == 1;
    }

    public boolean xorr(BigInteger x) {
        int k = 0;
        BigInteger d = x;
        while (!d.equals(BigInteger.ZERO)) {
            k = k + 1;
            d = d.and(d.subtract(BigInteger.ONE));
        }
        return k % 2 == 1;
    }

    public long twosComplement(long x, int w) {
        if (x == 0) {
            return 0L;
        } else if (x > 0) {
            return (((1L << (64 - w)) - 1L) << w) | x;
        }
        else {
            return ~(((1L << (64 - w)) - 1L) << w) | x;
        }
    }

    public BigInteger twosComplement(BigInteger x, int w) {
        if (x.equals(BigInteger.ZERO)) {
            return BigInteger.ZERO;
        }
        if (x.compareTo(BigInteger.ZERO) > 0) {
            return x.subtract(BigInteger.ONE.shiftLeft(w));
        }
        else {
            return x.add(BigInteger.ONE.shiftLeft(w));
        }
    }
}
