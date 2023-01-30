package essent;

import java.math.BigInteger;
import java.util.ArrayList;

public abstract class Simulator {
    public ArrayList<Integer> stop_codes = new ArrayList<>();

    public int[] getStopCodes() {
        return stop_codes.stream().mapToInt(i->i).toArray();
    }

    abstract public BigInteger peek(String var);

    abstract public void poke(String var, BigInteger val);

    abstract public boolean step(boolean update_registers);

    public boolean xorr(long x, int w) {
        int k = 0;
        long d = x & ((1L<<w) - 1);
        while (d != 0) {
            k = k + 1;
            d = d & (d - 1L);
        }
        return k % 2 == 1;
    }

    public boolean xorr(BigInteger x, int w) {
        int k = 0;
        BigInteger d = x.and(BigInteger.ONE.shiftLeft(w).subtract(BigInteger.ONE));
        while (!d.equals(BigInteger.ZERO)) {
            k = k + 1;
            d = d.and(d.subtract(BigInteger.ONE));
        }
        return k % 2 == 1;
    }

    /** Interprets X with bitwidth W as a signed integer. */
    public long asSInt(long x, int w) {
        if (x >= (1L << (w - 1))) {
            return (((1L << (64 - w)) - 1L) << w) | x;
        }
        return x;
    }

    /** Interprets X with bitwidth W as an unsigned integer. */
    public long asUInt(long x, int w) {
        if (x < 0L) {
            return ~(((1L << (64 - w)) - 1L) << w) & x;
        }
        return x;
    }

    /** Interprets X with bitwidth W as a signed integer. */
    public BigInteger asSInt(BigInteger x, int w) {
        if (x.compareTo(BigInteger.ONE.shiftLeft(w - 1).subtract(BigInteger.ONE)) > 0) {
            return x.subtract(BigInteger.ONE.shiftLeft(w));
        }
        return x;
    }

    /** Interprets X with bitwidth W as an unsigned integer. */
    public BigInteger asUInt(BigInteger x, int w) {
        if (x.compareTo(BigInteger.ZERO) < 0){
            return x.add(BigInteger.ONE.shiftLeft(w));
        }
        return x;
    }
}
