package essent;

import java.math.BigInteger;

public class JavaUtils {
    public long andr(long x, int w) {
        for (int i = w - 1; i >= 0; i--) {
            if ((x & (1L << i)) == 0)
                return 0L;
        }
        return 1L;
    }

    public long andr(BigInteger x, int w) {
        for (int i = w - 1; i >= 0; i--) {
            if (x.and(BigInteger.ONE.shiftLeft(i)).equals(BigInteger.ZERO))
                return 0L;
        }
        return 1L;
    }

    public long orr(long x, int w) {
        for (int i = 0; i < w; i++) {
            if ((x & (1L << i)) != 0)
                return 1L;
        }
        return 0L;
    }

    public long orr(BigInteger x, int w) {
        for (int i = 0; i < w; i++) {
            if (!x.and(BigInteger.ONE.shiftLeft(i)).equals(BigInteger.ZERO))
                return 1L;
        }
        return 0L;
    }

    public long xorr(long x) {
        long k = 0;
        long d = x;
        while (d != 0) {
            k = k + 1;
            d = d & (d - 1);
        }
        return k % 2;
    }

    public long xorr(BigInteger x) {
        long k = 0;
        BigInteger d = x;
        while (!d.equals(BigInteger.ZERO)) {
            k = k + 1;
            d = d.and(d.subtract(BigInteger.ONE));
        }
        return k % 2;
    }
}
