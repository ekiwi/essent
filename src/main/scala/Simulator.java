import java.math.BigInteger;

public interface Simulator {
    BigInteger peek(String var);

    void poke(String var, BigInteger val);

    void step(boolean update_registers);
}
