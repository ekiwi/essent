public interface Simulator {
    int peek(String var);

    void poke(String var, int val);

    void step();
}
