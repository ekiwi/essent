#!/bin/zsh

echo 1. Compile \n 2. Clean Up

read user_selection

if [[ $user_selection == "1" ]];
then
    echo Deleting GCD.java
    rm examples/GCD.java

    # echo Deleting GCD.class
    # rm examples/GCD.class

    echo Compiling Essent via sbt
    sbt assembly

    echo Creating GCD.java
    ./utils/bin/essent -java  -O0 ~/Documents/Research/Essent/essent/examples/GCD.fir

    # echo Compiling GCD.java => GCD.class
    # javac examples/GCD.java
fi
if [[ $user_selection == "2" ]];
then
    echo Deleting GCD.java
    rm examples/GCD.java

    # echo Deleting GCD.class
    # rm examples/GCD.class
fi


# Add to GCD to test

# public String toString(){
#     return "x = " + x + "\ny = " + y + "\nclk = " + clk + "\nreset = " + reset + "\nio_a = " + io_a + "\nio_b = " + io_b + "\nio_e = " + io_e + "\nio_z = " + io_z + "\nio_v = " + io_v;
# }

# public static void main(String args[]) { 
#     GCD test = new GCD();
#     test.eval(true, true, true);
#     System.out.println(test.toString());
# }

