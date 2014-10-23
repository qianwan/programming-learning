import java.io.*;
import java.util.*;

class Main implements Runnable {
    static String ReadLn(int maxLength) {  // utility function to read from stdin,
        // Provided by Programming-challenges, edit for style only
        byte line[] = new byte [maxLength];
        int length = 0;
        int input = -1;
        try {
            while (length < maxLength) {//Read untill maxlength
                input = System.in.read();
                if ((input < 0) || (input == '\n')) break; //or untill end of line ninput
                line [length++] += input;
            }

            if ((input < 0) && (length == 0)) return null;  // eof
            return new String(line, 0, length);
        } catch (IOException e) {
            return null;
        }
    }

    public static void main(String args[]) {  // entry point from OS
        Main myWork = new Main();  // Construct the bootloader
        myWork.run();            // execute
    }

    public void run() {
        new myStuff().run();
    }
}

class myStuff implements Runnable {
    int[] cache = new int[1000000];

    private int cycleLength(int k) {
        if (cache[k] != 0) return cache[k];
        if (k == 1) return 1;
        if (k % 2 == 0) cache[k] = 1 + cycleLength(k / 2);
        else cache[k] = 1 + cycleLength(3 * k + 1);
        return cache[k];
    }

    public void run() {
        int maxLength = 100;
        String line = ThreeNPlus1.ReadLn(maxLength);
        while (line != null) {
            String[] ij = line.split(" ");
            int i = Integer.parseInt(ij[0]);
            int j = Integer.parseInt(ij[1]);
            if (i > j) {
                int tmp = i;
                i = j;
                j = i;
            }
            int maxCycleLength = 0;
            for (int k = i; k <= j; k++) {
                int tmp = cycleLength(k);
                if (tmp > maxCycleLength) maxCycleLength = tmp;
            }
            System.out.println(line + " " + maxCycleLength);
            line = ThreeNPlus1.ReadLn(maxLength);
        }
    }
}
