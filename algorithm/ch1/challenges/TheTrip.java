import java.io.*;
import java.util.*;
import java.lang.*;

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
    public void run(){
        int n = Integer.parseInt(Main.ReadLn(100));
        while(n != 0) {
            int[] cents = new int[n];
            int sum = 0;
            for (int i = 0; i < n; i++) {
                String line = Main.ReadLn(100);
                String[] m = line.split("\\.");
                cents[i] = Integer.parseInt(m[0]) * 100;
                if (m[1].length() == 1)
                    cents[i] += Integer.parseInt(m[1] + "0");
                else if (m[1].length() == 2)
                    cents[i] += Integer.parseInt(m[1]);
                sum += cents[i];
            }
            int avg = sum / n;
            int m = sum % n;
            Arrays.sort(cents);
            int[] balance = new int[n];
            for (int i=0; i<n; i++) {
                if (i < n-m) balance[i] = avg;
                else balance[i] = avg + 1;
            }
            int changes = 0;
            for (int i = 0; cents[i] < balance[i]; i++) {
                changes += balance[i] - cents[i];
            }
            System.out.printf("$%d.%02d\n", changes / 100, changes % 100);
            n = Integer.parseInt(Main.ReadLn(100));
        }
    }
}
