import java.io.*;
import java.util.*;

public class Secret_Entrance {
  public static void main(String[] args) throws FileNotFoundException {
    Scanner scanner = new Scanner(new File("input.txt"));

    while (scanner.hasNextLine()) {
      String line = scanner.nextLine();
      System.out.println(line);
    }

    scanner.close();
  }
}
