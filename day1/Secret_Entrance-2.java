import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Secret_Entrance {
  public static void main(String[] args) {
    try {

      Scanner scanner = new Scanner(new File("input.txt"));

      int position = 50;
      int result = 0;

      while (scanner.hasNextLine()) {
        String line = scanner.nextLine();
        char direction = line.charAt(0);
        int distance = Integer.parseInt(line.substring(1));

        for (int i = 0; i < distance; i++) {
          if (direction == 'R') position++;
          if (direction == 'L') position--;
          if (position % 100 == 0) result++;
        }
        // if (position == 0) result++;
        System.out.println("| pos: " + position + " | result: " + result + " | line: " + line);
      }

      System.out.println("Result: " + result);

      scanner.close();
    } catch (FileNotFoundException e) {
      System.out.println("File not found.");
      e.printStackTrace();
    }
  }
}
