public class jAnswers{

  public static void main(String [] args) {
    System.out.println(fiveB() + "");
  }

  public static int fiveB(){
    int count = 0;
    int pos = 0;
    int len = input.length;
    while(pos>=0 && pos < len){
      int oldPos = pos;
      int value = input[oldPos];
      pos = pos + value;

      if (value < 3)
        input[oldPos] = ++value;
      else
        input[oldPos] = --value;
      count++;
    }
    return count;
  }
}
