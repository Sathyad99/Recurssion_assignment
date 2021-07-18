object recursion extends App{

  //Question 1
  def GCD (x:  Int, y: Int):Int = y match{
    case 0 => x;
    case y if(y>x) => GCD(y, x);
    case y => GCD(y, x%y);
  }
  def prime(num : Int, d: Int=2): Boolean = num match{
    case num if(d==num)=>true;
    case num if GCD(num,d)>1 =>false;
    case num => prime(num, d+1);
  }
  println("Checking prime or not");
  println(prime(5));
  println(prime(8));

  //Question 2
  def primeSeq(p : Int, n : Int=2): Any ={
    if(p>=n){if(prime(n)) println(n);
    primeSeq(p, n+1)};
  }
println("Printing all prime numbers less than 'n'");
primeSeq(10);

//Question 3
def sum(number : Int): Int={
  if(number==1) 1;
  else (number+sum(number-1));
}
println("Printing the addition of numbers from 1-n")
println(sum(5));

//Question 4
def IsEven(num: Int): Boolean = num match{
  case 0 => true
  case _ => IsOdd(num-1)
}
def IsOdd(num : Int): Boolean = !(IsEven(num))

println("Determining whether a number is even or odd");
println(IsEven(20));
println(IsEven(7));
println(IsOdd(45));
println(IsOdd(18));

//Qestion 5

def findEvenNum(m : Int) : Int = {
  if(m%2 == 0) addEvenNum(m-2);
  else addEvenNum(m-1);
}

def addEvenNum(m: Int): Int = m match{
  case 0 => 0;
  case 1 => 0;
  case m if(m<0) => 0;
  case m => (m+addEvenNum(m-2));
}
println("Printing the addition of even numbers less than n");
println(addEvenNum(20));

//Question 6

def fibonacci(num: Int): Int = num match{
  case 0 => 0;
  case 1 => 1;
  case num => fibonacci(num-1)+fibonacci(num-2);
}
def fibSeq(num : Int): Any = {
  if(num>0) fibSeq(num-1);
  println(fibonacci(num));
}
println("Printing the fibonacci series of 'n'");
println(fibSeq(9));
}
