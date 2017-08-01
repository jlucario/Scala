
object Fibonacci {


//Tail Recursive
def j1(n: Int) = j1TR(n, 1, 0)

	def j1TR(n: Int, nxt: Int, result: Int): Int= {
	n match {
   	 case 0 => result
   	 case _ => j1TR(n-1, nxt+result, nxt)

     }
}



//Recursive

 def j2( n: Int): Int = n match {

    case 0 | 1 => n
    case _ => j2( n -1) + j2( n-2)
  }

 


//Iteration

 def j3( n: Int) = {
    
    var a = 0   
    var b = 1    
    
    def next( a: Int, b: Int) = Pair( b, a + b) ;    
  
    var i = 0
    while( i < n) {
      val Pair( c, d) = next( a, b) ;
      
      a = c
      b = d
      
      i = i +1
    }
    
    a
  }


  

//Simpler Stream	

lazy val j4 = {
  def j(a:Int,b:Int):Stream[Int] = a #:: j(b,a+b)
  j(0,1)
}



// functional stream

val j5:Stream[Int] = 0 #:: 1 #:: (j5 zip j5.tail).map{ case (a,b) => a+b }


	
	
	def main(args: Array[String]): Unit =  {
    var heading = String.format("%-7s %7s %7s %7s %7s %7s ", "i", "j1(i)", "j2(i)", "j3(i)","j4(i)","j5(i)")
    
    println(heading.replaceAll(".", "-"))
    println(heading)
    println(heading.replaceAll(".", "-"))
		for (i : Int <- 0 to 10) {
      printf("%-7d %7d %7d %7d %7d %7d  \n", i, j1(i), j2(i), j3(i),j4(i),j5(i))
		}
	}
  
}
