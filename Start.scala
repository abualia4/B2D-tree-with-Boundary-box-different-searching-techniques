 /******************************************************************************************************************
 * Dependencies: Node.scala, Point.scala, Rectangle.scala, BKDTree.scala, Search.scala
 * Programming language: SCALA
 * @author Ahmed Alia
 * @email abualia4@najah.edu 
 * @date july 2019 
 * Description: This object contains main method to launch a Scala application:
 ************************************************************************************************************************/
object Start{
    def main(args:Array[String]){
        
       var data:List[Point] =List()

        /**
        * Generating random Data to build the balanced 2D tree
        * Notice there is opportunity to duplicate points, 
        * but in insertBKDTree method removes the duplication during build the B2DT.
        * */
          val r = scala.util.Random
          for(j<-1 to 1000){
              
              var pointObject=new Point(r.nextInt(1000000), r.nextInt(1000000) ) 
              data= pointObject ::data
         }
            
        
        val bk=new BKDTree()
        
        //Build the balanced 2D tree
        val timeBuild0 = System.nanoTime()  
        bk.insertList(data,0)
        val timeBuild1 = System.nanoTime()  
        println("Elapsed time for building B2DT : " + (timeBuild1 - timeBuild0)/1000 + " μs")
        //Given Point for searching
        val givenPoint:Point=new Point(500,500)
        val search=new Search()
        print("The given Point for searching is: " )  
        givenPoint.printPoint()
       
        println("\n-----------------------------------------------------------------------------------------------------")
        //KNN with complete Search Space
        val time0 = System.nanoTime()
        val k:Int=100 // number of neighbours
        val completeSearch=search.KNNSearch(bk.current, givenPoint, k, 0)
        val time1 = System.nanoTime()
        print("KNN with complete search-> "+k+ " Neighbours points are: ")
        for(i<-completeSearch)
          i._1.printPoint()
           
        //KNN with pruning Search Space
        println("Elapsed time : " + (time1 - time0)/1000 + " μs")
        val t0 = System.nanoTime()
        val result=search.KNNPrune(bk.current, givenPoint, k, 0,99999999)
        val t1 = System.nanoTime()
        print("KNN with pruning search-> " +k+ " Neighbours points are: ")
        for(i<-result)
          i._1.printPoint()
        println("Elapsed time : " + (t1 - t0)/1000 + " μs")
       
       //Range search with pruning Search Space
       val timeR0 = System.nanoTime()
       val givenDistance:Double=500
       val prune=search.range(bk.current,givenPoint,0,givenDistance)
       val timeR1 = System.nanoTime()
       print("Range with pruning search-> " +" Points within "+ givenDistance +" are: ")
       for(i<-prune)
          i._1.printPoint()
       println("Elapsed time : " + (timeR1 - timeR0)/1000 + " μs")
      
       //Distance between every pair of nodes in the tree within specific distanc
       println("Distance between every pair of nodes in the tree within: "+givenDistance+ "are as follows:")
       val timePair0 = System.nanoTime()
       var u= search.pair(bk.current,bk.current,0,givenDistance)
       val timePair1 = System.nanoTime()  
       println("Elapsed time for distance between each pair : " + (timePair1 - timePair0)/1000 + " μs")
    
      for(w<-u){
          print("[")
          w._1.printPoint()
          print(",")
          w._2.printPoint()
          print("]")
          println(" Distance: "+w._3)
        }
        
        
   }
}

Start.main(Array("Go"))
