/*************************************************************
 * Dependencies: none
 * Implementation of 2D point.
 * Programming language: SCALA
 * @author Ahmed Alia
 * @email abualia4@najah.edu 
 * @date july 2019 
 * Description: Data type for every point in each node (2D coordinate space)
 * @param x defines x-axis
 * @param y defines y-axis
 *************************************************************/
class Point(x:Int, y:Int)
{
    val x1=x
    val y1=y
    
   /**
    * To get the point from  the current node
    * @return array of two integer numbers
    */
    def getPoint():Array[Int]={
       Array(x,y)
    }
    /**
     * To print the point(x,y) from the current node
     */
    def printPoint():Unit={
       print("(" +x1 +","+y1+")")
    }

}
