/***********************************************************************************
 * Dependencies: none
 * Programming language: SCALA
 * @author Ahmed Alia
 * @email abualia4@najah.edu 
 * @date july 2019 
 * Description: Defines the boundary box of 2D coordinate space for each point
 * @param xMin minimum value of boundary box based on x axis 
 * @param yMin minimum value of boundary box based on y axis 
 * @param xMax maximum value of boundary box based on x axis
 * @param yMax maximum value of boundary box based on y axis
 ***********************************************************************************/
import scala.math._
/**
 * Defines the boundary box of 2D coordinate space for each point
 * @param xMin minimum value of boundary box based on x axis 
 * @param yMin minimum value of boundary box based on y axis 
 * @param xMax maximum value of boundary box based on x axis
 * @param yMax maximum value of boundary box based on y axis
 */
class Rectangle(xMin:Int,yMin:Int, xMax:Int,yMax:Int){
   
   val xmin:Int=xMin
   val ymin:Int=yMin
   val xmax:Int=xMax
   val ymax:Int=yMax
   
   /**
    *To get minimum value of boundary box based on x axis 
    * @return integer
    */
   def xMin():Int={
        xmin
   } 
   
   /**
    *To get minimum value of boundary box based on y axis  
    * @return integer
    */
   def yMin():Int={
        ymin
   } 
   /**
    *To get maximun value of boundary box based on x axis 
    * @return integer
    */
   def xMax():Int={
        xmax
   } 
   
   /**
    *To get maximum value of boundary box based on y axis 
    * @return integer
    */
   def yMax():Int={
       ymax
   }
   
    /**
    * To find the minimum distance between the given point and a boundary box for current point
    * @param value  given point
    * @return double
    */
    
    def distanceSquaredTo(value :Point) :Double={
       var dx:Double= 0.0
       var dy:Double = 0.0;
       val p=value.getPoint()
        if(p(0) < xmin) 
          dx = p(0) - xmin
        else if (p(0) > xmax) 
                 dx = p(0) - xmax
            
        if (p(1)< ymin) 
           dy = p(1) - ymin
        else if (p(1)> ymax)
                dy = p(1) - ymax;
          
        sqrt( dx*dx + dy*dy) 
          
    }
    
}
