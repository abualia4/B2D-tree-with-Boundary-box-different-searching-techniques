/*******************************************************
 * Dependencies: Point.scala, Rectangle.scala
 * Programming language: SCALA
 * @author Ahmed Alia
 * @email abualia4@najah.edu 
 * @date july 2019 
 * Description: Defines the data type of each node
 * @param value is the node's point (x,y)
 *******************************************************/

class Node(value:Point){
    
   var point:Point=value
   var parent:Node=null
   var left:Node=null
   var right:Node=null
   var rect:Rectangle=null
   var checked:Int=0
 
}
