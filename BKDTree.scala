 /*******************************************************
 * Dependencies: Node.scala, Point.scala, Rectangle.scala
 * Programming language: SCALA
 * @author Ahmed Alia
 * @email abualia4@najah.edu 
 * @date july 2019 
 * Description: This class is responsible for:
 * building Balanced 2D Tree (B2DT)
 * Printing the B2DT
 * @param value is the node's point (x,y)
 *******************************************************/
 class BKDTree{
    var current:Node=null
    /**
     * building the B2DT
     * Determining the boundary box for each point
     * @param current is the current node starting from  the root of B2DT
     * @param value is the node's point(x,y)
     * @param depth is depth or the level of the current node
     * @return Node
     */
    def insertBKD(current:Node, value:Point, depth: Int ):Node ={
         
         if(current==null)
         return null
         
    
        var cd:Int=depth%2
        var newPoint=value.getPoint()
        var currentPoint=current.point.getPoint()
        
        /**
         * For X dimension/plane
         */
        if(cd==0){ 
           if(newPoint(cd)<currentPoint(cd)){
             if(current.left!=null)
                 insertBKD(current.left,value,depth+1) 
              else{
                  current.left=new Node(value)
                   
                  current.left.parent=current
                  current.left.rect=new Rectangle(current.rect.xMin(),current.rect.yMin(),currentPoint(0),current.rect.yMax())
                   
              }
               
           }
          
           else  {
                if(current.right!=null)
                    insertBKD(current.right,value,depth+1)
                else {
                    current.right=new Node(value)
                    current.right.parent=current
                     
                    current.right.rect=new Rectangle(currentPoint(0),current.rect.yMin(),current.rect.xMax(),current.rect.yMax())
 

                }

            }  
         }
         
         /**
         * For Y dimension/plane
         */
        else{
             if(newPoint(cd)<currentPoint(cd)){
               if(current.left!=null)
                 insertBKD(current.left,value,depth+1) 
               else{
                  current.left=new Node(value)
                  current.left.parent=current
                  
                  current.left.rect=new Rectangle(current.rect.xMin(),current.rect.yMin(),current.rect.xMax(),currentPoint(1))
       
               }
            }
          
             else {
                if(current.right!=null)
                    insertBKD(current.right,value,depth+1)
                else {
                    current.right=new Node(value)
                    current.right.parent=current
                  
                    current.right.rect=new Rectangle(current.rect.xMin(),currentPoint(1),current.rect.xMax(),current.rect.yMax())
                  }
             } 
        }
        
        return current
     }
     
    /**
     * This is used to insert one point to B2DT and it is called by the next method (insert)
     * @param Node is the current node
     * @param is the new node's point(x,y)
     * @return Boolean
     */
     def insert(current:Node,value:Point):Boolean={
         insertBKD(current,value,0)
        true  
     }
     
     /**
      * This is used to add the new node to B2DT
      * Also it prepares the first node(root) with it's boundary box (Initialization node)
      * @param value node's point(x,y)
      */
     def insertUser(value:Point):Unit={
         
         if(current==null)
         {
             current=new Node(value)
             current.rect=new Rectangle(0, 0, 9, 10);
             
         }
         else
           insert(current,value)
    }
     
    /**
     * Adding list of points(x,y) to B2DT
     * @param points set of points(x,y) 
     * @param depth the current depth/level of the B2DT
     */
    def insertList (points: List[Point], depth:Int ):Unit  ={
    
         if( points!=null)
         {       
             
           var sortedPoints:List[Point] =List()
         
           var cd = depth%2
           if(cd == 0)
            {
              sortedPoints=points.sortWith(_.getPoint()(0)<_.getPoint()(0))
            
            }
           else{
               sortedPoints=points.sortWith(_.getPoint()(1)<_.getPoint()(1))
              }
              
              var medianIndex:Int=(sortedPoints.length)/2  
           
              insertUser(sortedPoints(medianIndex))
              var leftSubtree:  List[Point]=List()
              var rightSubtree: List[Point]=List() 
              leftSubtree =sortedPoints.take(medianIndex)
              rightSubtree =sortedPoints.takeRight(sortedPoints.length-(medianIndex+1))  
            
              if(medianIndex-1>=0 &&leftSubtree.length>0){
                    insertList(leftSubtree,depth+1) 
                }
           
             if(medianIndex-1<sortedPoints.length &&rightSubtree.length>0){
                insertList(rightSubtree,depth+1)
       
          }  
        }
  }
             
  /**
   * Printing the B2DT's points(x,y)
   * starting from the root
   * then left subtree
   * then right subtree
   * @param current is the current node starting from the root
   */
     def printTree(current: Node ):Unit={
         
          if(current!=null) { 
              println( current.point.getPoint()(0)+"," +current.point.getPoint()(1))
              printTree(current.left )    
              printTree(current.right )
       }
      
     }
 }
 
