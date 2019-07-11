 /******************************************************************************************************************
 * Dependencies: Node.scala, Point.scala, Rectangle.scala, BKDTree.scala
 * Programming language: SCALA
 * @author Ahmed Alia
 * @email abualia4@najah.edu 
 * @date july 2019 
 * Description: This class is responsible for:
 * Finding the points in a B2DT that are closest to a given point using  KNN with complete search space.
 * Finding the points in a B2DT that are closest to a given point using KNN with pruning search space.
 * Finding the points in a B2DT that are within a given distance using range search with pruning search space.
 * Finding the distance between each pairs of nodes using pruning search space
 ************************************************************************************************************************/

import scala.math._

/**
 * 
 */
class Search{
    
    /**
     * The Following lists are used as initialization lists in the following methods
     */
    var queue:List[(Point,Double)]=List()
    var sortedQueue:List[(Point,Double)]=List()
    
    var queueP:List[(Point,Double)]=List()
    var sortedQueueP:List[(Point,Double)]=List()
    
    var queueRange:List[(Point,Double)]=List()
    var sortedQueueRange:List[(Point,Double)]=List()
   
    var queueR:List[(Point,Point,Double)]=List()
    var sortedQueueR:List[(Point,Point,Double)]=List()
    
    var queuePair:List[(Point,Point,Double)]=List()

   /**
    * find the euclidean distance between the two points(x,y)
    * @param a is the first point
    * @param a is the second point
    * @return double
    */
   def euclideanDistance(a:Point, b:Point, k:Int):Double={
        var sum=0.0;
        for(i<-0 to k-1){
             sum+= pow((a.getPoint()(i)-b.getPoint()(i)),2)
             
        }
        
        return  sqrt(sum)
    }
    
    /**
     * Finding the points in a B2DT that are closest  to a given point using  KNN with complete search space.
     * @param current is the current node starting from the root
     * @param point is the given point
     * @param n is the number of neighbors points
     * @param depth is the current level of B2DT
     * @return List[(Point,Double)]
     */
    def KNNSearch(current:Node, givenP:Point, n:Int, depth: Int):List[(Point,Double)]={
               
         var cd=depth%2      
         if(current==null) 
             return null
                 
         else  {
                    var ed:Double=euclideanDistance(givenP,current.point,2)
                    var tuple=(current.point,ed)

                    if(queue.length<n) {
                        queue = tuple :: queue
                        sortedQueue=queue.sortBy(x=>(x._2))
                        queue=sortedQueue
                    }  
                    else { 
                          
                          if(tuple._2<sortedQueue.last._2){ 
                             queue=sortedQueue. updated(sortedQueue.length-1,tuple)
                             sortedQueue=queue.sortBy(x=>(x._2))
                             queue=sortedQueue
                             
                          }
                              
                        }
                        
                  if(givenP.getPoint()(cd)<current.point.getPoint()(cd)){ 
                               KNNSearch(current.left,givenP,n,depth+1)  
                               KNNSearch(current.right,givenP,n,depth+1) 

                         }
                           else{  

                               KNNSearch(current.right,givenP,n,depth+1)  
                               KNNSearch(current.left,givenP,n,depth+1) 
                            
                          }
              

           } 
            
         return sortedQueue     
     }
     
     
     /**
     * Finding the points in a B2DT that are closest to a given point using KNN with pruning search space.
     * @param current is the current node
     * @param givenP is the given point
     * @param n is the number of  neighbors points
     * @param depth is the current level of B2DT
     * @param minimum is the current best distance between the given point and furthest neighbor
     * @return List[(Point,Double)]
     */
      def KNNPrune(current:Node, givenP:Point, n:Int, depth: Int, minimumD:Double):List[(Point,Double)]={
               
         var cd=depth%2 
          
         if(current==null||current.rect.distanceSquaredTo(givenP)>minimumD) {
          
             return null
          }
          
         else  {      
                      var ed:Double=euclideanDistance(givenP,current.point,2)
                      var tuple=(current.point,ed)
                      
                      if(queueP.length<n) {
                          queueP = tuple :: queueP
                          sortedQueueP=queueP.sortBy(x=>(x._2))
                          queueP=sortedQueueP
                     }  
                      else { 
                          
                           if(tuple._2<sortedQueueP.last._2){ 
                               queueP=sortedQueueP. updated(sortedQueueP.length-1,tuple)
                               sortedQueueP=queueP.sortBy(x=>(x._2))
                               queueP=sortedQueueP
                            }
                        }
                       
                          if(givenP.getPoint()(cd)<current.point.getPoint()(cd)){ 
                               KNNPrune(current.left,givenP,n,depth+1,sortedQueueP.last._2)  
                               KNNPrune(current.right,givenP,n,depth+1,sortedQueueP.last._2)
                         }
                           else{  

                               KNNPrune(current.right,givenP,n,depth+1,sortedQueueP.last._2)  
                               KNNPrune(current.left,givenP,n,depth+1,sortedQueueP.last._2) 
                          }

           } 
            
         return sortedQueueP     
     }
     
     
    /**
     * Finding the points in a B2DT that are within a given distance using range search with pruning search space.
     * @param current is the current node
     * @param givenP is the given point
     * @param depth is the current level of B2DT
     * @param minimum is the given minimum distance which is used in range search 
     */
      def range(current:Node, givenP:Point,  depth: Int, minimumD:Double):List[(Point,Double)]={
               
         var cd=depth%2 
          
          if(current==null||current.rect.distanceSquaredTo(givenP)>minimumD) {
              return null
          }
          
          else  {      
                      var ed:Double=euclideanDistance(givenP,current.point,2)
                      var tuple=(current.point,ed)
                      
                      if(ed<=minimumD) {
                          queueRange = tuple :: queueRange
                          sortedQueueRange=queueRange.sortBy(x=>(x._2))
                          queueRange=sortedQueueRange
                           
                     }  
                      
                         
                          if(givenP.getPoint()(cd)<current.point.getPoint()(cd)){ 
                              range(current.left,givenP,depth+1,minimumD)  
                              range(current.right,givenP,depth+1,minimumD) 

                         }
                          else{  
                              range(current.right,givenP,depth+1,minimumD)  
                              range(current.left,givenP,depth+1,minimumD) 
                          }

          } 
            
         return sortedQueueRange     
     } 
     
     /**
      * Finding the distance between each pairs of nodes using pruning search space
      * findRoot, pairDistance and pair methods are used to find the distance between each pairs without any duplication 
      */
     
     /** 
      * Find the root of B2DT based on the current node
      * @param tree is the current node
      * @return Node
      */
      
     def findRoot(tree:Node):Node={
       
        if(tree.parent==null)
          return tree
        else{
            findRoot(tree.parent)
        }
       
      }
      
      /**
      * To find the distance between the given node and others without any duplication 
      * @param current is the current node
      * @param givenP is the given node
      * @param depth is the current level of B2DT
      * @param minimum is the given minimum distance which is used as threshold
      * @return List[(Point,Point,Double)]
      */
      def pairDistance(current:Node, givenP:Node,  depth: Int, minimumD:Double):List[(Point,Point,Double)]={
               
         var cd=depth%2
           if(current==null||givenP==null||current.rect.distanceSquaredTo(givenP.point)>minimumD) {
               return null
           }
          else  {     if( current.checked==0){ 
                         var ed:Double=euclideanDistance(givenP.point,current.point,2)
                        
                         if(ed<=minimumD && ed!=0 ) {
                              var tuple=(givenP.point,current.point,ed)
                              queueR = tuple :: queueR
                              sortedQueueR=queueR.sortBy(x=>(x._3))
                              queueR=sortedQueueR
                            }  
                       }    
                      
                          if(givenP.point.getPoint()(cd)<current.point.getPoint()(cd)){ 
                               pairDistance(current.left,givenP,depth+1,minimumD)  
                               pairDistance(current.right,givenP,depth+1,minimumD) 

                          }
                           else{  
                               pairDistance(current.right,givenP,depth+1,minimumD)  
                               pairDistance(current.left,givenP,depth+1,minimumD) 
                               }
                        
           } 
            
         return sortedQueueR     
     } 
     
     
     /**
      * This method is used to determine the first node in each pair.
      * it is responsible to avoid the duplication of pairs
      * @param current is the current node which starts from  B2DT's root
      * @param tree is the current node which starts from the B2DT's root, tree is a copy of the current
      * @param depth is the current level of B2DT
      * @param distance is the given minimum distance which is used as threshold
      * @return List[(Point,Point,Double)]
      */
      def pair(current1:Node, tree:Node,   depth: Int, distance:Double):List[(Point,Point,Double)]={ 
          
          queueR=List()
          sortedQueueR=List()
          
          if(tree==null&& current1==null)
              return null
          else{  
                current1.checked=1
                  
               var res=pairDistance(findRoot(current1), tree,  0, distance)  
               if(res!=null)
               queuePair=res++queuePair
               
              pair(current1.left ,tree.left, depth+1, distance)
              pair(current1.right  ,tree.right,depth+1, distance)
                 
               }
        return queuePair 
      }
      
      
}
