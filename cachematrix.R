## Date: 5/24/2015
## This is a file that creates a special matrix that can cache itself and its inverse
## There are two funcitons: makeCacheMatrix() and cacheSolve()
##To use this, a matrix that is able to be inversed needs to be created
## Example:
## examplematrix <- makeCacheMatrix()
## examplematrix$set(matrix(c(1,1,-1,2),nrow=2,ncol=2))
## examplematrix$get() #print matrix to see it 
## cacheSolve(examplematrix) #get the inverse matrix



## This is a creator function that creates a special matrix
## It has four functions that allow you to set and get the matrix
## along with also set and getting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   inverse<-NULL
   set <- function(y){
      x <<- y
      inverse <<- NULL
   }
   
   get <- function() x
   setinverse <- function(z) inverse <<- z
   getinverse <- function() inverse
   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This is a function that inerverse a special matrix created in makeCacheMatrix
## The function will try to find the cached inverse; if it cannot, it will run solve()

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)){
           message("getting cached data")
           return(inverse)
        }
        
        #no inverse cached, so pull the variable
        data <- x$get()
        inverse <-- solve(data)
        x$setinverse(inverse)
        inverse
}
