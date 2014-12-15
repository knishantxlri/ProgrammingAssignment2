## This function creates a special "matrix" object that can cache its inverse
## set the value of the matrix, get the value of the matrix
#set the value of the inverse, get the value of the inverse

makeCacheMatrix <- function(x = matrix(c(1,2,3,4),nrow =2, ncol = 2)){
      inv <- NULL
      
#method to assign new value to the object
      set <- function(y){
          x <<- y
          inv <<- NULL
      }
      
#method to get the value of object
      get <- function(){x}
      
#method to set matrix inverse
      setinverse <- function (inverse){
          inv <<- inverse
      }
      
#method to get the inverse
      getinverse <- function(){inv}
      
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
#If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...){
#get the value of inverse
      inv <- x$getinverse()
#check if inverse is already there, then return inverse
      if(!is.null(inv)){
          message("getting the cached data")
          return(inv)
      }
#get matrix for calculating inverse
      mat <- x$get()
#calculate inverse of the matrix
      inv <- solve(mat)
#set the value of inverse
      x$setinverse(inv)
#return inverse
      inv 
}  
