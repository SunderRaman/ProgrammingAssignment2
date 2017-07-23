## There are 2 functions makeCachematrix(get/set matrix & its invrese) and 
## cacheSolve for returning the inverse from cache 

## This function gets and set a matrix, as well as its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  invMatrix <- NULL
  
  # get the Matrix
  get <- function() {
    x
  }
  
  # set the Matrix
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  # get the Inverse of the Matrix
  getinverse <- function() {
    invMatrix
  }

  # set the Inverse of the Matrix
  setinverse <- function(inverseMatrix) 
  {
    invMatrix <<- inverseMatrix
  }
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function returns the inverse of the matrix and if 
# already available, then picks from the cache
cacheSolve <- function(x, ...) {
  invMatrix <- x$getinverse()

    if(!is.null(invMatrix)) {
      message("Getting the inverse of the matrix from cache")
      return(invMatrix)
    }
  # get the Current matrix
  curMatrix <- x$get()
  # inverse the matrix
  invMatrix <- solve(curMatrix)
  #set the inverse of the matrix
  x$setinverse(invMatrix)
  invMatrix
}
