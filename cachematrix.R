
## Creates a list containing: 
    ## a function to set the value of a matrix
    ## get the value of the matrix
    ## set the value of the inverse matrix
    ## get the value of the inverse matrix 
## Assumes input is a squared matrix having an inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates the inverse of a matrix using the list returned by the makeCacheMatrix function above. 
## If the inverse matrix has been calculated before, it gets it from the cache and skips the computation.
## Otherwise, the inverse is calculated and stored in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
