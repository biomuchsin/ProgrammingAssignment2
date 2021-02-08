## This functions were written as the solution for R programming course in Coursera

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(invrs) inv <<- invrs
  getInverse <- function() inv
  
  list(set = set, get = get, 
       setInverse = setInverse, getInverse = getInverse)
  
}


## Computes the  inverse of the special "matrix"

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  dat <- x$get()
  inv <- solve(dat, ...)
  x$setInverse(inv)
  inv
}
