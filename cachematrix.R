## These functions create a special "super matrix" that stores a matrix
## and its inverse, and returns the inverse matrix.

## This function creates a list of functions that create and retrieve the
## original and inverse matrices

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) invrs <<- solve
  getinverse <- function() invrs
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates and returnds the inverse of the matrix created
## above, if the inverse has not already been calculated.
## If the inverse has already been calculated, it is retrieved from the cache
## and returned. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data,...)
  x$setinverse(invrs)
  invrs
}
