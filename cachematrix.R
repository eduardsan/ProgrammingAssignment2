## This function creates a special "matrix" object that can cache its inverse.

## Implementation follows example from assignments' documentation.

makeCacheMatrix <- function(x = matrix()) {
    cahed_inv <- NULL
    set <- function(y) {
      x <<- y
      cahed_inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) cahed_inv <<- inv
    getinverse <- function() cahed_inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
