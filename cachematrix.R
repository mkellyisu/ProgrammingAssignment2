## Functions to create a cached matrix and store its inverse

## Function returning a list with setter, getter, setInverse and getInverse 
## functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(mean) m <<- mean
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Function that takes a cached matrix.  Checks for a cached inverse matrix, and
## if found, returns the cached matrix.  Otherwise, solves for the inverse of
## the matrix, caches the inverse matrix and returns it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m  
}

