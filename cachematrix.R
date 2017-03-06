## These are simple functions for saving the inverse of a matrix in the cache.
## The first one actually allows setting and getting the matrix/cached inverse while the second calculates the inverse if it's not already cached

## This is a simple function that receives a matrix object and returns a list of get and set functions (oop like) for retrieving the matrix itself and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(input_inv) {
    inverse <<- input_inv
  }
  getinverse <- function() {
    inverse
  }
  list(set = set, get = get, setinv = setinverse, getinv = getinverse)
}


## This simple function checks if a given special matrix (above) object has a cached inverse. if not, it calculates and caches the inverse through the setinverse function

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("retrieving inverse matrix")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setinv(inverse)
  inverse
}
