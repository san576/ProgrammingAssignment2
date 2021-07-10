## The two functions below are designed to make inverse computation of a matrix 
## efficient by using the cached value of the inverse matrix.

## This function stores the matrix and defines the necessary functions to store
## retrieve the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the matrix passed in the function 
## makeCacheMatrix and based on whether the function has been called earlier
## it either returns the previously computed value or computes the inverse of
## the matrix and stores it for future references.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
