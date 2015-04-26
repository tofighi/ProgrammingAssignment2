#Matrix inversion is usually a costly computation and there may be some 
#benefit to caching the inverse of a matrix rather than computing it repeatedly
#In this way, the following functions will perform operations based on cashing

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(inputMatrix= matrix()) {

  invMatrix <- NULL
  
  setCached <- function(y) {
    inputMatrix <<- y
    invMatrix <<- NULL
  }
  
  # Functions for getting and setting cached inverse matrix values
  getCached <- function() inputMatrix
  # Calculation inverse matrix using built-in solve() function
  setCached.inverse <- function(solve) invMatrix <<- solve
  getCached.inverse <- function() invMatrix
  
  list(
    setCached = setCached, 
    getCached = getCached,
    setCached.inverse = setCached.inverse,
    getCached.inverse = getCached.inverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(cacheable.matrix, ...) {
  invMatrix <- cacheable.matrix$getCached.inverse()
  # Checking if there is a cached matrix available
  if(!is.null(invMatrix)) {
    message("Getting cached inverse matrix")
    return(invMatrix)
  }

  # If there is no cached matrix available, an inverted matrix will be created
  matrix.to.inverse <- cacheable.matrix$getCached()
  invMatrix <- solve(matrix.to.inverse)
  cacheable.matrix$setCached.inverse(invMatrix)
  # returning inverse
  invMatrix
  
}