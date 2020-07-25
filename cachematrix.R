## Put comments here that give an overall description of what your
## functions do

## This Function take the inverse of a matrix
## Autor: pablo.proano@epn.edu.ec

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setval <- function(y) {
    x <<- y
    m <<- NULL
  }
  getval <- function() x
  set_inverse <- function(solve) m <<- solve
  get_inverse <- function() m
  list(setval = setval, getval = getval,set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function take the inverse of a matrix, but if the inverse of the matrix have been
## already calculated, the function take it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getval()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}
