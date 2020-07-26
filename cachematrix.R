## Put comments here that give an overall description of what your
## functions do

## This Function take the inverse of a matrix
## Autor: pablo.proano@epn.edu.ec

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  y<-NULL
  z<-NULL
  
  setval <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  getval <- function() x
  
  setprevious <- function(y) {
    z <<- y
  }
  getprevious <- function() z
  
  set_inverse <- function(solve) m_inv <<- solve
  get_inverse <- function() m_inv
  list(setval = setval, getval = getval,set_inverse = set_inverse, get_inverse = get_inverse,
       setprevious=setprevious,getprevious=getprevious  )
}


## This function take the inverse of a matrix, but if the inverse of the matrix have been
## already calculated, the function take it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inv <- x$get_inverse()
  data <- x$getval()
  data_prev <- x$getprevious()
  print(data)
  print(data_prev)
  if(!is.null(m_inv)) {
    if(!is.null(data_prev)){
      message("getting cached data")
      return(m_inv)
    }
  }
  m_inv <- solve(data, ...)
  x$set_inverse(m_inv)
  x$setprevious(data)
  m_inv
}
