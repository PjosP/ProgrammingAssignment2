## Put comments here that give an overall description of what your
## Autor: pablo.proano@epn.edu.ec

## The function "makeCacheMatrix" creates a special "vector", which is really 
##a list containing a function to:

##1 set the value of the matrix
##2 get the value of the matrix
##3 set the value of the previous matrix used in the "cacheSolve Function"
##4 get the value of the previous matrix
##5 set the value of the inverse
##6 get the value of the inverse

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
## In order to test de function above run: 
## 1 mymatrix<-matrix(c(1,2,3,4),nrow<-2,ncol<-2)
## 2 special<-makeCacheMatrix(mymatrix)


## The function "cacheSolve" compute the inverse of a matrix, however, if the inverse
## has been already compute, the function get the value of the inverse from the cache
## also the function evaluate if the matrix to calculate the inverse is the same
## that a previous matrix used before.

cacheSolve <- function(x, ...) {
  m_inv <- x$get_inverse()
  data <- x$getval()
  data_prev <- x$getprevious()
  if(!is.null(m_inv)) {
    if(!is.null(data_prev)){
      if(identical(data,data_prev)){
        message("Same Data as before")
        message("getting cached data")
        return(m_inv)
      }
    }
    message("getting cached data")
    return(m_inv)
  }
  m_inv <- solve(data, ...)
  x$set_inverse(m_inv)
  x$setprevious(data)
  m_inv
}

## In order to test de function above run: 
## 1 cacheSolve(special)
## 2 cacheSolve(special) (The function will get the value from the cache)
## in order to evaluate a diferent matrix
## 3 othermatrix<-matrix(c(5,6,7,8),nrow<-2,ncol<-2)
## 4 special$setval(othermatrix)
## 5 cacheSolve(special) (The function will compute the inverse from the new matrix)
## 6 cacheSolve(special) (The function will get the value from the cache because the matrix is the same as before)