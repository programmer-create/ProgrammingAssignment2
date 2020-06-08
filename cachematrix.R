## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##This calculates the inverse of the matrix returned by makeCacheMatrix
    inv <- NULL
    set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) ##this is used to find the inverse of a function
  getInverse <- function() inv
  list(set = set, get = get,setInverse = setInverse, getInverse = getInverse)
}

##Will store previously called matrices

cacheSolve <- function(x, ...) {
 ##This will find the inverse of the matrix from makeCacheMatrix 
  inv <- x$getInverse()  
  if (!is.null(inv)) {   
    message("getting cached data")
    return(inv) ## This will return a matrix that is the inverse of 'x' if it is not null.
  }
  mat <- x$get() 
  inv <- solve(mat, ...)
  x$setInverse(inv) 
  inv
}
