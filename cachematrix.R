## This function "makeCacheMatrix" cache the inverse of a matrix and  function "cacheSolve" computes the inverse returned by "makeCacheMatrix". if the inverse is calculated the same matrix, then the "cacheSolve"retrieve the inverse from the  

## The function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes inverse of matrix

cacheSolve <- function(x, ...) {
  s <- x$getsolve()  
  if(!is.null(s)) {
    message("getting cached data") ## Return a matrix that is the inverse of 'x'
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
