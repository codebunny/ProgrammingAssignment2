## The makeCacheMatrix creates a special "matrix" object, which can then
## be fed into cacheSolve to return the inverse of that matrix.
## The matrix is assumed to be invertible.

## This function creates a special matrix object that can cache its inverse
## The matrix is assumed to be invertible

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y){
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
     m <- x$getsolve()
     
     # check if matrix has been solved already.
     # if so, return value from cache
     if(!is.null(m)){
          message("Getting cached data")
          return(m)
     }
     
     # if matrix has not been solved, then solve
     data <- x$get()
     m <- solve(data, ...)
     x$setsolve(m)
     
     # return inverse of matrix
     m
}
