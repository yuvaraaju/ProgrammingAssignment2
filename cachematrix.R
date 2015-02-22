##
## script: cachematrix.R
## author: Yuvaraaju
##   date: 22-FEB-2015

## Calculating an inverse of a matrix is an expensive operations.
## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function creates an inverse of a matix.
## If the matrix has not changed, then this function retrieves the inverse.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting inverse of given matrix from cache")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
