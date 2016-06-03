## These functions will use caching to compute the inverse of a matrix
## when possible rather than repeatedly computing it.

## makeCacheMatrix sets and gets the values of the initial matrix and
## the inverse of the initial matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve returns the inverse of the given matrix. It does so by
## checking to see if the inverse has already been computed. If it has
## it does not do any further computations and just gets the inverse.
## If it has not, the function computes the inverse and sets the inverse.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
