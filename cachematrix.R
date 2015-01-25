## This cachematrix.R script provide two functions.
## These functions can be used to cache a square matrix and 
## its inverse matrix. And hence avoids the additional processing
## of inverse generation subsequent times.
## Sample commands would look as follows:
##      > x <- matrix(rnorm(9),3,3)
##      > y <- makeCacheMatrix(x)
##      > cacheSolve(y)

## 1. makeCacheMatrix
##    This object cache a square matrix and its inverse
##    Provides four utility functions to set, get the input matrix;
##    as well as setInverse and getInverse respectively.

makeCacheMatrix <- function(x = matrix()) {
    ## 'x' should be a square matrix for which inverse
    ## matrix will be cached
    
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) m <<- inv
    getInverse <- function() m
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse )
}


## 2. cacheSolve
## This function returns the inverse of a square matrix
## from cache, if available.
## First time it create the inverse using solve function
## and push to cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invx <- x$getInverse()
    if(!is.null(invx)){
        message("Getting cached data")
        return(invx)
    }else{
        message("No data in cache!")
    }
    data <- x$get()
    message("Invoke solve to create the inverse.")
    invx <- solve(data)
    x$setInverse(invx)
    invx
}
