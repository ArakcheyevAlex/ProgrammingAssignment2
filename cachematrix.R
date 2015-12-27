## Functions to cache the inverse of a matrix
## 

## Function: makeCacheMatrix(x)
##
## Makes list of functions to store cached value of the matrix 
## and inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set=set, 
         get=get,
         setInverse=setInverse,
         getInverse=getInverse)
    
}

## Function: cacheSolve(x, ...)
##
## Computes the inverse of a matrix made by makeCacheMatrix()
## If it already has been calculated, returns cached value.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        print("getting cached data")
        return(inv)
    }
    inv <- solve(x$get())
    x$setInverse(inv)
    inv
}