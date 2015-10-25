## Put comments here that give an overall description of what your
## functions do

## Creates matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(newInverse) inv <<- newInverse
    getInverse <- function() inv
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Computes the inverse of matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
