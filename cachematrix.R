## Functions to create a cached matrix (represented as a list of several functions) and calculate (and cache) its inverse.


## creates a matrix whose inverse, once computed, is cached.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invr) inv <<- invr
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Takes a cached matrix and calculates its inverse (which is then cached.)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
