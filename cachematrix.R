## These are functions required for the programming assigment 2 of the data
## science specialization course 2. Their purpose is to efficiently compute the 
## inverse of a matrix in case when matrix does not change often and so its
## inverse can be stored in memory.

## Function below makes a matrix of a special type, which stores cache of
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Function below gets an inverse of a matrix of the special type, defined
## by previous function.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
