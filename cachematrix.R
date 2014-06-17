## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(inverse) m <-- inverse
    getSolve <- function() m
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not
## changed), then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if (!is.null(m)) {
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
