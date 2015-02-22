## These are a pair of functions that cache and compute the inverse
## of a matrix

## makeCacheMatrix creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve computes the inverse of the special
## "matrix" returned by makeCacheMatrix. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

