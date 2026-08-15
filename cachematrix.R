## These two functions together create a matrix that can
## cache its inverse, so it doesn't need to be recomputed every time
## if it has already been calculated once

## makeCacheMatrix creates a matrix object that can cache
## its inverse. It returns a list containing four functions.
## set: set the value of the matrix
## get: get the value of the matrix
## setinverse: set the value of the inverse (store it in the cache)
## getinverse: get the cached value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## then cacheSolve retrieves the inverse from the cache instead of recomputing it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}