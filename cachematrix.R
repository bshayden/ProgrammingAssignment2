## makeCacheMatrix and cacheSolve cache the inverse of a matrix.

## makeCacheMatrix creates a matrix object which is able to cache its inverse.
## It returns set, get, setinverse, and getinverse.

makeCacheMatrix <- function(x = matrix()) {
    cached_inverse <- NULL
    set <- function(y) {
        x <<- y
        cached_inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cached_inverse <<- inverse
    getInverse <- function() cached_inverse
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve computes the inverse of the matrix. 
## If the inverse for the matrix has already been calculated,
## it is instead retrieved from the cache.

cacheSolve <- function(x, ...) {
    cached_inverse <- x$getInverse()
    if(!is.null(cached_inverse)) {
        message("getting cached data")
        return(cached_inverse)
    }
    data <- x$get()
    cached_inverse <- solve(data, ...)
    x$setInverse(cached_inverse)
    cached_inverse
}
