# makeCacheMatrix
# returns matrix object that stores its inverse

makeCacheMatrix <- function(mtx = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
        mtx <<- y
        cachedInverse <<- NULL
    }
    get <- function() mtx
    setInverse <- function(inv) cachedInverse <<- inv
    getInverse <- function() cachedInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



# cacheSolve
# looks for inverse of matrix in the cache, if not found, computes inverse and sets cache. Returns
# inverse of the matrix.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        # message("getting cached data")
        return(inv)
    }
    mtx <- x$get()
    inv <- solve(mtx, ...)
    x$setInverse(inv)
    inv
}
