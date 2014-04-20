## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This two functions do exactly that. Create matrix that stores its inverse (using makeCacheMatrix)
## then get its inverse (using  cacheSolve)
##
## Minimal example:
##
## > mc <- makeCacheMatrix(mtx=matrix(c(1, 2, 3, 4), 2, 2))
##
## > cacheSolve(mc)  # first call, computing and set to cache
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(mc)  # second call, getting from cache
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


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
