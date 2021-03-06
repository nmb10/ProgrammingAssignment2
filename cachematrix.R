## Matrix inversion is usually a costly computation and there may be some
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
        # changes initial matrix and removes old matrix cache
        mtx <<- y
        cachedInverse <<- NULL
    }
    get <- function() mtx
    setInverse <- function(inv) {
        # sets inverse version of the matrix
        cachedInverse <<- inv
    }
    getInverse <- function() {
        # returns inverse of the matrix
        cachedInverse
    }
    list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


# cacheSolve
# looks for inverse of matrix in the cache, if not found, computes inverse and sets cache. Returns
# inverse of the matrix.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        # cached invers exists, return it
        return(inv)
    }
    # inverse is not cached yet, cache it and return
    mtx <- x$get()
    inv <- solve(mtx, ...)
    x$setInverse(inv)
    inv
}
