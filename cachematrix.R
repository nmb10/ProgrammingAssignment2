
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
