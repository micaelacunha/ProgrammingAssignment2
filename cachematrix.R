## Matrix inversion is usually a costly computation. The two functions
## in this file allow for caching the inverse of a matrix, avoiding its
## computation every time.

## This function creates a "matrix" object that can store its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <- inverse
        getinverse <- function() inv
        list( set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned
## by the function makeCacheMatrix. If it has already been calculated,
## it retrieves the inverse of the matrix from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mtrx <- x$get()
        inv <- solve(mtrx, ...)
        x$setinverse(inv)
        inv
}
