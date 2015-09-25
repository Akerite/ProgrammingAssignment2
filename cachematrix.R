## Caching the Inverse of a Matrix
## The following functions are used to create a special object that stores a matrix and
## caches its inverse

## The first function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
          x <<- y
          m <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}



## The second function computes the inverse of the special "matrix" created by the first
## function. If the inverse has already be computed, it will retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if (!is.null(m)){
          message("getting cached data")
          return(m)
        }
        mat <- x$get()
        m <- solve(mat, ...)
        x$setInverse(m)
        m
}
