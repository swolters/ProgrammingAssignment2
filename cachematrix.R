## The two functions created from the programming assignment 
## template functions create a matric and cache its inverse and 
## then check in the second fucntion if the inverse has already 
## been calculated in which case it is retrieved from the cache.

## This function creates a special "matrix" object that can cache 
## its inverse.
## In an analogous way as the template function makeVector this 
## sets and gets the matrix and the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    iv  <- NULL
    set <- function(y) {
        x <<- y 
        iv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) iv <<- inverse
    getInverse <- function() iv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return the inverse matrix and check if it already exists
    iv <- x$getInverse()
    if (!is.null(iv)) {
        message("cached inverse matrix")
        return(iv)
    }
    mx <- x$get()
    iv <- solve(mx, ...)
    x$setInverse(iv)
    iv
}
