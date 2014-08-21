## These functions are designed to recevie an invertable matrix, invert the
## matrix, and cache the result to an object for use later. If this calculation
## has already been completed with the result cached, it will retrieve the cached
## data instead of recalculating the inverse

## This function creates a list containing functions to: 1) set the matrix object, 
## 2) get the matrix, 3) set the inverse of the matrix, and 4) get the inverse

makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL
        set <- function(y) {
                x <<- y
                invert <<- NULL
        }
        get <- function() x
        setinvert <- function(solve) invert <<- solve
        getinvert <- function() invert
        list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)
}


## This function first checks to see if the inverted matrix has already been solved
## and cached. If so, it returns the cached result. If not, this function inverts
## the matrix and caches the results using the "makeCacheMatrix" sub-functions

cacheSolve <- function(x, ...) {
        invert <- x$getinvert()
        if(!is.null(invert)) {
                message("Getting cached data...")
                return(invert)
        }
        data <- x$get()
        invert <- solve(data, ...)
        x$setinvert(invert)
        invert
}