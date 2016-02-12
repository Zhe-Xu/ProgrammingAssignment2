## Put comments here that give an overall description of what your
## functions do

## A set of functions to cache the inverse of parameter matrix x:
## get: return x
## set: set x to y and i to null
## getinverse: return i
## setinverse: save inverse to i

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## If the inverse is null, compute it and save it in variable "i".
## Else, return i directly.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
