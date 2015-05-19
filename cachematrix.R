## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(newMatrix){
        x <<- newMatrix
        cachedInverse <<- NULL
    }
    get <- function(){
        x
    }
    setInverse <- function(newInverse){
        cachedInverse <<- newInverse
    }
    getInverse <- function(){
        cachedInverse
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    cachedValue <- x$getInverse()
    if(!is.null(cachedValue)) {
        message("getting cached data")
    } else {
        data <- x$get()
        cachedValue <- solve(data, ...)
        x$setInverse(cachedValue)
    }
    cachedValue
}
