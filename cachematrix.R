## This is the set of functions created to solve and cache the value of the inverse of a matrix

## This function is responsable to store a data structure that can be 
## used to cache the resulve of the inverse of a matrix. 
## The first and only argument must be a matrix.
##
## e.g. 
##   makeCacheMatrix(matrix(1:4, 2))

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


## This function is responsable to solve a matrix that is stored at
## the data structure created by the function makeCacheMatrix.
## It accepts any argument that will be passed to the function solve
## in order to calculate the cached matrix properlly or simply returns
## its cached value.

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
