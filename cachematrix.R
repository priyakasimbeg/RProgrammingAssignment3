##  To create a special object that stores a matrix and cache's its inverse.



## Creates a matrix object that has getters and setters for the matrix data and inverse.

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


## Returns the inverse of an invertible matrix. If the inverse is stored in the cache, it is retrieved and returned. Otherwise it is first calculated.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(as.matrix(data), ...)
    x$setinverse(i)
    i
}
