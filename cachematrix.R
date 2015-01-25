## Caching the Inverse of a Matrix


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## For example, if you have your matrix (which inverse you are interested in) stored in MAT, 
## you should call this function this way: makeCacheMatrix (x = MAT). 
## It will return a "matrix" object which actually consists of 4 functions:
## set(), get(), setInverse(), getInverse().
## In order to store the result in the new varialbe NEW, print the following:
## NEW <- makeCacheMatrix (x = MAT).
makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## In the example above, the original matrix was called MAT and the returned "matrix" was called NEW.
## If the inverse of MAT has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Otherwise, it will calculate the inverse of MAT, print it and store it in the corresponding part of NEW.
## In our example, you should call this function the following way: cacheSolve(NEW).
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse

}
