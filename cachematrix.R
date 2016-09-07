## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## Below are a pair of functions that cache the inverse of a matrix.

## Note:
## 1) Computing the inverse of a square matrix can be done with the solve function 
## in R. For example, if X is a square invertible matrix, then solve(X) returns 
## its inverse.
## 2) The <<- operator is be used to assign a value to an object in an environment 
## that is different from the current environment. 

## Assumption: The matrix supplied is always invertible.

## Example:
## > source("ProgrammingAssignment2/cachematrix.R")
## > inversematrix <- makeCacheMatrix(matrix(1:4,2,2))
## > cacheSolve(inversematrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) minv <<- inverse
    getinverse <- function() minv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getinverse()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setinverse(minv)
    minv
}
