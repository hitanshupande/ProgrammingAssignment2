## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a list that contains 4 functions: set, get, setinverse and getinverse
makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invrs <<- inverse
    getinverse <- function() invrs
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve function returns the inverse of the matrix. It checks if the inverse is already calculated. In that case
## it fetches the cached value, else it calculates and stores the inverse of the matrix
cacheSolve <- function(x, ...) {
    invrs <- x$getinverse()
    if(!is.null(invrs)) {
        message("getting cached data.")
        return(invrs)
    }
    data <- x$get()
    invrs <- solve(data)
    x$setinverse(invrs)
    invrs
}


