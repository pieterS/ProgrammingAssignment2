## The makeCacheMatrix and cacheSolve functions offer a way to define a matrix,
## calculate its inverse and cache the inverse for later use. 

## makeCacheMatrix defines four functions on square matrix x for further 
## manipulation of the matrix:
## set: set the values in the matrix
## get: get the values in the matrix
## setinverse: calculate the inverse of the matrix
## getinverse: load the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    # set the cached inverse to NULL
    i <- NULL
    
    # define the set function. When used, reset the cached inverse to NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # define the get function
    get <- function() x
    
    # define the setinverse function
    setinverse <- function(solve) i <<- solve
    
    # define the getinverse function
    getinverse <- function() i
    
    # return the list with the for functions 
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## return the inverse of the matrix x
## the cachesolve function takes a matrix made with makeCacheMatrix and returns
## its inverse.  If the inverse has been calculated before and has been cached,
## return the cached inverse. If the inverse has not been calculated before, 
## calculate it and cache it.

cacheSolve <- function(x, ...) {
    # get the cached inverse
    i <- x$getinverse()
    
    # if there is a cached inverse matrix, return it
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # get the matrix to invert and invert it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    
    # return the inverted matric
    i
}
