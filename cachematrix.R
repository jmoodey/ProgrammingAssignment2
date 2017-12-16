## Matrix inversion is usually a costly computation and there is a 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The following functions can be used to calculate the inverse of a matrix 
## cache it in memory. 
# 
## The makeCacheMatrix creates special "matrix" object that can chache 
## it's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve will retrieve 
## the inverse from the cache.
## Note: The function assumes that the matrix supplied is always invertible. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    message("Calculate inverse")
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
