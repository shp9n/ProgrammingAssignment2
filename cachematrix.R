## The following is a pair of functions that cache and compute the 
## inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtx <<- x; #assign the input matrix x to the variable mtx 
        inverse <<- NULL; #re-initialize inverse in the parent environment to null 
    }
    get <- function() return(mtx); #return the matrix mtx
    setinv <- function(inv) inverse <<- inv; #set the inverse equal to the inverse of the matrix x 
    getinv <- function() return(inverse); #return the cached inverse of x 
    return(list(set = set, 
    get = get, 
    setinv = setinv, 
    getinv = getinv))
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) { #return a matrix that is the inverse of mtx
    inverse <- mtx$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- mtx$get()
    invserse <- solve(data, ...)
    mtx$setinv(inverse)
    return(inverse)
}
