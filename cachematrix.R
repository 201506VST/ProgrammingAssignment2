## Solution to R Assginement 2
## Objective of assignment: to write a pair of functions that cache the inverse of a matrix
## with:
## function 1: "makeCacheMatrix" - to create a special "matrix" object that can cache its inverse
## function 2: "cacheSolve" - to compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

## Solution fuction 1 - "makeCacheMatrix"

makeCacheMatrix <- function(x = matrix()) { ## creates matrix object x
    a <- NULL                  ## define the cache a
    set <- function(y) {
        x <<- y                ## assign the input matrix y to the variable x in the parent environment
        a <<- NULL             ## re-initialize a in the parent environment to null
    }
    get <- function() x        ## return the matrix x
    setinverse <- function(inv) a <<- inv ## set the cache a equal to the inverse of the matrix x
    getinverse <- function() a ## return the cached inverse of x
    list(set = set,get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}



## Solution fuction 2 - "cacheSolve" - Returns a matrix that is the inverse of 'x'

## This function calculates the inverse of the special "matrix" created in function 1 (see above)
## It first checks to see if the inverse 
## has already been caclulated. If so, it 'get's the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
    a <- x$getinverse()
    if(!is.null(a)) {          ## checks to see if the cached date a is filled, if yes 
        message("getting cached data") ## this messafe is shown and
        return(a)              ## data is retrieved from the cache a and then skips the computation
    }
    
    ## is a is NULL (no cache), then the inverse of the special "matrix" x is computed and cached in a
    data <- x$get()
    a <- solve(data, ...)
    x$setinverse(a)
    a
}