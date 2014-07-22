## Assignment: Caching the Inverse of a Matrix
#########################################################################################
## Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the inverse
## of a matrix rather than compute it repeatedly.

## Below pair of functions are designed to cache the inverse of a matrix.
##
## There are two parts of functions here:
## part 1: makeCacheMatrix
##      This function creates a special "matrix" object that can cache its inverse.
## part 2: cacheSolve
##      This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##      If the inverse has already been calculated (and the matrix has not changed),
##      then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <<- x$getsolve()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        print(data)
        m <<- solve(data)
        x$setsolve(m)
        m
        
}
