##
## Assignment 2 from the R Programming course
## 
##              Illustrates lexical scoping using caching
##                  - Two functions makeCacheMatrix and cacheSolve are used to
##                    create and cache the inverse of a matrix.
##
##
## 

## The function makeCacheMatrix creates a special matrix function that caches its inverse
## The inverse of a matrix is computed by the solve() function. If the inverse has already
## been computed and stored in the cache it returns that value, if not it computes the inverse
## using the solve() function

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y) {
        x <<- y
        m <<- NULL
}
get <- function() x
setmatrix <- function(solve) m <<- solve()
getmatrix <- function() m
list(set = set, get = get,
     setmatrix = setmatrix,
     getmatrix = getmatrix)
}

##
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
##

cacheSolve <- function(x=matrix(), ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrixdata <- x$get()
        m <- solve(matrixdata, ...)
        x$setmatrix(m)
        m
}

## Some test code to test the functions
mat1 <- matrix(data=c(1,2,3,4),nrow=2,ncol=2)