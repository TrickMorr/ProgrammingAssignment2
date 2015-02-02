## As required in Assignment 2 of Coursera course, R Programming:
##
## These two functions are for caching the inverse of a matrix
## in order to conserve CPU time in a repetitive application.
##
## The two functions work together. The first creates or resets
## the cache and then creates a list of functions called by the
## second, in order to calculate a value and store it in memory.
## 
##
## ***Function 1:***
##
## makeCacheMatrix(x = matrix())
## arguments: x, a square matrix (by default creates an empty one)
## output: special list of functions
## other output: environment for storage of cached values
## 
## Details:
## Whenever called, it creates/resets a cached inverse with value NULL
## 
## It then assigns the input matrix to a local variable and creates
## a list of four functions:
##
## set()        This function is different, and to be called separately
##              from the other three, which are all called from 
##              within cacheSolve().
##              It takes a matrix as its argument, checks if it is a new 
##              value, and if so, assigns it to the local variable 
##              retrievable by the get() function.
## 
## Functions designed to be called from within cacheSolve:
##
## getsolve()   retrieves the value of s, the cached inverse(or NULL)
## get()        retrieves the matrix assigned to the local variable
## setsolve()   caches the newly calculated inverse


makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        
        ## set() is a bit different from the assignment example. This set is
        ## hopefully more efficient by comparing the new matrix to the matrix
        ## which was previously set. If they are identical, even if assigned to
        ## a different variable, the function will recognize that the inverse 
        ## will therefore also be identical, so the cached inverse is preserved.
        set <- function(y) {
                if(!identical(x, y)) {
                x <<- y
                s <<- NULL
                } else {
                        message("input matrix is identical to previous matrix")
                        message("inverse cache preserved")
                }
        }
        get <- function() x 
        setsolve <- function(inverse) s <<- inverse   
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## ***Function 2:***
##
## cacheSolve(x)
## argument: x, the special list created by makeCacheMatrix()
## output: inverse of the matrix passed to makeCacheMatrix() or set()
## other output: calculated inverse cached for quick retrieval
##
## Details:
## cacheSolve() first calls getsolve() and checks for a cached value.
## If value is not NULL, it is returned, computational power is conserved.
##
## If cached value is NULL, cacheSolve() calls 
## get() to get the matrix entered by either set(),
## or the original call of makeCacheMatrix(). 
##
## cacheSolve() then solves for the inverse, calls
## setsolve() to set that inverse as the cached value,
## and finally returns same calculated inverse.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s       ## Returns a matrix that is the inverse of 'x'
}
