## The two functions interact with each other. The first one resets 
## the cache and creates a list of functions for the second one to
## use.


## makeCacheMatrix() takes a matrix and first, since it's a new 
## matrix, sets the cached inverse to NULL.
## It then creates a list of functions that can be assigned to a 
## variable and either called by the user or called from within a 
## call of cacheSolve, to check for a cached value of the matrix 
## inverse and return it, or solve for a new inverse value, and 
## cache and return that value.


makeCacheMatrix <- function(x = matrix()) {
        s <- NULL   
        set <- function(y) {   
                x <<- y   
                s <<- NULL   
        }
        get <- function() x 
        setsolve <- function(solve) s <<- solve   
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## cacheSolve takes the list created by makeCacheMatrix, and uses
## the functions within it to check for an existing cached value and 
## return it.
## If no cached value is found the function then calls the get
## function to get the matrix, solves for the inverse, calls the 
## setsolve function to set that inverse as the cached value, and
## finally returns that same calculated inverse.

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
