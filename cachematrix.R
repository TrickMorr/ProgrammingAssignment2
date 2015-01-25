## The two functions work together. The first one resets or creates
## the cache and creates a list of functions to be called by the
## second function, or by the user as needed.


## makeCacheMatrix() takes or creates a matrix and initially  
## sets the cached inverse to NULL.
## It creates a list of functions that can be assigned to a 
## variable and then are either called by the user or called from within a 
## call of cacheSolve. 
## Functions are:
## set(), to set the matrix value if it is changed,   
## getsolve(), to retrieve the value of s, which is either the cached inverse or NULL  
## get(), to retrieve the new matrix values to calculate the new inverse
## setsolve(), to cache the inverse calculated in cacheSolve()


makeCacheMatrix <- function(x = matrix()) {
        s <- NULL   
                                                ##set() to set new matrix,
                                                ##sets as vector the matrix
        set <- function(y) {                    ##of Ts & Fs from y==x.
                test <- as.vector(y == x)       ##if sum of range is not 2, is
                if(sum(range(as.numeric(test))) != 2) { 
                x <<- y                         ##at least one FALSE, new/old
                s <<- NULL                      ##matrices are not equal, so 
                } else {                        ##reset matrix value & cache
                        message("input matrix is equal to previous matrix")
                        message("inverse cache preserved")
                }                               
        }                                       
        get <- function() x 
        setsolve <- function(solve) s <<- solve   
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## cacheSolve accesses the functions created by makeCacheMatrix,
## to check for an existing cached value and return it.
## If cached value is NULL, the function then calls the get
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
