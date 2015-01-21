## makeCacheMatrix takes a matrix and first sets the cached inverse
## to NULL, then creates a list of functions that can be
## called by the user or usually from a call of cacheSolve, to check
## for a cached value of the matrix inverse and return it, or solve 
## for a new inverse value, and cache and return that value.
## 

## cache s is set to NULL

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



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
        ## Return a matrix that is the inverse of 'x'
}
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
