## Assignment 2 of R Programming:
## The first function creates a special "matrix" (actually a list with a matrix
## and a function) and caches (writes) its inverse to the parent function, or
## global environment.  The second function computes the inverse of the special
## "matrix".  If the matrix has already been calculated then the function
## returns the cached "matrix".

## This function creates a special "matrix" (actually a list with a matrix
## and a function) and caches (writes) its inverse to the parent function, or
## global environment.

makeCacheMatrix <- function(x = matrix()) {
    ## initialize variables in this environment
    m <- NULL
    set <- function(y) {
        ## initialize variables in the parent environment
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    
    ## return the special "matrix"
    list (set = set, get = get,
          setmean = setmean,
          getmean = getmean)
}



## This function computes the inverse of the special "matrix".  If the matrix 
## has already been calculated then the function returns the cached "matrix".

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmenan()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
