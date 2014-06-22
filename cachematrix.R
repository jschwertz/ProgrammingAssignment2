## Assignment 2 of R Programming:
##
## The first function creates a special "matrix" (actually a list with a matrix
## and a function) and caches (writes) its inverse to the parent function, or
## global environment.  The second function computes the inverse of the special
## "matrix".  If the matrix has already been calculated then the function
## returns the cached "matrix".

## This function creates a special "matrix" (actually a list with a matrix
## and a function) and caches (writes) its inverse to the parent function, or
## global environment.

makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize variable in this environment
    m <- NULL
    
    ## initialize variables in the parent environment
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## define "functions"
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
    
    ## intialize variables
    m <- x$getmenan()
    
    ## check for cached "matrix"
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## cache doesn't exist calculate inverse
    
    ## initialize variables
    data <- x$get()
    m <- mean(data, ...)
    
    ## calculate inverse
    x$setmean(m)
    
    ## return newly calculated "matrix"
    m
}
