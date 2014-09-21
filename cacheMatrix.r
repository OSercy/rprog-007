## --------------------------------------------------
##      R programming (rprog-007)
##      Assignment 2
##
## Two functions that cache the inverse of a matrix
##---------------------------------------------------

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {

	## Initializes the inverse property
    i <- NULL

	## Method to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    	## Method to get the matrix
    get <- function() {
    	## Returns the matrix
    	m
    }

    	## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    	## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Returns the inverse property
        i
    }

    	## Returns a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated, 
## then cacheSolve retrieves it from the cache.

cacheSolve <- function(x, ...) {

    ## Returns a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Just returns the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Gets the matrix from our object
    data <- x$get()

    ## Calculates the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Sets the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}

