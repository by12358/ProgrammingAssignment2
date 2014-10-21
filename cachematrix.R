## Matrix inversion is usually a costly computation.  The following
## two function can be used to create a "cached matrix" object which
## stores the inverse of a matrix for future reference after it has 
## been calculated once.  Changing the matrix results in the inverse
## needed to be recalculated.

## Name:      makeCacheMatrix
## Arguments: a matrix
## Returns:   a cached matrix object, returned in the form of a list
## Usage:     use to create an object corresponding to a matrix that will 
##            store the inverse of a matrix for future reference, saving
##            repeated calls to solve(), which can be computationally costly.
makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL  ## set the inverse matrix to NULL when first created
    set <- function(y) {
        x <<- y
        x_inv <<- NULL  ## if the matrix changes, reset the inverse to NULL
    }
    get <- function() x
    setinv <- function(inv) x_inv <<- inv
    getinv <- function() x_inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Name:      cacheSolve
## Arguments: a cached matrix object created from makeCacheMatrix
## Returns:   a matrix corresponding to the inverse of the matrix stored
##            in the cache matrix objected create from makeCacheMatrix
## Usage:     use cacheSolve to retrieve the inverse of a matrix.  If the
##            inverse has already been calculated, the cached version will
##            be returned.  If not, it will be calculated and returned.
cacheSolve <- function(x) {
    inv <- x$getinv()
    if(!is.null(inv)) {   ## if the inverse has already been calculated...
        message("getting cached data")
        return(inv)       ## ...return it, and stop
    }
    data <- x$get()       ## retrieve the original matrix from the cached
                          ## matrix object
    inv <- solve(data)    ## calculate the inverse of our original matrix
    x$setinv(inv)         ## cache the inverse for future use
    inv                   ## return the newly calculated inverse matrix
}
