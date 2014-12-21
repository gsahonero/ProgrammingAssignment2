## The following functions are designed to create a matrix object which
## can store its value and its inverse. The values of the matrix can 
## also be modified.


## This function creates the matrix object. The following actions can be 
## performed by calling the right function considering the object as a list:
## x$set(), set the matrix.
## x$get(), gets the value of the matrix.
## x$setR(), set the reverse of the matrix.
## x$getR(), get the reverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setR <- function(solve) m <<- solve
    getR <- function() m
    list(set = set, get = get,
         setR = setR,
         getR = getR)
}


## This function computes the inverse of the matrix x.
## If the inverse has already being computed, the matrix returns the cached copy.
## Otherwise, it computes the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getR()
    if(!is.null(m)) {
        ## A cached inverse has been found. Returning the cached data:
        message("getting cached data")
        return(m)
    }
    ## Getting the matrix
    data <- x$get()
    # Computing the inverse:
    m <- solve(data, ...)
    ## Assigning the inverse to the matrix object argument
    x$setR(m)
    m
}
