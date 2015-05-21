## The functions makeCacheMatrix and cacheSolve can be used to create a wrapper object around a matrix that caches its inverse
## To create the wrapper object call makeCacheMatrix with the matrix as an argument
## Use w$get() to get the original matrix
## Use cacheSolve(w) to get the inverse of the matrix
## When calling repeatedly cacheSolve on the returned wrapper object, the inverse will be only computed the first time.
## Subsequent calls to cacheSolve will return the cached value.

## Creates a wrapper object for matrix x capable of caching the inverse of x

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # Initialize the inverse to an empty value
    
    # getter/setter for the original matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    
    # getter/setter for the inverse of the matrix
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    
    # Constructs a list containing the getter/setter functions using a closure
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns the inverse of matrix stored in x. The inverse of x is computed only during the first invocation.
## Subsequence invocations will return its cached value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # If the cached value of the inverse is not empty (NULL), return it
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        # Return the cached inverse
        return(i)
    }

    # Otherwise compute the inverse of the matrix store in x
    data <- x$get()
    i <- solve(data, ...)
    # Store the result in the cache
    x$setinv(i)
    # Return the computed inverse
    i
}
