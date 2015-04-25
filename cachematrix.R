## Cacheable matrix inverse
#
## makeCacheMatrix to create a matrix whose inverse can be cached
#
## cacheSolve on a cacheable matrix will return its inverse by first checking
## checking in the cache if already computed 

## Function to return a cacheable matrix
## Use $set to set the matrix to a new value
## Use $get to get the embedded matrix
makeCacheMatrix <- function(x = matrix()) {
    mtxInverse <- NULL
    set <- function(m) {
        mtxInverse <<- NULL # Matrix changed => Clear the inverse
        x <<- m # Store the new matrix
    }
    get <- function() x # Get the matrix
    setInverse <- function(inv) mtxInverse <<- inv # Set the matrix inverse
    getInverse <- function() mtxInverse # Get the matrix inverse
    #Returns the list with getter and setter
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Get the inverse of a cacheable matrix created with makeCacheMatrix
## Use cache value if available
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv = x$getInverse()
    # If cache is available, return it
    if (!is.null(inv)) {
        message("Returning cached inverse")
        return(inv)
    }
    # Cache not available
    mtx = x$get() # Get the matrix
    inv = solve(mtx, ...) # Compute the inverse
    x$setInverse(inv) # Set it in the cacheable matrix
    inv # Return it
}

##Testing
# mt = matrix(1:4, nrow = 3, ncol = 3)
# cacheableMTX = makeCacheMatrix(mt)
# cacheableMTX$get()
# cacheSolve(cacheableMTX) # Compute
# cacheSolve(cacheableMTX) # From cache
