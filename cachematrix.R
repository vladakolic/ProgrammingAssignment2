## The following two functions create a matrix that can cache its inverse
## value to speed up future calculations, and calculate or retrieve that
## inverse value respectivly. Use wisely.

## Creates a matrix whose inverse can be cached to avoid the need for
## computing the inverse every time cacheSolve is called.
makeCacheMatrix <- function(x = matrix()) {
    
    # initialize the cached inverse value of the makeCacheMatrix to NULL.
    inv <- NULL
    
    # set the current makeCacheMatrix object to a new value.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # returns the matrix contents of the makeCacheMatrix object
    get <- function() x
    
    # cache a new inverse value
    setInverse <- function(inverse) inv <<- inverse
    
    # returns the cached inverse of the makeCacheMatrix object
    getInverse <- function() inv
 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns the inverse of the makeCacheMatrix x. If the inverse has
## already been computed, the cached value is returned.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # get the cached inverse of x
    inv <- x$getInverse()
    if (!is.null(inv)) {
        print("[insert joke here] to confirm that cached inverse value was returned.")
        return(inv)
    }
    
    # ugly-ass code to calculate inverse, cache it and then return it.
    mat <- x$get()
    inv <- solve(mat, ...) # unclear: set argument b to be identity matrix before adding '...' argument?
    x$setInverse(inv)
    inv
}
