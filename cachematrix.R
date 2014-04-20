## makeCacheMatrix : Caches a symmetric matrix and its inverse.
## Argument: x = a symmetric matrix whose inverse is to be cached.
## Returns:  A list of functions (get, set, getInverse, setInverse).

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                mat <<- y
                inv_mat <<- NULL
        }
        get <- function() mat
        setInverse <- function(inv = matrix()) inv_mat <<- inv
        getInverse <- function() inv_mat

	set(x)

        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve:	Returns the inverse of a symmetric matrix from cache if cache
##		is not empty.
##		Otherwise, computes the inverse of a symmetric matrix, saves it
##		in cache and returns the inverse matrix.
# Argument: x = an object created with makeCacheMatrix().
# Returns:  Inverse of a matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat  <- x$getInverse()
        if(!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        a_mat <- x$get()
        inv_mat <- solve(a_mat, ...)
        x$setInverse(inv_mat)
        inv_mat
}
