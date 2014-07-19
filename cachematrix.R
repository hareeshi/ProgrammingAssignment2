## The below pair of functions cache the inverse of a matrix
## usage > cacheSolve(makeCacheMatrix(your matrix name))

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL                  ## initializing the inverse matrix
    setinv <- function(imatrix) {       ## This function sets the inverse matrix
        inv_matrix <<- imatrix
    } 
    getinv <- function() inv_matrix     ## return the inverse matrix
    get <- function() x                 ## returns the matrix
    list(get = get,                     ## returning list of functions
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv_matrix <- x$getinv()         ## retrieve the cache
    if (!is.null(inv_matrix)){       ## if available, return the cache value
        message ("getting cached data")
        return (inv_matrix)
    }
    data_matrix <- x$get()           ## if no cache, get the matrix
    inv_matrix <- solve(data_matrix) ## calculate the inverse
    x$setinv(inv_matrix)             ## set the cache value
    inv_matrix                       ## return the inverse matrix
}
