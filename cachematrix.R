## Function is used to create an object to store a matrix and 
##          to cache its iverse 
## args: x - a square invertible matrix
## returns: list containing functions to: set the matrix, get the
##          matrix, set the matrix's inverse, get the matrix's inverse 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Function calculates and sets the inverse of the matrix or takes 
##          it from the cache (if available).
## args: x - a vector of type makeCacheMatrix
## returns: inverse of the matrix

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
