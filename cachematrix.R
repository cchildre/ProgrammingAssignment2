## Functions makeCacheMatrix and cacheSolve cache the inverse of the matrix, x
## Use makeCacheMatrix on the invertible matrix, followed by cacheSolve on the 
## list created

## makeCacheMatrix creates a list containing a function to set and get 
## the value of the matrix along with the corresponding inverse

### x is an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        x.inv <- NULL
        set <- function(y) {
                x <<- y
                x.inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) x.inv <<- solve
        getinv <- function() x.inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve calculates the inverse of the above function, first checking 
## the cache for the inverse of the matrix rather than recalculating

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        x.inv <- x$getinv()
        if(!is.null(x.inv)) { # if mean is present in cache
                message("getting cached data")
                return(x.inv)
        }
        data <- x$get()
        x.inv <- solve(data, ...)
        x$setinv(x.inv)
        return(x.inv)
}
