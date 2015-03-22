## These functions will cache the value of the inverse of a matrix
## so that if the value of the inverse is needed again it can be 
## looked up in the cache instead of being computed again.

## makeCacheMatrix is a function that will create a "matrix" that
## list containing a function to set the value of the matrix, get
## the value of the matrix, set the value of the inverse, and get
## the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculated the inverse of the matrix created in
## makeCacheMatrix. If the inverse of the matrix is already calculated
## it gets it from the cache and does not perform the calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
