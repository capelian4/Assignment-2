## makeCacheMatrix function creates a matrix that can cache its inverse.
## It sets the value of the matrix, gets the value of a matrix, sets the value of an inverse of the matrix, and gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i = NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
                }
        get <- function() x
        setinverse <- function(inverse)
                i <<- inverse
        getinverse <- function()
                i
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse)
        }

## The cacheSolve() function like the solve() function calculates the inverse of a matrix.
##cacheSolve() retrieves already calculated matrix inverses using previous computations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("retriving cached data")
                return(i)
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
}

        
