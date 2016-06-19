## The following pair of functions is used to cache the inverse of
## a matrix in order to avoid the repeatitive computation of the 
## inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of the inverse
## (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inv<<-NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <-function() inv
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## cacheSolve calculates the inverse of the matrix. However, it first
## checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setmean(inv)
        inv
}