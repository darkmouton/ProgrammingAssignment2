##These function are used to check the cache for a cached inversed matrix, 
##and if not present, solve it and commit to the cache

## makeCacheMatrix create a list with all the necessary functions (get and set the matrix, 
##and get and set into the cache the inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL

        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve check the cache for an already calculated inverse, and calculate it
## if not already present.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
