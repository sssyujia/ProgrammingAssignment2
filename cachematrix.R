## A pair of functions to cache the inverse of a matrix

## makeCacheMatrix creates a matrix

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


## cacheSolve calculates the inverse of matrix created. 
## If the inverse has already been calculated, it will gets the inverse and skips the computation.

cacheSolve <- function(x=matrix(), ...) {
         m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

