## Below are two functions that are used to create a special object that 
## stores a matrix and caches its inverse

## This function creates a special “matrix” to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function computes the inverse of the special “matrix” created 
## with the function above. It first checks to see if the inverse has already
## been calculated. If so, then it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
