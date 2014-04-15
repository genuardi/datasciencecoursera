## Modifcation of the example functions to produce two new functions
## that solve a matrix (or find a previously solved value using cache)

## makeCacheMatrix: creates a special "matrix" object that can cache its 
##   inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: computes the inverse of the special "matrix" returned by
##   makeCacheMatrix. If the inverse has already been calculated (and the
##   matrix has not changed), then the cachesolve should retrieve the inverse
##   from the cache.
cacheSolve <- function(x, ...) {
        ## Check to see if the computation is already done (i.e. if i exsists)
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## If i doesn't already exsist, then compute and return it
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}