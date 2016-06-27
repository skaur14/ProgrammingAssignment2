##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
##Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

##The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse, which is really a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        in <- NULL
        set <- function(y) {
                x <<- y
                in <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) in <<- inverse
        getinverse <- function() in
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##The following function calculates the inverse of the special "matrix" created with the above function.
##It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        in <- x$getinverse()
        if(!is.null(in)) {
                message("getting cached data")
                return(in)
        }
        data <- x$get()
        in <- solve(data, ...)
        x$setinverse(in)
        in
}

