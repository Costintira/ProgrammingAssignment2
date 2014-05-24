## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setcache.inverse <- function(solve) inv <<- solve
        getcache.inverse <- function() inv
        list(set = set, get = get,
             setcache.inverse = setcache.inverse,
             getcache.inverse = getcache.inverse)
        
        

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getcache.inverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setcache.inverse(inv)
        inv
}
