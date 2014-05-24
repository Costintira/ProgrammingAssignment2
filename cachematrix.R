#The function makeCacheMatrix creates a list containing a function to
        #1. set the value of the matrix
        #2. get the value of the matrix
        #3. set the value of the inverse matrix and write to cache with <<-
        #4. get the value of the inverse matrix from cache


makeCacheMatrix <- function(x = matrix()) {
        #the makeCacheMatrix function instantiates x as matrix and creates empty vector inv
        inv <- NULL
        
        #the set function takes the input, assigns it to x and clears the cache
        #by setting inv<<-NULL with the superassignment operator <<-
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        #the get functions gets the matrix that will be inverted and returns it
        get <- function() x
        
        #the setcacheinverse function assigns to inv variable the solution of solve()
        setcache.inverse <- function(solve) inv <<- solve
        
        # the getcache.inverse function returns the cashed inverse of the x matrix
        getcache.inverse <- function() inv
        
        #returns a list of functions available in "makeCacheMatrix"
        list(set = set, get = get,
             setcache.inverse = setcache.inverse,
             getcache.inverse = getcache.inverse)
        
}


#The following function calculates the mean of the matrix created with
#the makeCacheMatrix function. 
#First it checks to see if the inverse has already been calculated.
#If YES gets it from the cache and skips the computation
#else it calculates the inverse of the matrix and sets it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getcache.inverse()
        
        #if the inverse is already computed and stored in cache it will retrieve it from cache
        #and inform us about it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        #if the inverse is not stored in cache it gets the matrix, 
        #calculates the inverse and stores it in cache
        data <- x$get()
        inv <- solve(data)
        x$setcache.inverse(inv)
        
        #returns the inverse of the initial matrix
        inv
}
