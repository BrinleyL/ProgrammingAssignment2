##a pair of functions that cache the inverse of a matrix

##The first function creates a special "matrix", 
##which is really a list containing a function to 
##set/get value of matrix,set/get value of inversed matrix


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##The following function calculates the inversed matrix of the special "matrix". 
##It first checks to see if the inversed matrix has already been calculated. 
##If so, it gets the inversed matrix from the cache and skips the computation. 
##Otherwise, it calculates the inversed matrix and sets the value of it in the cache via the setinverse function.
## Return a matrix that is the inverse of 'x'


cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
