# The functions serve to create an object with matrix and cache for its inverse matrix and 
# for solving and retrieving the inverse matrix from that object

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    haschanged <- FALSE
    set <- function(y) {
        x <<- y
        inverse <<- NULL
        haschanged <<- TRUE
    }
    get <- function() x
    setinverse <- function(inversematrix) {
        inverse <<- inversematrix
        haschanged <<- FALSE
    }
    getinverse <- function() inverse
    gethaschanged <- function() haschanged
    list(
        set = set, 
        get = get, 
        setinverse = setinverse, 
        getinverse = getinverse, 
        gethaschanged = gethaschanged)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inversematrix <- x$getinverse()
    if(is.null(inversematrix) || x$gethaschanged()) {        
        data <- x$get()
        inversematrix <- solve(data, ...)
        x$setinverse(inversematrix)
        inversematrix
    } else {
        message("getting cached data")
        inversematrix    
    }  
}
