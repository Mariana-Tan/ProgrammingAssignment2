## My functions cache the inverse of a matrix.


## makeCacheMatrix(x): 

## Creates a special "matrix" object that can cache its inverse,
## which is really a list containing functions to:
##      1. set: set the value of the matrix
##      2. get: get the value of the matrix
##      3. setinv: set the value of the inverse
##      4. getinv: get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # initialize inverse
    set <- function(y) { # set matrix
        x <<- y
        i <<- NULL
    }
    get <- function() x # get matrix
    setinv <- function(inv) i <<- inv # set inverse
    getinv <- function() i # get inverse
    list(set = set, 
         get = get, 
         setinv = setinv, 
         getinv = getinv) # return a list
}


## cacheSolve(x, ...): 

## Returns a matrix that is the inverse of 'x', 
## where 'x' is a special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## cached
    i <- x$getinv() # get inverse
    if(!is.null(i)) { # if inverse is cached
        message("getting cached data")
        return(i) # return inverse
    }
    ## not cached
    data <- x$get() # get matrix
    i <- solve(data) # solve inverse
    x$setinv(i) # set inverse
    i # return inverse
}
