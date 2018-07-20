# makeCacheMatrix is a functon to set the value of a matrix,
# get the value of the matrix, set the value of the inverse 
# and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    matinv <- NULL
    set <- function(y) {
        x <<- y
        matinv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) matinv <<- inv
    getinv <- function() matinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


# cacheSolve is a function that calculates the inverse of
# a matrix. If the inverse has been calculated, the computation
# is skipped. Otherwise, it calculates the inverse and sets it
# in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matinv <- x$getinv()
    if(!is.null(matinv)) {
      message("getting cached data")
      return(matinv)
    }
    data <- x$get()
    matinv <- solve(data)
    x$setinv(matinv)
    matinv
}
