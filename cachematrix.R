## Create a special `matrix` object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinvr <- function(solve) m <<- solve
    getinvr <- function() m
    
    list(set=set, get=get, setinvr=setinvr, getinvr=getinvr)
}


## Compute the inverse of the special `matrix`
## If the inverse has already been calculated and `matrix` unchanged 
## then retrieve from cache
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    m <- x$getinvr()
    
    if (!is.null(m)) {
        message("Retrieving cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    
    x$setinvr(m)
    
    m
}