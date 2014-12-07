## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL   #i stores inverse if set
    
    set <- function(y) {
            x <<- y
            i <<- NULL
    }
    get <- function() x
    
    setinv <- function(inv) i <<- inv ##store inverse in i
    getinv <- function() i
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x
    
    i <- x$getinv()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }   ##if inverse exist return inverse
    
    ##else calc inverse and store into cache matrix
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}
