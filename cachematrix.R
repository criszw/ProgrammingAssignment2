## By using the <<- operator, the following functions cache the inverse of a matrix

## Creates "special" matrix, which is really a list of containing fucntions to:
## set the matrix
## get the matrix
## set the inverse matrix
## get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    m=NULL
    set<-function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) {
        m <<- inv
    }
    getinv <- function() m
    cbind(set, get, setinv, getinv)
}


## Searches for an inverse, if it can't find one then it calculates it
## Returns inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
