## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## Now I've writen a pair of functions that cache the inverse of a matrix


##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get<- function() x
        setinver <- function(solve) m <<- solve
        getinver <- function() m
        list(set = set, get = get, setinver= setinver, getinver = getinver)
}


##  This function creates a special "matrix" object that can cache its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinver()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinver(m)
        m
}
