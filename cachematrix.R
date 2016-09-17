## The following functions calculate and cache the value of the inverse of a 
## square matrix, "x", that is provided as an argument to makeCacheMatrix

## makeCacheMatrix creates a list containing functions to 1) set 
## and 2) get the value of a matrix and 3) set and 4) get the value of the 
## solved matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solvedMatrix) m <<- solvedMatrix
        getSolve <- function() m
        list(set=set, get=get, setSolve=setSolve, getSolve=getSolve)
}


## cacheSolve takes a list output of makeCacheMatrix as an argument and returns 
## the inverse of the matrix assigned to x in makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getSolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setSolve(m)
        m
}
