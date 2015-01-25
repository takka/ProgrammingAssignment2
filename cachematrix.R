## This file contains two functions (makeCacheMatrix and cacheSolve) 
## for creating a special list object from a given matrix object
## and storing the cached value of inversed matrix

## Function makeCacheMatrix() gets a matrix object and returns a list object that contains 
## functions for getting and setting original matrix object
## and getting and setting the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
	## init, set null value for inversed matrix
		 s <- NULL
		 ## set value 'y'(from 'set' environment) for x from 'makeCacheMatrix' environment 
		 ## and set null for s from 'makeCacheMatrix' env
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        ## get the matrix's value for this 'makeCacheMatrix' object
        get <- function() x
        ## set the value of s from 'makeCacheMatrix' object
        setsolve<- function(solve) s <<- solve
        ## function getsolve() gets the value of s
        getsolve <- function() s
        ## returning list object with all previous functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## cacheSolve function pass as parameters makeCacheMatrix obj (and the other params that can be passed to solve function)
## and it returns the inversed matrix 
## but with cached inversed matrix 

cacheSolve <- function(x, ...) {
        ## get inversed value from passed as a param makeCacheMatrix obj
        s <- x$getsolve()
        ## checking if the inversed value has been allready calculated 
        if(!is.null(s)) {
                message("getting cached data")
                ## return the matrix that has been calculated before and stored in makeCacheMatrix obj
                return(s)
        }
        ## in other case
        ## get the original data matrix from makeCacheMatrix obj
        data <- x$get()
        ## get the inversed value of data matrix
        s <- solve(data, ...)
        ## set the inversed value in makeCacheMatrix obj
        x$setsolve(s)
        ## Return a matrix that is the inverse of 'x'
        s

}
