## Subject: Coursera, R Programming - Assignment 2
## Date: 24/8/2014
## Description:
## The following functions manipulating the benefits of the scoping rules 
## of the R language by creating a matrix object and caching its inverse.

## This function creates "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) { 
                x <<- y
                i <<- NULL
        }
        get <- function() x
        ## caching the inverse
        setinv <- function(inv) i <<- inv # set the value of the inverse
        getinv <- function() i            # get the value of the inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function computes an inverse of the a matrix
cacheSolve <- function(x, ...) {
        i <- x$getinv() 
        if(!is.null(i)) {
                message("cache retrieval") ## getting the cached data
                return(i)  ## Return a matrix that is the inverse of 'x'
        }
        data <- x$get()
        library(MASS) ## loading the MASS package is required for ginv()
        i <- ginv(data) ## Moore-Penrose Generalized Inverse
        x$setinv(i)
        i
}
