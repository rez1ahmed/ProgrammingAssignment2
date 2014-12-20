################################################################################
## File: cachematrix.R
## 
## This file contains the following two functions:
##
## 1. makeCacheMatrix: This function creates a special "matrix" object that can 
##                     cache its inverse.
##
## 2. cacheSolve:      This function computes the inverse of the special 
##                     "matrix" returned by makeCacheMatrix() function described
##                     above. If the inverse has already been calculated (and 
##                     the matrix has not changed), then cacheSolve should 
##                     retrieve the inverse from the cache.
################################################################################

################################################################################
## Function: makeCacheMatrix
##
## This function creates a special "matrix" object that can cache its inverse.
## Initially, the inverse of the matrix is NULL. We also set the calculated
## inverse value to NULL if the matrix changes due to "set" call. 
##
## Usage:
## y <- makeCacheMatrix(matrix(......)) 
## 
## y$get(): Returns the matrix.
## y$set(Z): Sets the matrix to z.
## y$getInverse(): Returns the inverse of the matrix y.
## y$setInverse(): Sets the inverse of the matrix y.
##
################################################################################
makeCacheMatrix <- function(x = matrix()) {
    ## Initializing the inverse of matrix x to be NULL
    x_inv <- NULL
    
    ## Defining a function "set" to allows users update the matrix x
    set <- function(y) {
        ## Setting matrix x to be y
        x <<- y
        
        ## Initializing the inverse of the new matrix x to be NULL
        x_inv <<- NULL
    }
    
    ## Defining a function "get" to allow users access the matrix x
    get <- function() x
    
    ## Defining a function "setInverse" that allows users to directly
    ## set the inverse of matrix x
    setInverse <- function(inv) x_inv <<- inv
    
    ## Defining a function "getInverse" to allow users access the inverse of
    ## the matrix x
    getInverse <- function() x_inv
    
    ## Constructing the special matrix object as a list, that allows users
    ## can cache its inverse of a matrix
    list (set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


################################################################################
## Function: cacheSolve
##
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix() function described above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.
################################################################################
cacheSolve <- function(x, ...) {
    ## First checking whether inverse of x is present or not
    x_inv <- x$getInverse()
    
    ## If the inverse is already present (not NULL), 
    ## return the cached value
    if (!is.null(x_inv)){
        ## Notify user that we are using cached data
        message("Getting cached data")
        
        ## Return the inverse of matrix x
        return (x_inv)
    }    
    
    ## When inverse of matrix x is not present, we will compute it
    ## Accessing matrix x and storing it as data
    data <- x$get()
    
    ## Using the solve() function to retrieve inverse of matrix x
    x_inv <- solve(data, ...)
    
    ## Storing inverse of matrix x for future usage
    x$setInverse(x_inv)
    
    ## return inverse of matrix x
    x_inv
}
