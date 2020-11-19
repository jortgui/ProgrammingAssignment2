## Jordi Ortiga
##
## Put comments here that give an overall description of what your functions do
##
## This script contains two functions:
##   1 - makeCacheMatrix 
##   2 - cacheSolve
##
## The objective of those function is to be able to store a matrix and to store
## also the inverse of that matrix. In a way that only is necessary to recalculate
## the inverse of the matrix when the matrix is changed by the user. While the 
## matrix remains unchanged, we will be able to get the inverse matrix without
## the necessity to recalculate, saving time.
##
## Description of functions:
##   - makeCacheMatrix: used to set and retrieve the stored values of a matrix
##     and the inverse of that matrix.
##     Example of use: v <- makeCacheMatrix(a) where a is an invertible matrix.
##     The enclosed environment in the returned object by makeCacheMatrix has 
##     two variables:
##     - x: stores the matrix
##     - x.inv: stores the inverse of x
##     The returned object by makeCacheMatrix has 4 functions, following the example:
##     - v$get() returns the value of the stored matrix (in x). 
##     - v$set(a) where a is an invertible matrix. To be used only to change the
##       stored matrix (in x).
##     - v$getinv() returns the value of the stored inverse matrix (in x.inv).
##     - v$setinv(a.inv) where a.inv is the inverse of the matrix. Do not use
##       this function, this function is used by cacheSolve function.
##     IMPORTANT: after the following commands...
##     - v <- makeCacheMatrix(a)
##     - v$set(a)
##     ...the value of the inverse matrix (x.inv) is reset and v$getinv() will
##     return NULL. It is necessary to call cacheSolve(v) to calculate and 
##     store the inverse matrix.
##
##   - cacheSolve: used to calculate the inverse of the matrix using the returned 
##       object by makeCacheMatrix.
##       Example of use: cacheSolve(v) where v is the retuned object by makeCacheMatrix.
##       cacheSolve() will calculate and set the value for the inverse matrix only
##       if has not been calculated previously.
##       In any case, cacheSolve(v) returns the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    #x:     has the matrix
    #x.inv: has the inverse of x
    
    x.inv <- NULL
    set <- function(y) {
        x <<- y          # <<- to set x value in the enclosed environment by makeCacheMatrix 
        x.inv <<- NULL   # <<- to set x.inv value in the enclosed environment by makeCacheMatrix
    }
    
    get <- function() x
    setinv <- function(inv) x.inv <<- inv # <<- to set x.inv value in the enclosed environment by makeCacheMatrix
    getinv <- function() x.inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    x.inv <- x$getinv() #get inverse matrix
    if(!is.null(x.inv)) {
        message("getting cached data")
        return(x.inv) #END of function if x.inv already set
    }
    
    #calculate and set inverse matrix, only if x.inv is NULL
    data <- x$get()
    x.inv <- solve(data, ...)
    x$setinv(x.inv)
    x.inv    
}
