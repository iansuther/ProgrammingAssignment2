## this package contains two functions

## 1. makeCacheMatrix :- creates an object / place (essentially a list) that holds the matrix
##                       and inverse of that matrix object. on first accepting the matrix, the
##                       inverse is set to null 

makeCacheMatrix <- function(x = matrix()) {
    # initialize the variable that holds the inverse of the matrix entered
      inv_mtx <- NULL
    
    # the following function will store the matrix and resets the inverse
      set_mtx <- function(y) {
          x <<- y
          inv_mtx <<- NULL
      }

    # the following function will return the matrix as entered
      get_mtx <- function() x
      
    # the following function will set the inverse of the matrix
      set_inv <- function(inv) inv_mtx <<- inv

    # the following function will return the inverse of the matrix
      get_inv <- function() inv_mtx

    # the output of this function is 
     list(set_mtx = set_mtx, get_mtx = get_mtx,
             set_inv = set_inv,
             get_inv = get_inv)
}


## 2. cacheSolve :- produces the inverse of a matrix, which is handled / returned by makeCacheMatrix
##                  it checks if the inverse exists and if is does not it is then computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    # run the function to get the inverse
    inv_mtx <- x$get_inv()

    # check if the inverse matrix existed (was cached) or not
    # if it is not null (i.e. it exists) then return it
    if(!is.null(inv_mtx)) {
                message("getting cached inverse matrix")
                return(inv_mtx)
    }
 
    # this part of the function happens if the inverse matrix does not exist (i.e. was not cached)

    # first the matrix is retrieved
    mtx_data <- x$get_mtx()

    # then the inverse it calculated
    inv_mtx <- solve(mtx_data, ...)
 
    # set the inverse and the return (output) it
    x$set_inv(inv_mtx)
    inv_mtx

}
