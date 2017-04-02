## Put comments here that give an overall description of what your
## functions do

## Function that returns a list of functions to get and set the matrix and to get and set its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(matr){
    x <<- matr
    inv_mat <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv_mat <<- inverse
  getinv <- function() inv_mat
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getinv()
  if(!is.null(inv_mat)){
    message("getting cached data")
    return(inv_mat)
  }
  matr_data <- x$get()
  inv_mat <- solve(matr_data, ...)
  x$setinv(inv_mat)
  inv_mat
}

