## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(input = matrix()) {

  inv <- NULL
  set <- function(y) {
    input <<- y
    inv <<- NULL
  }
  get <- function() input
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# This function returns the inverse of the matrix. To compute the inverse It checks if
# the inverse has already been computed. If computation has already been done then, it gives the result and doesn't do the
# computation. else it computes the inverse, sets the value in the cache with the help of 
# setinverse function.

cacheSolve <- function(input_matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- input_matrix$getinverse()
  if(!is.null(inverse)) {
    message("giving cached matrix.")
    return(inv)
  }
  data <- input_matrix$get()
  inverse <- solve(data)
  input_matrix$setinverse(inv)
  inverse
}
