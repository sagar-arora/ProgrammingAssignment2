## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(q = matrix()) {
  ## create a matrix object q and some associated sub-functions/methods
  
  ## define the cache p
  p <- NULL
  set <- function(input) {
    q <<- input ## assign the input matrix input to the variable q in the
    ## parent environment
    ## 
    
    p <<- NULL ## re-initialize m in the parent environment to null
  }
  get <- function() q
  setinverse <- function(inverse) p <<- inverse
  getinverse <- function() p
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# This function returns the inverse of the matrix. To compute the inverse It checks if
# the inverse has already been computed. If computation has already been done then, it gives the result and doesn't do the
# computation. else it computes the inverse, sets the value in the cache with the help of 
# setinverse function.

cacheSolve <- function(input_matrix, ...) {
        ## Return a matrix that is the inverse of 'input_matrix'
  inverse <- input_matrix$getinverse()
  if(!is.null(inverse)) {
    message("giving cached matrix.")
    return(inverse)
  }
  data <- input_matrix$get()
  inverse <- solve(data)
  input_matrix$setinverse(inverse)
  inverse
}
