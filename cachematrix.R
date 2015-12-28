## This function gets and sets the input for the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  
  
  invr = NULL
  set = function(y) {
    x <<- y
    invr <<- NULL
  }
  get = function() x
  setinv = function(inverse) invr <<- inverse 
  getinv = function() invr
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## This function inverses the matrix with the cache functionality

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invr = x$getinv()
  
  if (!is.null(invr)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(invr)
  }
  
  mat.data = x$get()
  invr = solve(mat.data, ...)
  
  x$setinv(invr)
  
  return(invr)
}
