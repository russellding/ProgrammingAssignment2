## This function makeCacheMatrix creates a special matrix, which is really a list containing a function to:
##1.Set the value of the matrix
##2.Get the value of the matrix
##3.Set the inverse of the matrix
##4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##The following function calculates the inverse of the special matrix created with the above function.
##However, it first checks to see if the inverse has already been calculated.
##If so, it gets the inverse matrix from the cache and skips the computation.
##Otherwise, it calculates the inverse and assign the inverse matrix to "inv" via setinverse function.


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
