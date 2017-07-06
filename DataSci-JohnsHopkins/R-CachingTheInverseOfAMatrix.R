# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

# Write the following functions:
#   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#   cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

# Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# For this assignment, assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...){
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

B <- matrix( c(2, 4, 3, 1, 5, 7, 1, 2, 3),  nrow=3, ncol=3) 
B_inverse <- solve(B)
xm <- makeCacheMatrix(B)
xm_1 <- cacheSolve(xm)
xm_2 <- cacheSolve(xm)
