## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  matrixsolved <- NULL
  set <- function(y) {
    x <<- y
    matrixsolved <<- NULL
  }
  get <- function() x
  setsolved <- function(solved) matrixsolved <<- solved
  getsolved <- function() matrixsolved
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixsolved <- x$getsolved()
  if(!is.null(matrixsolved)) {
    message("getting cached data")
    return(matrixsolved)
  } else if (dim(x$get())[1] != dim(x$get())[2]) {
    message("Matrix not inversible because not a square matrix!") 
  }
  else if (det(x$get()) == 0) {
    message("Matrix not inversible because determinant is equal to zero!") 
  }
  else {
    data <- x$get()
    matrixsolved <- solve(data, ...)
    x$setsolved(matrixsolved)
    matrixsolved
  }
}