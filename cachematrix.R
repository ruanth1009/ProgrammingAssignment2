## Put comments here that give an overall description of what your
## functions do

# An advanced version of computing matrix inversion. Its efficiency will be promoted by
# the following step:
# Step1: Storing a set of matrices and their inversions using "makeCacheMatrix".
# Step2: Whenever we want to compute the inversion of certain matrix, we just have to look up
# in our previous storage. Only if we cannot find its inversion will we start to compute.

## Write a short comment describing this function

# Step1: Storing a set of matrices and their inversions using "makeCacheMatrix".
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv_result) m <<- inv_result
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function

# Step2: Whenever we want to compute the inversion of certain matrix, we just have to look up
# in our previous storage. Only if we cannot find its inversion will we start to compute.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
