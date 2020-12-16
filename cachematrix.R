## These two functions caches into memory the inverse of a matrix.
## makeCacheMatrix creates a special "matrix" object
## cacheSolve computes the inverse of the matrix from makeCacheMatrix. 
## This first checks whether the inverse is already calculated, if it is,
## then cacheSolve retrieves the inverse from the cache otherwise, it calculates the inverse of that matrix.

## set() stores the input matrix
## get() retrieves the input matrix from the memory
## setinv() stores the inverse of that matrix
## getinv() retrieves the inverse of that matrix from the memory.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}


## Checks if already computed using getinv()
## Retrieves inv from cache if it is already computed otherwise it retrieves the matrix using get() and
## computes the inverse of that matrix using solve() then stores the inverse into setinv() then the whole 
## function returns the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } else {
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    return(inv)
  }
}
