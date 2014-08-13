## ProgrammingAssignment2 for Coursera course `R Programming` by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD


# makeCacheMatrix creates a special "matrix" object that maintains a cache its inverse.
# Arguments:  x   a matrix
# The returned object is represented by a list of accessor functions to get and set the matrix and cached inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    #changing the matrix clears the cache
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inv <<- i
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


# cacheSolve computes the inverse of the special "matrix" returned by `makeCacheMatrix`. 
# If the inverse has already been calculated (and the matrix has not changed), then
# cacheSolve will retrieve the inverse from the cache.
# Arguments:  x   a wrapped matrix created by makeCacheMatrix
#                 additional arguments will be passed to solve()
# Value: returns the inverse of the matrix
cacheSolve <- function(x, ...) {
  #try cache first
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #not found in cache
  data <- x$get()
  m <- solve(data, ...)
  #cache result
  x$setInverse(m)
  m
}
