# makeCacheMatrix returns a list of 4 functions:
# set, get, setinverse, getinverse
# to store a matrix and its inverse
# Setting the matrix clears the inverse
# If the inverse has not been set, getinverse returns NULL
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve returns the inverse of a matrix
# Input argument x should be a cached matrix as return by makeCacheMatrix
# cacheSolve stores the inverse in the cached matrix:
#     * the inverse is returned from the cache in cache matrix if present;
#     * the inverse is cacl;ulated and added to the cache if not preiously present
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("retrieving cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
