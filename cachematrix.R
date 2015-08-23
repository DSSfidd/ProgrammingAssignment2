## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a list containing data matrix and its cached inverse
# furthermore the list contains setinv and getinv functions which
# set and return the calculated inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# This function checks whether the inverse of a matrix has already been solved
# in case it has not, it calculates the inverse and caches it
# finally the inverse is returned

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}