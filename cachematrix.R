## function designed to store a 2 by 2 matrix and solve its inverse
## makeCacheMatrix is a repository as a list that stores the 
## inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve ##diff name?
  getinv <- function() inv
  ## list of matrices
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## solveCache retruns the inverse assuming it is solveable
## there is a check within the function to see if the solution is in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
