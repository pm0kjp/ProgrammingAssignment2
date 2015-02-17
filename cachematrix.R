
## This series of functions uses lexical scoping to permit object caching
## so that we can set a matrix, find its inverse, forget about it, then
## come back and re-request its inverse without having to re-calculate.
## Saves CPU and memory!

## makeCacheMatrix sets up not only the actual matrix upon which calculations
## (including but not limited to inverse calculation) can be made, but it
## creates the ecosystem of getters and setters for the matrix itself and for 
## the inverse.  

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # we initialize with the inverse being null, since we've not calculated yet.
  set <- function(y) {
    x <<- y       # set the matrix to whatever was specified
    inv <<- NULL  # also, when we set the source matrix, we null out the calculated inverse,
                  # since it's no longer valid (was computed on old matrix or was already
                  # null anyhow since this was the first use)
  }
  get <- function() x                           # return the matrix
  setinv <- function(solve) inv <<- solve       # set the inverse
  getinv <- function() inv                      # return the inverse
  list(set = set, get = get,    # these allow us to access object methods,
       setinv = setinv,         # e.g. my_vector$setinv
       getinv = getinv)
}


## cacheSolve takes the object created by makeCacheMatrix and finds or computes
## the inverse of the matrix.  If it computes it, it also stores it for easy
## delivery next time.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()                 # first, grab what may already have been set!
  if(!is.null(inv)) {               # fantastic, it's not null, so we can use it.
    message("getting cached data")  # let user know what we're doing
    return(inv)                     # return the inverse and we're done.
  }
  data <- x$get()                   # no stored inverse, so take the matrix,
  inv <- solve(data, ...)           # calculate the inverse,
  x$setinv(inv)                     # store it for later,
  inv                               # return the inverse and we're done.
}
