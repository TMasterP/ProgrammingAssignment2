## Initializes two objects, x and inverse
## Four behaviors, "getters and setters", retrieve and set data within the object
## the <<- operator which assigns the value on the right side of the operator to an object in the parent environment named 
##     by the object on the left side of the operator
## Assigning the value of NULL clears any value of the object that had been cached by a prior execution
## Naming the list elements allows the use of $ instead of [[]]

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inverse <<- solve
  getsolve <- function() inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cachemean() attempts to retrieve a mean from the object passed in as the argument.
## Then it checks to see whether the result is NULL

cacheSolve <- function(x, ...) {

  inverse <- x$getsolve()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setsolve(inverse)
  inverse
}
