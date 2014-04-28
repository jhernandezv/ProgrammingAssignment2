## Programming Assignment 2
## using cache for marix inverse calculation

## First part uses cache

makeCacheMatrix <- function(x = matrix()) {
  mx <- NULL
  set <- function(y) {
    x <<- y
    mx <<- NULL
  }
  get <- function() x
  setmxinverse <- function(mxinverse) mx <<- mxinverse
  getmxinverse <- function() mx
  list(set = set, get = get,
       setmxinverse = setmxinverse,
       getmxinverse = getmxinverse)  
}


## Second part calculates the matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mx <- x$getmxinverse()
  if(!is.null(mx)) {
    message("getting cached matrix")
    return(mx)
  }
  data <- x$get()
  mx <- solve(data, ...)
  x$setmxinverse(mx)
  mx  
}
