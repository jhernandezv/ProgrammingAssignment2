## Programming Assignment 2
## using cache for marix inverse calculation

## First part uses cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmxinverse <- function(mxinverse) m <<- solve
  getmxinverse <- function() m
  list(set = set, get = get,
       setmxinverse = setmxinverse,
       getmxinverse = getmxinverse)  
}


## Second part calculates the matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmxinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmxinverse(m)
  m  
}
