## The following functions cache the inverse of matrix to 
## reduce computation costs

## Creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() s
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
       )
}


## Computes the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("geting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
