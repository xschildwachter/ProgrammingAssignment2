## Returns a cached version of the matrix passed as argument
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolved <- function(solve) s <<- solve
  getsolved <- function() s
  list(set=set, get=get, setsolved=setsolved, getsolved=getsolved)
}

## Returns the inverse of the cached matrix passed as argument
## Returns cached value if available, computes otherwise
cacheSolve <- function(x, ...) {
  s <- x$getsolved()
  if (! is.null(s)) {
    message('Getting cached data')
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolved(s)
  s
}
