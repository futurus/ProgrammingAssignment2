## Pair of functions support cached matrix inversion

## this function mirrors the example, additionally it keeps track matrix x's change status

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  chg <- FALSE
  set <- function(y) {
    if (identical(x, y)) { ## if x and y are identical, skip
      return;
    } else {  ## otherwise set x to new matrix y and declare that it has changed
      x <<- y
      chg <<- TRUE
      m <<- NULL
    }
  }
  
  get <- function() x
  sInverse <- function(inv) m <<- inv
  gInverse <- function() m
  sChange <- function() chg <<- FALSE
  change <- function() chg
  list(set = set, get = get,
       sInverse = sInverse,
       gInverse = gInverse,
       sChange = sChange,
       change = change)
}


## if inverse is calculated AND x hasn't changed, show the inverse
## otherwise, calculate the inverse and set x's change status to false 
## (it hasn't changed since last calculation)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$gInverse()
  if(!is.null(m) & !x$change()) {
    message("getting cached data")
    return(m)
  }
  
  m <- solve(x$get(), ...)
  x$sInverse(m)
  x$sChange() ## reset x's change status to FALSE (since last calculation, which is just now
  m
}
