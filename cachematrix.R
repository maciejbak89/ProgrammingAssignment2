## The first function, makeCacheMatrix, creates a special "matrix" containting a function that:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse
## 4. gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      invmatrx <- NULL
      set <- function(y) {
        x <<- y
        invmatrx <<- NULL
      }
      get <- function() x 
      setinverse <- function(solv) invmatrx <<- solv
      getinverse <- function() invmatrx
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The second function, cacheSolve, calculates the inverse of the special "matrix"
## created with makeCacheMatrix. It first checks if the inverse has already been 
## calculated, and if it has, it gets the inverse from the cache and skips the computation.
## If it hasn't yet been calculated, the function calculates the inverse of the data and
## sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      invmatrx <- x$getinverse()
      if(!is.null(invmatrx)) {
        message("getting cached data")
        return(invmatrx)
      }
      data <- x$get()
      invmatrx <- solve(data, ...)
      x$setinverse(invmatrx)
      invmatrx
}
