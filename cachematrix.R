# makeCacheMatrix
# Function create the "matrix" object(list) that cached it inverse.

# Usage of the function
# M<-makeCacheMatrix() Create list with empty matrix x

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) 
    {
    x <<- y
    inv <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


# cacheSolve
# Function computes the inverse of the matrix returned by
# function makeCacheMatrix.  If the inverse matrix was already calculated, 
# then the cached data read from the cache.

# Usage function cacheSolve(x)
# Returns the inverse of the "matrix" x


cacheSolve <- function(x, ...) 
{
  inv <- x$getinverse()
  if(!is.null(inv))
  {
    message("Get cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


## Test example of inverse matrix 3x3
# Mtest<-matrix(c(1:8,10),3,3)
# M <- makeCacheMatrix(Mtest)
# cacheSolve(M)
## Second usage of cacheSolve will "Get cashed data"
# cacheSolve(M)
## Test of inverse by R solve function: 
# solve(Mtest)
