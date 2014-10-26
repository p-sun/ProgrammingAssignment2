## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix - # creates a matrix that caches it's inverse
# stores 2 things:
#1) x, the matrix,     2) i, the matrix's inverse
# also returns a list of 4 functions
# $set() - set the matrix, x
# $get() - get the matrix, 
# $setinv() - set m's inverse, i
# $getinv() - get m's inverse, i
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix
#If the inverse has already been calculated (and the matrix has not changed), 
#then retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  if(!is.null(x$getinv())){
    print("Getting from cache...")
    x$getinv()
  } else {
    inv <- solve(x$get())
    x$setinv(inv)
    inv
  }
}

##For testing functions
d <- matrix(c(1,4,5,2,4,7,11,11,11), 3, 3)
k <- makeCacheMatrix()
k$set(d)
cacheSolve(k)
cacheSolve(k)

