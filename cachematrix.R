## The cachematrix.R contains two functions makeCacheMatrix() and cacheSolve().
## The first function creates an object to store matrix and its inverse.
## The second function, uses an argument returned by makeCacheMatrix() to
## retrieve the cached inverse matrix.  

## makeCacheMatrix() establishes a pathway to set the matrix, get the matrix,
## set the inverse matrix and get the inverse matrix from cache. This function
## returns a list containing the above-mentioned functions to set and get
## matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	invmat <- NULL
      set <- function(y) {
      	x <<- y
            invmat <<- NULL
            }
      get <- function() x
      setinv <- function(inv) invmat <<- inv
      getinv <- function() invmat
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve() retrieves the inverse matrix from its argument - x$getinv().
## Next, if() function checks if any value is stored in invmat. If stored value
## is not NULL then cached value is returned to the parent environment. Otherwise,
## cacheSolve() gets value for new matrix and calculates its inverse. 

cacheSolve <- function(x, ...) {
	invmat <- x$getinv()
      if(!is.null(invmat)) {
      	message("getting cached data")
            return(invmat)
            }
      data <- x$get()
      invmat <- solve(data, ...)
      x$setinv(invmat)
      invmat

## Return a matrix that is the inverse of 'x'
## x <- rbind(c(1,0,-1), c(0,1,0), c(0,1,1))
## > x
##       [,1] [,2] [,3]
## [1,]    1    0   -1
## [2,]    0    1    0
## [3,]    0    1    1
##
## Inverse of 'x':
##      [,1] [,2] [,3]
## [1,]    1   -1    1
## [2,]    0    1    0
## [3,]    0   -1    1
}