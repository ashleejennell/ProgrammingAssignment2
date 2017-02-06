## Caching the Inverse of a Matrix

## Functions will cache the inverse of a matrix to avoid repeated
## calculation. 

## makeCacheMatrix creates a special "matrix" object 
## that gets and sets the value of the matirx and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Function takes a matrix x as its argument, where x is 
  ## assumed to be invertible.
  
  inv <- NULL ## initalizes the object which stores the inverse
  set <- function(y){
    x <<- y 
    inv <<- NULL
  } ## globally sets value of x and initializes inv 
  get <- function() x ## gets current value of x
  setinv <- function(inverse) inv <<- inverse ## globally sets inv
  getinv <- function() inv ## gets current value of inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  ## creates special "matrix" object

}


## cacheSolve first checks to see of the inverse of the matrix 
## had previously been calculated. If so (and the matrix has not
## changed), it uses this inverse value. If not, it 
## calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinv() ## gets global value of inv
  if(!is.null(inv)){
    meassage("getting cached data")
    return(inv)
  } ## checks if inv is non-zero and returns its value if it is
  data <- x$get()
  inv <- solve(data,...) ## calculates inverse if it doesn't exist
  x$setinv(inv) ## globally sets calculated value of inv
  inv ## returns the inv of matrix x
}
