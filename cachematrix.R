## Put comments here that give an overall description of what your
## functions do

## A <- makeCacheMatrix(B) creates a special matrix function A from a given matrix B

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # The set function changes the value of the cache matrix A:
  # With A$set(C), A is now generated from matrix C.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # The get function simply displays the original matrix
  get <- function() x
  
  # The setinv function changes the value of inv to s
  # It's the function called by cacheSolve
  setinv <- function(s) inv <<- s
  
  # The getinv function simply displays inv
  # inv is the inverse matrix, once it's calculated by cacheSolve()
  getinv <- function() inv
  
  # This is what is displayed when calling the makeCacheMatrix function
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve(A) calculates the inverse matrix and stores it in the cache matrix

cacheSolve <- function(x, ...) {
  
  # load the inv value from the cache matrix into the local inv variable
  # with the getinv() function
  inv <- x$getinv()
  
  # if the inv value is not NULL do not calculate anything: just return inv
  if(!is.null(inv)) {
    return(inv)
  }
  
  # load the original matrix from the cache matrix into the data variable
  # with the get() function
  data <- x$get()
  
  # load into local inv the value of the inverse of the original matrix 
  # calculated with solve()
  inv <- solve(data, ...)
  
  # load the value of local inv to the cache matrix with the function setinv 
  x$setinv(inv)
  
  inv
}
