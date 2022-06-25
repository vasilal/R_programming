# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
   # Initial NULL value
   A <- NULL
   # Local functions
   set_X <- function(y) {
     x <<- y
   }
   get_X <- function(){
     x
   }
   set_Inv <- function(I){
      A <<- I
   }
   get_Inv <- function(){
     A
   }
   # The result (a list)
  list(set = set_X,get = get_X,setinverse = set_Inv,getinverse = get_Inv)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  M <- x$getinverse()
  # If the Null value
  if (is.null(M)) {
     data <- x$get()
     M <- solve(data, ...)
     x$setinverse(M)
  }
  # The result
  M
}
