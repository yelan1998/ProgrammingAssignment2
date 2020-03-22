## Put comments here that give an overall description of what your
## functions do
## -------------------------comments--------------------------
## In this assignment, I completed the following two functions:
## <makeCacheMatrix> which used to create a matrix that can cache 
## the inverse;
## <cacheSolve> which used to directly return the cached inverse 
## when the inverse has already been cached, or calculate the 
## inverse and then cache the inverse when the inverse has not been
## cached.
##--------------------------------------------------------------


## Write a short comment describing this function

## This function used to create a matrix that can cache the inverse;
## it takes a normal matrix as an input, and users can do 4 operations:
## get and set the matrix value, or get and set the inverse value.

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  
  set <- function(y){
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) iv <<- inverse
  getinverse <- function() iv
  
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function used to directly return the inverse value of the 
## cached matrix created in the last functon, or it will calculate
## the inverse value and cache it in the matrix when the matrix 
## has not cached the inverse yet.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iv <- x$getinverse()
  if(!is.null(iv)){
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data,...)
  x$setinverse(iv)
  iv
}
