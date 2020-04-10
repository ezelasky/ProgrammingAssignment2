## Programming Assignment #2: Lexical scoping

## Objective: create a function with getters and setters to 
## cache a matrix and it's inverse
##
## Variables cached:
## x - input square matrix
## i - i of matrix x 
makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the inverse
  i <- NULL
  
  # define the setter function ... variables are in parent environment
  set <- function(y) {
    x <<- y      # set the data
    i <<- NULL   # set the inverse to null, since data has changed 
  }
  
  # getter function for the matrix
  get <- function() x
  
  # setter function to set inverse matrix
  setinv <- function(inv) i <<- inv
  
  # getter function for invserse matrix
  getinv <- function() i
  
  # return a list of functions that allow access to variables
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Objective: create a function to return the cached matrix inverse 
## or calculate it if necessary
##
cacheSolve <- function(x, ...) {
  
  # get the inverse matrix
  i <- x$getinv()
  
  # check to see if it is present
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
 
  # inverse matrix not present -- calculate it
  # get data
  data <- x$get()
  
  # calculate inverse
  message("calculating inverse matrix")
  i <- solve(data, ...)

  # set the inverse matrix in the cached object
  x$setinv(i)
  
  # return the inverse matrix
  i
}
