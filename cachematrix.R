## These functions will take a matrix, inverse the matrix and output the result. If the matrix is
## already inversed the function prints out a message and merely returns the already in memory
## inversed matrix.

## This function sets up a lexical scoping matrix that can be seen in the parent environment

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  print(environment())                        #printing the environments out for testing
  evn <- environment()                        #used for testing purposes
  print(parent.env(evn))                      #printing the environments out for testing
  set <- function(y) {                        #constructs a function to set/reset solve to null and to cache parent x from y
    x <<- y                                   #is setting the cached x value to the passed in matrix
    s <<- NULL                                #set/reset s/solve to null
  }
  get <- function() x                         #function get to return x
  setsolve <- function(solve) s <<- solve     #creates a function to set the parent solved matrix
  getsolve <- function() s                    #function to return the solved matrix
  getevn <- function() environment()          #helper function to return the environment used in testing
  list(set = set, get = get,... =             #this is the list of helper functions for the function
       setsolve = setsolve,
       getsolve = getsolve,
       getevn = getevn)
}


## this function solves/inverses a matrix. If the the matrix is already solved it returns the result from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(class(try(solve(s),silent=T))=="matrix") {   #this looks to see if the matrix is already solved.
    message("already solved")                     #already solved provide a message
    return(s)                                     #return the already solved/inverse matrix
  }
  data <- x$get()                                 #gets unsolved/normal matrix
  s <- solve(data, ...)                           #solve/inverse the matrix. 
  x$setsolve(s)                                   #cahce the result
  s                                               #retrun the solved matrix
}




##  Every thing below this line is a test script for the above functions
##  Test Script

x <- rbind(c(1, -1/4), c(-1/4, 1))                #set up a numeric martix
mtrx <- makeCacheMatrix(x)                        #create the cached matriX
cacheSolve(mtrx)                                  #should not show the already solved message. but outputs and caches the solved matrix
cacheSolve(mtrx)                                  #shows the already solved message and the solved matrix
