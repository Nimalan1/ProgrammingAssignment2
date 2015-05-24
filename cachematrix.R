# this function creates a list with 4 member functions: set, get, setInv,getInv                                 # getInv uses <<- assignment operator so that these internal variables are unexposed to the outside environment. 

  makeCacheMatrix <- function(x = matrix()) {

      xinv <- NULL 
      set <- function(y) {
	  x <<- y
	  xinv <<- NULL 
      }

      get <- function() x 
      setInv <- function(inv) xinv <<- inv # set the inversed matrix
      getInv <- function() xinv # return the inversed matrix
      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)
  }


  cacheSolve <- function(x, ...) {
      m <- x$getInv()  x
      if(!is.null(m)) {
	  message("getting cached data")
	  return(m)
      }
      data <- x$get() 
      m <- solve(data) 
      x$setInv(m)
      m 
  }

 # Test
 # generate a  matrix
  test <- matrix(runif(9,1,100),3,3)
  testCached <- makeCacheMatrix(test)

  testInv <- cacheSolve(testCached)
  testInv <- cacheSolve(testCached)
  testInv <- cacheSolve(testCached)
  testInv <- cacheSolve(testCached)
  testInv <- cacheSolve(testCached)
