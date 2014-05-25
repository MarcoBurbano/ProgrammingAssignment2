#creates a special "Matrix", which is really a list containing a function to
#set the value of the Matrix
#get the value of the Matrix
#set the value of the solve
#get the value of the solve
makeCacheMatrix<- function(x = matrix())
{
      x<-as.data.frame(x)
      m <- NULL
      
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      
      getsolve<- function() m 
      setsolve<- function(solve) m <<- solve
      list(get=get, set=set,
           setsolve = setsolve,
           getsolve = getsolve)
}


# Return a matrix that is the inverse of 'x' using cache 
# it gets the solve from the cache and skips the computation
cacheSolve <- function(x, ...) {
   
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
      
}
