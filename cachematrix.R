## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
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


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
      #x<-as.data.frame(x)
      
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


