## Functions that store a matrix and calculate its inverse
## To use: 
##        create a matrix M<-matrix(c(4,7,2,6),nrow=2,ncol=2)
##        create a makeCacheMatrix object myMatrix<-makeCacheMatrix(M)
##        call cacheSolve on the object cacheSolve(myMatrix) 

## Provides function to store and return the matrix and its solved inverse

makeCacheMatrix <- function(x = matrix()) {
  s<-NULL
  set <- function(y)
  {
    x<<-y
    s<<-NULL
  }
  get <-function()x
  
  setsolve<-function(solve) s<<-solve
  getsolve<- function() s
  list(set =set, get=get,setsolve=setsolve,getsolve=getsolve)

}


## Calls functions from makeCacheMatrix to return the inverse

cacheSolve <- function(x, ...) {
        s<-x$getsolve()
        if(!is.null(s))
        {
          message("getting cached solve data")
          return (s)
        }
        data <- x$get()
        s <- solve(data,...)
        x$setsolve(s)
        s
  
}
