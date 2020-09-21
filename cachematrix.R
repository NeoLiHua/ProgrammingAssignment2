## The functions are used for caching the inverse of a matrix.

## The makeCacheMatrix function creates a special 'matrix' object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  In<<-NULL
  set<-function(y){
    x<<-y
    In<<-NULL
  }
  get<-function()x
  setIn<-function(Inverse) In<<-Inverse
  getIn<-function() In
  list(set = set, get = get,
       setIn = setIn, getIn = getIn)
}


## The cacheSolve function computes the inverse of the matrix created by
## 'makeCacheMatrix' above. If the inverse has already been calculated
## the cacheSolve would retrieve the inverse from the Cache.

cacheSolve <- function(x, ...) {
  In<-x$getIn()
  if (!is.null(In)){
    message("getting cached data")
    return(In)
  }
  data<-x$get()
  In<-solve(data,...)
  x$setIn(In)
  In
  ## Return a matrix that is the inverse of the special matrix
}
