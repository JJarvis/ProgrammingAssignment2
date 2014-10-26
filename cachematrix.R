## makeCacheMatrix - set up the list that contains the accessors for the matrix in question
## cacheSolve - return the cached inverse if it hs already been instantiated, otherwise compue inv chache it and return it

## makeCacheMatrix - set up the list that contains the accessors for the matrix in question

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## cacheSolve - return the cached inverse if it hs already been instantiated, otherwise compue inv chache it and return it

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
