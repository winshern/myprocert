
## This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setmatrix <- function(solve) m<<-solve
  getmatrix<-function()m
  list(set=set, get=get, 
       setmatrix=setmatrix, 
       getmatrix=getmatrix)
}

## This function computes the inverse of the special matrix
## If inverse has been calculated, then will retrieve inverse from cache
cacheSolve <- function(x=matrix(), ...){
  m<-x$getmatrix()
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
