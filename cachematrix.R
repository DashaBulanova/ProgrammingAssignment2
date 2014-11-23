
## Create special type of matrix that can be used for cached its inverse matrix

makeCacheMatrix <- function(originalMatrix = matrix()) {

  inverseMatrix <- NULL
  
  get = function(){
    originalMatrix
  }
  
  set = function(y){
    originalMatrix <<- y
    inverseMatrix <<- NULL
  }  
  
  getInverse = function(){
    inverseMatrix
  }
  
  setInverse = function(inverse){
    inverseMatrix <<- inverse
  }  
  
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
 
  m <- x$getInverse()
  
  ## If inverse matrix has been already calculated then return result from cache
  if (!is.null(m)){
    message("getting result from cache")
    return(m)
  }
  
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setInverse(m)
  m
}
