##The functions below first create a matrix and then compute the inverse of the matrix
##The inverse of the matrix is stored in memory. This way, the computation doesn't need
##to be repeated twice if you want to compute the inverse again.  


###This function creates a matrix that is really a list containing functions that 
# 1.) set the matrix
# 2.) get the matrix
# 3.) set the inverse of the matrix
# 4.) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix(), ... ) {
  
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInv <- function(Inv) m <<- Inv
  getInv <- function() m
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}


###This function checks whether the inverse has been computed or not. If it has, it returns the 
## cached inverse. If it hasn't, it computes the inverse and stores it in memory

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}

