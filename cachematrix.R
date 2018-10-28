## This is written for Week 3's assignment in the R Programming couse by 
## zacsentell


## This function will create a special matrix object and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x  
## retrieves x
  setinverse <- function(inverse) invs <<- inverse
  getinverse <- function() invs
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will compute the inverse of the matrix returned by the previous function 
## if the inverse has preivously been calculated then this function will retrieve it from the 
## cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    invs <- x$getinverse()
    if (!is.null(invs)) {
      message("getting cached data")
      return(invs)
    }
    matrix <- x$get()
    invs <- solve(matrix, ...)
    x$setinverse(invs)
    return(invs)
}
