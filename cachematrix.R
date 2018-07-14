## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## If matrix availabel, stores the matrix in cache else stores NULL
makeCacheMatrix <- function(x = matrix()) {

  invertedMatrix <- NULL
  set <- function(y){
    x <<- y
    invertedMatrix <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse) invertedMatrix <<- inverse
  getInverse <- function() invertedMatrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## loads cached inverted matrix or calculates that and returns inverted matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invertedMatrix <- x$getInverse()
  if(!is.null(invertedMatrix)){
    message("Getting cached data")
    return(invertedMatrix)
  }
  data <- x$get()
  invertedMatrix <- solve(data,...)
  x$setInverse(invertedMatrix)
  invertedMatrix
}
