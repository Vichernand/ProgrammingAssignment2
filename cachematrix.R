#The first one is the function that creates the special matrix and can cache
#the inverse of the matrix. the second function calculates the inverse of the
# matrix if it has not calculated before

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

ma<- matrix(c(2,3,5,7),2,2)
ma

ma1<- makeCacheMatrix(x=ma)
cacheSolve(ma1)
