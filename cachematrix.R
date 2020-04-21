##The main aim of the makeCacheMatrix is to produce a square and invertible matrix and puts it into Cache

##The function also contains 4 other functions:set,get,setinv,getinv

##set function is used to set the values for the matrix

##get function is used for getting the values of matrix

##setinv function is used for setting the inverse

##getinv function is used for getting the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv<<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##cacheSolve function either retrieves the inverse in Cache or solves for the matrix and sets the inverse in Cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

