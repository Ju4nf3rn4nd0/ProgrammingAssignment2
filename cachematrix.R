## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function 
# answer: 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## answer: this function defines i as the inverse of x, if i is null then a message appears where there is not data. 
# data is given by the solution of an equation sistem where i is the matrix to solve. 
# finally x gets inverse according to the i solution. Then it returns i where i is the inverse matrix of x. 
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


##### my answer
## let's define a simetric matrix

matrix <- matrix(rnorm(10,mean = 100,sd=100),2,2)
matrix

m <- makeCacheMatrix(matrix)
cacheSolve(m)
