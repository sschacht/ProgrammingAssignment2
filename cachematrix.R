
## Function which will cache a created Matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Function which will use the cached Matrix and invert the matrix by
## using the function solve

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

## Matrix will be created
matrix.s <- matrix(c(1:4), nrow = 2)
## Matrix will be shown
matrix.s
## Chaced Matrix will be created
my_matrix <- makeCacheMatrix(matrix.s)
## Matrix will be inversed by using cached matrix
my_matrix_inv <- cacheSolve(my_matrix)
## Inverted Matrix will be shown
my_matrix_inv