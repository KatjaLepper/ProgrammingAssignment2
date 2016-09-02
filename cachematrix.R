# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.


## ---------------------------------------------------------------------------- 
## This function creates a special "matrix" object that can cache its inverse.
## ---------------------------------------------------------------------------- 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # generates an empty (NULL) inverse matrix at the beginning
                set <- function(y) { # define function that ...
                x <<- y # ... sets the matrix x to a new matrix y             
                m <<- NULL # ... resets the inverse function to NULL
                }
  get <- function() x # define function that catches matrix
  setInverse <- function(solve) m <<- solve 
  # define function that assigns inverse result from solve command to empty inverse matrix m
  getInverse <- function() m
  # define function that catches inverse 
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
  # Return list with functions defined above
}

## ---------------------------------------------------------------------------- 
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated,
## then the cachesolve should retrieve the inverse from the cache.
## ---------------------------------------------------------------------------- 
cacheSolve <- function(x, ...) {
        # 1. Inverse matrix already calculated and cached by makeCacheMatrix
        m <- x$getInverse() # assigns inverse matrix from makeCacheMatrix to m 
        if(!is.null(m)) { 
        message("getting cached data")
        }
        # 2. Inverse matrix NOT already calculated and cached by makeCacheMatrix
        data <- x$get() # draws original matrix
        m <- solve(data, ...) # calculates inverse of original matrix
        x$setInverse(m) # assigns inverse matrix to m
        m # prints m
}

## ---------------------------------------------------------------------------- 
## Example 
## ---------------------------------------------------------------------------- 
exampleMatrix <- rbind(c(9,13,5,2), c(1,11,7,6), c(3,7,4,1), c(6,0,7,10))
x <- makeCacheMatrix(exampleMatrix)

cacheSolve(x) # Calculates data because tehere is no cached data available

##      [,1]       [,2]        [,3]        [,4]
##      [1,]  0.09905101 -0.1049822 -0.01897983  0.04507711
##      [2,]  0.09074733  0.1014235 -0.18505338 -0.06049822
##      [3,] -0.26453144 -0.1387900  0.70937129  0.06524318
##      [4,]  0.12574140  0.1601423 -0.48517200  0.02728351


cacheSolve(x) # Retrieves cached data from first calcuation

##      getting cached data
##      [,1]       [,2]        [,3]        [,4]
##      [1,]  0.09905101 -0.1049822 -0.01897983  0.04507711
##      [2,]  0.09074733  0.1014235 -0.18505338 -0.06049822
##      [3,] -0.26453144 -0.1387900  0.70937129  0.06524318
##      [4,]  0.12574140  0.1601423 -0.48517200  0.02728351
