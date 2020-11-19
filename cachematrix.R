## Put comments here that give an overall description of what your
## functions do


## Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { ##set the value of the matrix
      x <<- y
      inv <<- NULL
    }
    get <- function() {x} ##get the value of the matrix
    setInverse <- function(inverse) {inv <<- inverse} ##set the value of the inverse matrix
    getInverse <- function() {inv} ##get the value of the inverse matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, then it retrieve the inverse from
## the cache 

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
       inv <- x$getInverse() ##return matrix assigned to x and store in "inv"
       if(!is.null(inv)) {   ##check if the inverse was already been calculated
         message("getting cached data") ##if it is, gets data from cache
         return(inv)
         }
       mat <- x$get() #If not, compute the inverse matrix in the cache
       inv <- solve(mat, ...) ##solve --> standard to compute inverse
       x$setInverse(inv) ##value of inverse
       inv ##inversed matrix
}
