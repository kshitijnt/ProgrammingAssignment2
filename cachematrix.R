## Matrix inversion is an expensive computation and there are benefits to 
## catching the Matrix inversion than compute it each time. 

## The following functions together catch an inverse of a matrix.

## Assumption is that a matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse. 
## It does 4 things as below
## Set the value of the matrix.
## Get the value of the matrix.
## Set the value of the matrix inverse.
## Get the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) 
{
      m <- NULL
      set <- function(y) 
          {
             x <<- y
             m <<- NULL
          }
      get <- function() x
      setinverse <- function(square) m <<- square
      getinverse <- function() m
      list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache and not recompute it.
## However if the inverse has not been calculated previously then the CacheSolve function computes
## the inverse using the setinverse function.

cacheSolve <- function(x, ...) 
{
  m <- x$getinverse()
  if(!is.null(m)) 
    {
        message("getting cached data")
        return(m)
    }
  data <- x$get()
  m <- square(data, ...)
  x$setinverse(m)
  m
}

