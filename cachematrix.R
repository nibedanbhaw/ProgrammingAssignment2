## These functions compute the inverse of a square matrix and caches the computed inverse. 
## If the same matrix is supplied for inverse computation, instead of computing the inverse again, the cached result is returned

## makeCacheMatrix returns a set of functions in form of a list that helps to obtain the input matrix and cache the resultant inverse. 
makeCacheMatrix <- function(x = matrix()) {

    i<-NULL
  
    set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse<- function() i
  
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve returns the inverse of the input matrix. It first looks in cache for the inverse. If the inverse is found in cache,
## it retrieves and returns the inverse from the cache without actual inverse computation. In case, a different matrix is supplied
## as an input, this function computes the inverse, caches and returns the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
i<- x$getinverse()

if(!is.null(i)) {
  message("getting cached data")
  return(i)
}

data <- x$get()
i <- solve(data, ...)
x$setinverse(i)
i
}
