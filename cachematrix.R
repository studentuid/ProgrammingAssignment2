## Put comments here that give an overall description of what your
## functions do

## This function required a matrix to compute 
## Example: thematrix <- matrix(c(1:2,2:1), 2,2)
## Store cached Matrix in variable: cacheoutput <- makeCacheMatrix(thematrix)
## Output / get cached matrix: cacheoutput$get()

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

      ## m will store / cache the matrix in getmatrix function
      
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      ## variable$get() retrieve the values
      get <- function() x
      setmatrix <- function(solve) m <<- solve
      
      ## variable$getmatrix() shows that the matrix has been stored
      getmatrix <- function() m
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}

## Example of cached inverse:
## inversedmatrix <- cacheSolve(cacheoutput)
## Show inversed matrix: inversedmatrix
## Repeat to see if cached: inversedmatrix <- cacheSolve(cacheoutput)
## Show inversed matrix: inversedmatrix

## computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      m <- x$getmatrix()
      if(!is.null(m)) {
            message("Getting cached data")
            return(m)
      }
      matrixdata <- x$get()
      m <- solve(matrixdata, ...)
      x$setmatrix(m)
      m
}


