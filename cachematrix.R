## In order to avoid re-computation of costly operations like matrix inverses, the following two functions have been designed. 
## For a given matrix if its inverse is computed, it will be cached. 
## In any further operations, if this matrix inverse is required, the cached value will be returned instead of re-computing the inverse again. 
## When there is no cached value, the inverse will be computed and cached.

## makeCacheMatrix - This function creates a customized matrix object, whereby it can store a matrix, retrieve a matrix and store (cache) its inverse (if computed) or initialize the matrix inverse to NULL.

makeCacheMatrix <- function(x = matrix()) {

	## initializing the inverse to NULL
	inverse <- NULL

	## function to set the matrix to a given input value and also initializing its inverse to NULL
	setMatrix <- function(y)
	{
		x <<- y
		inverse <<- NULL
	}

	## function to get the matrix value of this customized matrix object
	getMatrix <- function() x

	## given an inverse for this matrix, this function caches the value
	setInverse <- function(inv)
	{
		inverse <<- inv
	}

	## this function helps in retrieving the inverse for this matrix object
	getInverse <- function() inverse

	## the return value of this function, a list with four functions described above as its elements
	list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}

## cacheSolve - This function takes the customized matrix object created by "makeCacheMatrix" as input and computes and returns its inverse if the cached value is NULL, otherwise simply returns the cached inverse value

cacheSolve <- function(x, ...) {

	## Retrieves the inverse from the customized matrix object
	inverse <- x$getInverse()
	
	## if the inverse is cached (i.e., not null), return the cached inverse matrix
	if(!is.null(inverse))
	{
		message("Retrieving the cached data")
		return(inverse)
	}

	## there is no cached value, hence getting the matrix data to compute the inverse
  message("No cached data availabe. Hence, calculating the inverse !")
  matData <- x$getMatrix()

	## computing the inverse
	inverse <- solve(matData)

	## caching the inverse, to avoid any re-computation of inverse of the same matrix
	x$setInverse(inverse)
        
	## Returning a matrix that is the inverse of 'x'
	inverse
}
