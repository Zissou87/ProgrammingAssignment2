## Matrix inversion can be costly
## The following functions when used together allow for the storage
## and computation of the inverse of a matrix

## makeCacheMatrix is a function that creates a matrix object that can
## cache it's own inverse

## makeCacheMatrix returns a list of functions:
## set - sets the value of the matrix
## get - gets the value of the matrix
## setinverse - sets the value of the inverse of the matrix
## getinverse - returns the stored value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
			inv <- NULL
			set <- function(y) {
					x <<- y*
					inverse <<- NULL
			}
			getmatrix <- function() x
			setinverse <- function(inverse) inv <<-inverse
			getinverse <- function() inv
			list(set = set, get = getmatrix,
					setinverse = setinverse,
					getinverse = getinverse)
}


## cacheSolve returns the inverse of the stored matrix from makeCacheMatrix 
## after checking if the inverse has already been computed. If the matrix
## has already been computed it returns the stored data and doesn't
## recompute the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
		if(!is.null(inv)) {
					message("getting cached data")
					return(inv)
		}
		data <- x$get()
		inv <- solve(data, ...)
		x$setinverse(inv)
		inv
}
