## This R file contains two functions to create a "matrix" object which can cache its
## inverse as well as compute the inverse of the data contained by the "matrix" object.
## Once the inverse has been computed, the result can be retrieved from the cache in order
## to reduce computation time.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        # Cached inverse
        inv <- NULL

	  # $set(y) stores the provided matrix and deletes the cache.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

	  # $get(y) retrieves the stored matrix.
        get <- function() {
		    x
	  }

	  # $setinv(inverse) stores the provided matrix inverse.
        setinv <- function(inverse) {
		    inv <<- inverse
	  }

	  # $setinv(inverse) retrieves the stored matrix inverse.
        getinv <- function() {
	    	    inv
  	  }

	  # Return a list of the four functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        ## Retrieve stored matrix inverse
        inverse <- x$getinv()

        ## If the matrix inverse is already stored, then return it
        if(!is.null(inverse)) {
                return(inverse)
        }

        ## Retrieve stored matrix data
        data <- x$get()

        ## Calculate the matrix that is the inverse of 'x'
        inverse <- solve(data, ...)

        ## Cache the matrix that is the inverse of 'x'
        x$setinv(inverse)

        ## Return a matrix that is the inverse of 'x'
        inverse
}