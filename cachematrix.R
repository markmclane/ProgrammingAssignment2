## makeCacheMatrix -  
## Mark McLane  1/23/15 - this function  creates a special "matrix" object
## that can cache a matrix and it's inverse;  returns a list of get/set functions to be used later

makeCacheMatrix <- function(x = matrix()) {
	#resets the inverse
	inverse_of_x <- NULL
	
	#directly sets the original matrix value and resets the inverse
	set <- function(y) {
			x <<- y
			inverse_of_x <<- NULL
	}
	
	#simply returns the original stored matrix 
	get <- function() x

	#sets the inverse matrix value
	setinverse <- function(the_inverse) inverse_of_x <<- the_inverse
	
	#returns the current inverse matrix 
	getinverse <- function() inverse_of_x
	
	#this is the return of makeCacheMatrix, which is a list of the various getter/setter functions above
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## cacheSolve -  
## Mark McLane  1/23/15 -  returns the inverse of matrix, using the cached version 
## if available, otherwise calculates and sets in the cache  

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  ## Use cached version if available
  x_inverse <- $getinverse()
  if(!is.null(x_inverse)) {
    message("getting cached data")
    return(x_inverse)
  }
  
  data <- x$get()
  x_inverse <- solve(data)
  x$setinverse(x_inverse)
  x_inverse
  
}
