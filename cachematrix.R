 
## The functions makeCacheMatrix and CacheSolve are used to determine the inverse of a given matrix
## The inverse is stored in cache and retrieved on subsequent calls instead of recomputing it each time
## thereby reducing the computation time


## makeCacheMatrix function creates a special matrix object. It is 
## by the cacheSolve function to compute the inverse of the matrix . It also caches the inverse 
## value which can be accessed on subsequent calls.

makeCacheMatrix <- function(x = matrix()) {
	         
            inv <- NULL
	          
	          set <- function(y){
		          
	                 x <<- y
		               inv <<- NULL
          	}

	          get  <- function() x
	         
## Cache inverse
	         
            setinverse <- function(inverse) inv <<- inverse
	          getinverse <- function() inv
	         
## Create special matrix object
	         
		        list(set = set, get = get,
		        setinverse = setinverse,
		        getinverse = getinverse)
		
}

## cacheSolve function returns the inverse of a matrix. It first checks if the inverse value is 
## available in the cache. If it is available, it retrieves it from the cache else it computes it

cacheSolve <- function(x) {
      
  
  ## First check if inverse exists in cache
    
            inverse  <- x$getinverse()
		        if(!is.null(inverse)){
		        message("getting cached data for inverse")	

	          return(inverse)
	
}
	## Compute the inverse when cached data not available
    
    		    matr <- x$get()
		        inv <- solve(matr)
		        x$setinverse(inv)
		        inv
}
