## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## This function creates a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse property to NULL
    inv <- NULL
    
    # Method to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset the inverse when the matrix changes
    }
    
    # Method to get the matrix
    get <- function() x
    
    # Method to set the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    
    # Method to get the inverse of the matrix
    getInverse <- function() inv
    
    # Return a list of methods
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    # Get the inverse from the cache
    inv <- x$getInverse()
    
    # If the inverse is already cached, return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, get the matrix
    data <- x$get()
    
    # Calculate the inverse using solve()
    inv <- solve(data, ...)
    
    # Cache the inverse
    x$setInverse(inv)
    
    # Return the inverse
    inv
}


## Example usage:
## 
## # Create a sample invertible matrix
## my_matrix <- matrix(c(4, 2, 7, 6), nrow = 2, ncol = 2)
## 
## # Create the special matrix object
## cached_matrix <- makeCacheMatrix(my_matrix)
## 
## # First call - computes and caches the inverse
## inverse1 <- cacheSolve(cached_matrix)
## print(inverse1)
## 
## # Second call - retrieves from cache (displays "getting cached data")
## inverse2 <- cacheSolve(cached_matrix)
## print(inverse2)
## 
## # Verify the inverse is correct (should return identity matrix)
## my_matrix %*% inverse1
## 
## # Update the matrix
## new_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
## cached_matrix$set(new_matrix)
## 
## # This will compute a new inverse (cache was reset)
## inverse3 <- cacheSolve(cached_matrix)
## print(inverse3)
