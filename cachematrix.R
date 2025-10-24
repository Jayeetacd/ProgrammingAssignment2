
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse property to NULL
    inv <- NULL
    # Method to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset the inverse when matrix changes
    }
    # Method to get the matrix
    get <- function() x
    # Method to set the inverse
    setInverse <- function(inverse) inv <<- inverse
    # Method to get the inverse
    getInverse <- function() inv
    # Return a list of methods
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
 
cacheSolve <- function(x, ...) {
    # Try to get the cached inverse
    inv <- x$getInverse()
    # If inverse is already cached, return it
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
 
# Create a sample invertible matrix
my_matrix <- matrix(c(4, 2, 7, 6), 2, 2)
 
# Create the special matrix object
cached_matrix <- makeCacheMatrix(my_matrix)
 
# First call - computes the inverse
cacheSolve(cached_matrix)
 
# Second call - retrieves from cache (you'll see "getting cached data")
cacheSolve(cached_matrix)
