# These functions allow you to compute the inverse matrix of a square invertible
# matrix.
# A cache is used that holds previously generated inverse matrices. Any newly
# calculated inverse matrices are added to this cache.
#
# Example usage:
# > t <- matrix(1:4, nrow = 2, ncol = 2)
# > cm <- makeCacheMatrix(t)
# > cacheSolve(cm)
# no cached data found!
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(cm)
# cached data found!
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
# This function creates a matrix object with the set, get, setCacheMatrix and
# getCacheMatrix functions
makeCacheMatrix <- function(x = matrix()) {
    # set m to NULL. Will be set manually in setCacheMatrix()
    m <- NULL

    # Set function to manually set the matrix (not the inverse)
    # sets m to NULL as for this new matrix no inverse has been calculated
    # and no cache has been set
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    # return them matrix itself (not the inverse)
    get <- function() x

    # set the inversed matrix in the cache
    setCacheMatrix <- function(solve) m <<- solve

    # get the inversed matrix from cache
    getCacheMatrix <- function() m

    # expose the functions this function offers
    list(set = set, get = get,
        setCacheMatrix = setCacheMatrix,
        getCacheMatrix = getCacheMatrix)
}

# This function returns the inverse matrix.
# If the inverse matrix is in the cache, it will return that cached data.
# If the inverse matrix is not in the cache, it will calculate it, store it
# in the cache, and return it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Try to get the inverted matrix from the cache
    m <- x$getCacheMatrix()
    # If m is not null we have cached data. Return that data.
    if(!is.null(m)){
        message("cached data found!")
        return(m)
    }
    # If no cached data is found:
    else{
        message("no cached data found!")
        # get the matrix
        data <- x$get()
        # get the inverse matrix
        m <- solve(data)
        # store the inverse matrix in the cache
        x$setCacheMatrix(m)
        # return the inverse matrix
        return(m)
    }
}
