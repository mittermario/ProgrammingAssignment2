## Assignment: Caching the inverse of a matrix

## create a customized matrix that allows caching of its inverse
makeCacheMatrix <- function(x = matrix()) {

    ## default value of member cached_inverse
    cached_inverse <- NULL
    
    ## setter method for customized matrix
    set <- function(y)
    {
        x <<- y
        cached_inverse <<- NULL
    }
    
    ## getter method for customized matrix
    get <- function() x
    
    ## setter method for calculated and storing the inverse
    set_inverse <- function(inverse) cached_inverse <<- inverse
    
    ## getter method for stored inverse
    get_inverse <- function() cached_inverse
    
    ## return list of methods
    list(set = set, get = get,
                  set_inverse = set_inverse,
                  get_inverse = get_inverse)
}


## efficient calculation of matrix inverse that makes use of makeCacheMatrix
cacheSolve <- function(x, ...) {
    
    ## try to get stored matrix
    inverse <- x$get_inverse()
    
    ## return existing cached value
    if(!is.null(inverse))
    {
        message("getting cached inverse matrix")
        return(inverse)
    }
    
    ## calculate inverse and store value in custom matrix
    inverse <- solve(x$get())
    x$set_inverse(inverse)
    inverse
}
