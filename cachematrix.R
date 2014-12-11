## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list of functions: to set and get the values of the matrix
## and the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # setting the value of the inverse matrix to zero, if it exists
        
        if(class(x) != "matrix" ) {stop("object is not a matrix")} # checking the class of the input
        if(det(x) == 0) {stop("the determinant is zero")} # checking whether the determinant of a matrix is zero
        set <- function(y) { # "set" lets us to assign a new value to "x"
                x <<- y
                inv <<- NULL
        }

        get <- function() {x} # assign the value of the original matrix
        setinv <- function(solve) {inv <<- solve} # setting and caching ("superassigning") an inverse matrix
        getinv <- function () {inv} # on subsequent calls getinv will cache the value of an inverse matrix
        
        list(get = get, setinv = setinv, getinv = getinv) # list of functions
}


## cacheSolve calculates the inverse matrix and caches it, thus saving time of accessing
## the output

cacheSolve <- function(x, ...) {
        inv <- x$getinv() # accessing "x" dataframe and getting the inverse matrix
        
        if(!is.null(inv)) { # if "inv" is not NULL, then it is in cache
                message("getting cached data") # print the message on the console
                return(inv)
        }
        
        data <- x$get() # further code is executed when inv is NULL (not in cache)
        inv <- solve(data) # calculating inverse matrix
        x$setinv(inv)
        inv
}
