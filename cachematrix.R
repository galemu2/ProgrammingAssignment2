## this function takes a value and converts it into a 2 x 2 matrix, 
## and calculates the inverse using the solve functtion

## create a matrix with "NA" value
## fill the matrix with desired values (values must fit a 2 X 2 matrix)
## clculate the inverse with using the solve function

makeCacheMatrix <- function(x = matrix( )) {
        ## creat a matrix with "NA" values
        m <- matrix()
        ## this function converts the input into a 2 x 2 matrix. 
        ## values must fit into a 2 x 2 matrix
        set <- function(y) {
                x <<- matrix(y, 2, 2)
                m <<- matrix( )
        }
        get <- function()x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)                
}


## identify the cached matrix with "NA" values.
## if cached matrix has "NA" values (starting at the begining), then
## calculate the invers of the patrix and cache it.
## if chached matrix does not have "NA" values, and the matrix has not changed,
## then retrive the cached inverse matrix and desplay the matrix.

cacheInverse <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## is the matrix has "NA" values, then
        if(!is.na(m[1,1])) {
               message("Getting cached data.")
               return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
