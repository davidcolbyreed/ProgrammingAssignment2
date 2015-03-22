## makeCacheMatrix takes a matrix and returns a list of output variables in its environment
## These outputs are created here to serve as inputs to the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL # Initialize to NULL in order to make room for inputted value on the first pass through the function
    print(environment()) # Testing to grab the name of makeCacheMatrix's environment
    evn <- environment() # Testing to see that makeCacheMatrix was created in the global environment
    print(parent.env(evn)) # Continued: Testing to see that makeCacheMatrix was created in the global environment
    set <- function(y) { # y is an argument passed into the function (essentially a body double for x in the parent)
        x <<- y # Assigns y to x in the parent environment (that of makeCacheMatrix)
        mat <<- NULL # Makes mat a null value in the in the parent environment (that of makeCacheMatrix) in order to allow for subsequent programmatic modification
    }
    get <- function() x # A function to be used in cacheSolve if there's already a cached value
    setmat <- function(inverted_matrix) mat <<- inverted_matrix # Assigns the inverted matrix to mat in the makeCacheMatrix environment
    getmat <- function() mat # Returns mat from makeCacheMean environment
    getevn<- function() environment() # Testing to see that there's a third environment (with no stored variables)
    list(set = set, get = get, # Returns a list of 
         setmat = setmat,      # 1) functions from the makeCacheMatrix environment, 
         getmat = getmat,      # 2) their values, and 
         getevn = getevn)      # 3) test prints their environments (which should all be the same)     
    }
}


## cacheSolve takes a matrix argument and checks to see whether that argument has been 
## evaluated. If yes, then it returns the cached value of the inverted matrix. 
## If no, then it computes the inverted matrix by calling the solve function. 

cacheSolve <- function(x, ...) {
    m <- x$getmat() # Checks the makeCacheMatrix environment and assigns the value from mat there to mat here
    if(!is.null(mat)) { # Logical test of whether mat is NULL or defined
        message("getting cached data") # A friendly status update for the user
        return(mat) # Returns the value of mat from the makeCacheMatrix environment (if that value != NULL)
    }
    data <- x$get() # Creates a local environment variable data containing the x matrix
    mat <- solve(data, ...) # Evaluates an inverted matrix and assigns that value 
    x$setmat(mat) # Caches the matrix inversion in the local environment
    mat # Returns the inverted matrix
}
