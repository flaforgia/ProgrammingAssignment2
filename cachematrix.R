## CacheMatrix.R:
## This script will create 2 functions. One (makeCacheMatrix()) will act as a 
## constructor of the methods used by the second (cacheSolve()) to compute the 
## inverse of a matrix or to retrieve it from the cache. 
##
## 1 - Caching Methodology:
## The caching is done by using the '<<-' operator. According to the help, this
## operator attempts to assign a value to a variable by looking for an existing 
## instance of that variable in a parent environment. It will continue 
## searching up the stack, looking for an existing variable (i.e., one with the 
## same name) until it finds one or it reaches the global environment. If it 
## finds a match, it assigns the value to that variable (in the first parent 
## environment where it finds a match). If it doesn't find a match, it creates
## the variable and assigns the value in the global environment.
## As result the variables are accessible by all the functions that need to 
## manipulate them. In the object programming syntax, this can be compared to 
## public variable.
## 
## 2 - The Functions:
## 
##      2.1 - makeCaheMatrix():
##        The makeCacheMatrix() function is the constructor which will create 
##      4 methods to manipulate the matrix that has been put as argument. 
##        This function is doing basically a initialization job. it takes a 
##      matrix as argument and will set two variables called inv and x.
##      The matrix is assigned to the variable x and the inverse of the matrix
##      is assigned to the variable inv. Because this inverse is not yet 
##      computed, it is initialized to NULL.
##        It is important to note that the argument must be a square matrix or 
##      the script will not be able to compute the inverse. This is why there
##      is two error detections test conditions. 
##      If the argument is not a matrix or not a square matrix, the program 
##      will stop and report an error message.
##        The set() and get() methods will manipulate the value of the matrix. 
##      Get() will retrieve the value and set() will set it.
##      The setinv() and getinv() methods have the same role but for the 
##      inv (i.e. inverse) variable.
##        The function returns a list of the methods. The list can also be 
##      cached or not depending of the value of the argument private. If 
##      private is TRUE, the list returned will be accessible only by the 
##      functions defined in the scope of the parent and needs to be 
##      specifically assigned to a variable with the '<-' operator. If private
##      is FALSE, the list will be automatically add to the global environment.
##      This was not requested in the assignment but I thought it could give
##      more flexibility to the script (cf. examples below).
##      
##      2.2 - cacheSolve():
##        This function will compute the inverse of the matrix input. It 
##      first checks if the inverse is not already in the cache by calling the
##      getinv() method.
##        if (inv != NULL), cacheSolve() displays inv value and does not 
##      compute the inverse.
##        if (inv ==  NULL), cacheSolve() retrieves the matrix in the cache by 
##      calling getinv() and  use it to compute the inverse with the function
##      solve(). Once the inverse is known, it is also stored in the cache with
##      the setinv() method.
##        Finally the function returns the inverse.
##
## 3- Examples Of Uses:
##
##      3.1 - private=TRUE
##      
##      R > source('~/Documents/ProgrammingAssignment2/cachematrix.R')
##      R > testNoList1 <- function() {
##        + print("entering test subfunction 1:")
##        + print(cacheSolve(listmethods))
##        + } 
##      R > testNoList <- function() {
##        + listmethods <- makeCacheMatrix(matrix2,private=TRUE)
##        + print("entering main test function:")
##        + print(cacheSolve(listmethods))
##        + testNoList1()
##        + }
##      R > testNoList()
##      [1] "entering main test function:"
##                   [,1]         [,2]         [,3]         [,4]
##      [1,]  0.008352062  0.020716323  0.045187407 -0.018639417
##      [2,] -0.002796167 -0.004580063 -0.009377548  0.006258734
##      [3,]  0.004461302 -0.001957542  0.016798321 -0.009838484
##      [4,] -0.005359443 -0.001035419 -0.002053322  0.002031105
##      [1] "entering test subfunction 1:"
##      Error in cacheSolve(listmethods) : object 'listmethods' not found 
##
##      3.2 - private=FALSE
##
##      R > source('~/Documents/ProgrammingAssignment2/cachematrix.R')
##      R > testNoList1 <- function() {
##        + print("entering test subfunction 1:")
##        + print(cacheSolve(basicMatrix))
##        + } 
##      R > testNoList <- function() {
##        + makeCacheMatrix(matrix2,private=FALSE)
##        + print("entering main test function:")
##        + print(cacheSolve(basicMatrix))
##        + testNoList1()
##        + }     
##      R > testNoList()
##      [1] "entering main test function:"
##                   [,1]         [,2]         [,3]         [,4]
##      [1,]  0.008352062  0.020716323  0.045187407 -0.018639417
##      [2,] -0.002796167 -0.004580063 -0.009377548  0.006258734
##      [3,]  0.004461302 -0.001957542  0.016798321 -0.009838484
##      [4,] -0.005359443 -0.001035419 -0.002053322  0.002031105
##      [1] "entering test subfunction 1:"
##      getting cached data
##                   [,1]         [,2]         [,3]         [,4]
##      [1,]  0.008352062  0.020716323  0.045187407 -0.018639417
##      [2,] -0.002796167 -0.004580063 -0.009377548  0.006258734
##      [3,]  0.004461302 -0.001957542  0.016798321 -0.009838484
##      [4,] -0.005359443 -0.001035419 -0.002053322  0.002031105


## makeCacheMatrix():
##
## Usage: makeCacheMatrix(x=matrix(), private=TRUE, ...)
## Input Variable(s) : x       = square matrix (default value=matrix())
##                     private = logical vector (default value=TRUE)
## Return Variable(s): A list of the 4 methods returned as: 
##                     privateMatrix = List accessiblee by the children 
##                     publicMatrix  = List accessible by all the functions in
##                                     the global environment.
## Variables: inv = Inverse of the input matrix (default=NULL)
##            x   = Input matrix (x must be a square matrix)

makeCacheMatrix <- function(x = matrix(), private=TRUE, ...) {
        
        # variables initialization
        inv <- NULL
        
        # Is x a matrix?
        if (!(is.matrix(x))) {
                message("USAGE: makeCacheMatrix(x=matrix(),private=TRUE,...)")
                stop("x is not matrix.")
        }
        
        # Is x a square matrix?
        if (nrow(x) != ncol(x)) {
                message("USAGE: makeCacheMatrix(x=matrix(),private=TRUE,...)")
                stop("x is not a square matrix. Inversion is not possible")
        }
       
        # Build the methods
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x

        setinv <- function(matrixinv) inv <<- matrixinv

        getinv <- function() inv
        
        # Return the functions in a list.
        privateMatrix <- list(set = set, 
                            get = get, 
                            setinv = setinv,
                            getinv = getinv) 
        if(private == TRUE) {
                    privateMatrix    
        } else if (private == FALSE) {
                publicMatrix <<- privateMatrix 
        } else {
                message("USAGE: makeCacheMatrix(x=matrix(),private=TRUE,...)")
                stop("private value unknown")
        }
}

## CacheSolve():
##
## Usage: cacheSolve(privateMatrix)
##        cacheSolve(publicMatrix)
## Input Variable(s) : A list of the contructor methods.
## Return Variable(s): An inverse of a square matrix.
## Variables: m    = The inverse of the input matrix from the cache.
##            data = The input matrix from the cache.

cacheSolve <- function(x,...) {
        
        # variables initialisation
        data <- x$get()
        m <- x$getinv() 
        
        # Test if inv is in the cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # Inv is not in the cache, compute the inverse of the matrix.
        m <- solve(data)
        x$setinv(m)
        m
}
