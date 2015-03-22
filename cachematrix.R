
# ==============================================================
# COURSE:       R Programming
# ASSIGNMENT:   Programming Assignment #2: Lexical Scoping
# 
# DATE:         March 2015
#
# FILE:         cachematrix.R
#
# DESCRIPTION:  The following functions create and use 
#               a special cached matrix object, which caches
#               both the value of the matrix and its calculated
#               inverse matrix.  Caching these values (especially
#               the inverse matrix value) can save matrix inverse
#               calculation time - if an inverse has already
#               been calculated for a matrix, it is remembered
#               and can be recalled, rather than doing the
#               calculation anew each and every time the
#               inverse of the provided matrix is desired.
#
#               Caching is achieved by making use of the
#               lexical scoping rules of R, and the <<-
#               operator.
#
#               Sub-functions for manipulation of the special
#               cached matrix object are also defined within the
#               makeCacheMatrix function, and are used by the
#               cacheSolve function.
#
# ASSUMPTIONS:  In the programming assignment specification, 
#               the assumption is that the matrix supplied 
#               is ALWAYS INVERTIBLE.
#
#               Even so, an extra check that the matrix is square
#               is included for completeness, though not required.
#
#               Note that if the matrix supplied can be assumed
#               to be invertible, that means that it is SQUARE,
#               and that it is truly INVERTIBLE, since not all
#               square matrices are invertible.
#
# EXAMPLE USE:  source("cachematrix.R")
#               m <- matrix(1:4,2,2)
#               special_m <- makeCacheMatrix(m)
#               cacheSolve(special_m)
#
# ==============================================================


# --------------------------------------------------------------
# FUNCTION: makeCacheMatrix
#
# PURPOSE:  Creates a special cached matrix.
#
#           This special matrix will have 2 cached values:
#               1. the cached matrix value
#               2. the cached inverse matrix value
#
#           It will also have 4 functions that can be used
#           to manipulate the cached matrix:
#               1. set fuction to set the cached matrix value
#               2. get function to retrieve the cached matrix 
#                  value
#               3. setinverse function to set the 
#                  cached inverse matrix value
#               4. getinverse function to retrieve the
#                  cached inverse matrix value
#
# INPUTS:   x - Normal matrix value to use to create the 
#               special cached matrix
#               If no x argument is provided by the caller,
#               the default empty matrix value is used: 
#               a 1 x 1 matrix containing NA.
#
# RETURNS:  Named list of the functions that can be used
#           to manipulate the special cached matrix created
#           by this function.
# --------------------------------------------------------------
#
makeCacheMatrix <- function(x = matrix()) {
    
    # Check that input is a matrix
    #
    if (class(x) != "matrix") {
        
        # If input is NOT a matrix, display a message
        # indicating proper usage, and return NULL
        #
        message("Input to makeCacheMatrix should be a matrix")
        return(NULL)
    }

    
    # Initialize cached inverse variable to NULL
    #
    inverse <- NULL
    
    
    # ..........................................................    
    # FUNCTION: set
    # PURPOSE:  Sets the cached matrix value.  
    #           Note that the <<- operator is used for 
    #           the assignments, and therefore the
    #           cached matrix and inverse values in the 
    #           parent environment are set (i.e. the 
    #           environment of makeCacheMatrix).
    # INPUTS:   y - value to set cached matrix variable x to
    # RETURNS:  NULL (the reset cached inverse value)
    # ..........................................................
    #
    set <- function(y) {
        
        # Set the cached matrix x value to y
        #
        x <<- y
        
        # Reset the cached inverse matrix value to NULL
        # since we are setting a new cached matrix value -
        # this is an indication that the inverse matrix
        # value needs to be recalculated
        #
        inverse <<- NULL
    }
    
    
    # ..........................................................    
    # FUNCTION: get
    # PURPOSE:  Retrieves the cached matrix value.
    #           Note that the free variable x is found
    #           in the parent environment (i.e. makeCacheMatrix),
    #           which is the cached matrix value.
    # INPUTS:   None
    # RETURNS:  Cached matrix value x
    # ..........................................................
    #
    get <- function() x
    
    
    # ..........................................................    
    # FUNCTION: setinverse
    # PURPOSE:  Sets the cached inverse value to new (passed in)
    #           value.  
    #           Note that the <<- operator is used for 
    #           the assignment, and therefore the
    #           cached inverse value found in the 
    #           parent environment (i.e. makeCacheMatrix)
    #           is set. 
    # INPUTS:   inverse_arg - value to set cached inverse 
    #           variable to.
    # RETURNS:  new inverse value (last evaluated value)
    # ..........................................................
    #
    setinverse <- function(inverse_arg) inverse <<- inverse_arg
    
    
    # ..........................................................    
    # FUNCTION: getinverse
    # PURPOSE:  Retrieves the cached inverse value.
    #           Note that the free variable inverse is found
    #           in the parent environment (i.e. makeCacheMatrix),
    #           which is the cached inverse value.
    # INPUTS:   None
    # RETURNS:  Cached inverse value (inverse)
    # ..........................................................
    #
    getinverse <- function() inverse
    
    
    # Return a named list of the functions that can be used
    # to manipulate the special cached matrix created
    # by makeCacheMatrix.
    #
    list ( set = set, get = get, 
           setinverse = setinverse, getinverse = getinverse )
    
}


# --------------------------------------------------------------
# FUNCTION: cacheSolve
#
# PURPOSE:  Returns the inverse matrix of the cached matrix x.
#           The inverse will either be retrieved from a
#           previously cached value (from a prior calculation),
#           or from a current calculation (which will also
#           set the cached value for the inverse).
#
#           The solve function is used to calculate the
#           inverse matrix.
#           
#           As per the programming assignment specifications,
#           it is assumed that the matrix supplied is
#           always invertible, although for the sake of 
#           completeness, an extra check is
#           included to make sure the supplied matrix is
#           square before trying to solve.
#
#           Note that the assumption of the supplied
#           matrix being "always invertible" implies a SQUARE
#           matrix that IS INVERTIBLE.  (Remember that 
#           not all square matrices are invertible.)
#
# INPUTS:   x - Cached Matrix (created by makeCacheMatrix)
#           ... - Additional arguments if needed
#                 (that would be passed along to the 
#                 solve function)
#
# RETURNS:  Inverse matrix of cached matrix x.
# --------------------------------------------------------------
#
cacheSolve <- function(x, ...) {
    
    # Retrieve cached inverse value
    #
    inverse <- x$getinverse()
    
    
    # If the cached inverse value is non-NULL,
    # then a cached inverse value is available
    # for immediate return to caller (without
    # having to calculate the inverse)
    #
    if (!is.null(inverse)) {
        message("Retrieving cached inverse...")
        return(inverse)
    }
    
    
    # Cached inverse value is NULL, so the inverse
    # matrix must be calculated...
    #
    
    # First, retrieve cached matrix value that we
    # have to find the inverse of.
    #
    data <- x$get()
    
    
    # Check that cached matrix value is a square matrix.
    # If it is not, display proper usage message
    # and return NULL.
    #
    # Note that this is an "extra" check (NOT required for 
    # this programming assignment):
    #
    # The programming assignment says that it can be assumed
    # that the matrix supplied is ALWAYS INVERTIBLE, so it should
    # be square.  This "extra" check is simply to display
    # an informative message to the user and return NULL
    # (in the case that the square matrix input assumption was 
    # NOT followed by the user).
    # Otherwise, the solve function would have thrown an error 
    # if the (cached) supplied matrix was not square.
    #
    data_rows <- nrow(data)
    data_cols <- ncol(data)    
    if (nrow(data) != ncol(data)) {
        message("Matrix must be square for inverse calculation.")
        message("Dimensions of matrix provided are ", 
            data_rows, "x", data_cols)
        return(NULL)
    }
    
    
    # Use the solve() function to find the inverse matrix.
    # Pass along additional arguments (...) to solve function call
    # if they were provided.
    #
    # The programming assignment says that it can be assumed that
    # the matrix supplied is ALWAYS INVERTIBLE.
    #
    # Side Note:
    # If the above mentioned assumption is NOT followed, the solve
    # function will throw an error IF the matrix provided is
    # NON-invertible.  Remember that not all square matrices are 
    # invertible.)
    #
    inverse <- solve(data, ...)
    
    
    # Set the cached inverse value to the inverse matrix calculated
    #
    x$setinverse(inverse)
    
    
    # Return the inverse matrix of the cached matrix x
    #
    inverse
    
}
