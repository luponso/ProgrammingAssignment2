## Programming Assignment 2: Lexical Scoping
## by Luponso
##
#############################################################################
## Function: makeCacheMatrix()
## Description: ##
## This function creates a special /
## "matrix" object that can cache its inverse.
## Arguments: 
## X: a square numeric or complex matrix containing the coefficients of the /
## linear system. Logical matrices are coerced to numeric.

makeCacheMatrix <- function(X = matrix()) {
  inv <- NULL
  set <- function(Y){             # Set a matrix X in the /
    X <<- Y                 # parent environment.
    inv <<- NULL            # Clean the inv of X value.
  }
  get <- function() {             # Returns the matrix X.
    X
  }
  setinv <- function(solve) {     # Cache the inv of X values.
    inv <<- solve
  }
  getinv <- function() {          # Return the cached inv of X values.
    inv
  }
  list (set = set, get = get,
        getinv = getinv, 
        setinv = setinv)          # List the functions in order to call /
}                                       # them from the cacheSolve function.

#############################################################################
## Function: cacheSolve 
## Description: ##
## This function computes the inverse of the special "matrix" returned by /
## makeCacheMatrix above. If the inverse has already been calculated (and /
## the matrix has not changed), then the cachesolve should retrieve the /
## inverse from the cache.
## Arguments: 
## X <- makeCacheMatrix(), ... (list of functions defined in makeCacheMatrix()) (?)

cacheSolve <- function(X, ...) {        
  inversa <- X$getinv()                   # Assigns the value cached in X$getinv() /
  if (!is.null(inversa)){                 # to the inversa variable. If the value is not NULL, /
    message("Getting cached data")  # returns this cached value, along with
    return(inversa)                 # the message "Geting cached data"
  }
  data <- X$get()                 # Retrive the data stored in X$get.
  inversa <- solve(data)          # Calculates its inverse.
  X$setinv(inversa)               # Stores the result in the parent environment
  inversa                         # Return the result: inv of X
}

### EXAMPLES ###
## > a <- matrix(rnorm(9), 3)
## > A <- makeCacheMatrix(a)
## > A$get()
##      [,1]        [,2]        [,3]
## [1,] 1.1025172 -1.33869145 -0.03284794
## [2,] 0.5524071  0.05210693  0.68346488
## [3,] 1.1441977 -0.94470445 -3.33762100
## > cacheSolve(A)
##      [,1]      [,2]       [,3]
## [1,] -0.1585298 1.4910128  0.3068839
## [2,] -0.8823545 1.2239240  0.2593142
## [3,]  0.1954011 0.1647182 -0.2678073
## > b <- matrix(1:4, 2)
## > B <- makeCacheMatrix(b)
## > B$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(B)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(A)
## Getting cached data
##      [,1]      [,2]       [,3]
## [1,] -0.1585298 1.4910128  0.3068839
## [2,] -0.8823545 1.2239240  0.2593142
## [3,]  0.1954011 0.1647182 -0.2678073
## > cacheSolve(B)
## Getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5