## Put comments here that give an overall description of what your
## functions do

# cachematrix.R

# SH
# Jan 2016
# Coursera course R Programming Project#2
#
# Functions included:
# makeCacheMatrix - create a special matrix obj
# cacheSolve - return the inverse of a matrix
# proj2_test - test above two functions

# Usage of "proj2_test":
# >source("cachematrix.R")
# >proj2_test()


########################################################

makeCacheMatrix <- function(x = matrix()) {
  # This function creates a special "matrix" object 
  # that can cache its inverse. Its member functions include
  # 1. set (set the value of the "matrix")
  # 2. get (get the value of the "matrix")
  # 3. setinv (get the value of the inverse of the matrix)
  # 4. getinv (get the value of the inverse of the matrix)

  
  m <- NULL # m is the inverse of the matrix
  set <- function(y) {
    x <<- y     # assign value to obj. in another env.
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


########################################################

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()   # check and see if the value exists
  if ( !is.null(m) ) {  # yes
    message("getting cached data")
    return (m)  # skip rest of code
  }
  
  # the value does not exist
  message("inverse not in cache. solve now ...")
  data <- x$get() 
  m <- solve(data, ...)  # calc. matrix inverse
  x$setinv(m)
  
  return (m)  
  
}

########################################################

proj2_test <- function(mat1=matrix(c(1,0,5,2,1,6,3,4,0),nrow=3,ncol=3)) {
  
  source("makeCacheMatrix.R")
  source("cacheSolve.R")
  
  message("Original matrix:")
  print(mat1)
  
  message("Test 1. Try to cache matrix inverse that does not exist ...")
  
  res <- try( t2 <- cacheSolve(t) )
  if(inherits(res, "try-error"))
  {
    #error handling code
    message("Calculate inverse ...")
    t <- makeCacheMatrix(mat1) # t is now created
    t2 <- cacheSolve(t) # inverse of t is calculated and stored in cache
    message("inverse of matrix:")
    print(t2)
    #       [,1] [,2] [,3]
    # [1,]  -24   18    5
    # [2,]   20  -15   -4
    # [3,]   -5    4    1    
  }
  
  #rest of iteration for case of no error
  
  # Test a case when the inverse is already in the cache
  message("Test 2. A case with matrix inverse stored in cache ...")
  t2 <- cacheSolve(t) # t2 is the inversed value stored in cache
  # getting cached data # !!!this string is the screen output!!
  print(t2) 
  #       [,1] [,2] [,3]
  # [1,]  -24   18    5
  # [2,]   20  -15   -4
  # [3,]   -5    4    1
  
}
