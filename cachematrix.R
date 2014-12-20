##This is for the second HW assignment 


## This function is used to initially create the cacheMatrix object


makeCacheMatrix <- function(x = matrix()) {      # input will be made into a matrix
  i <- NULL 
  
  #  i is the inverse and resets to NULL with the creation of each Cache Matrix
  
  get <- function() { x }   # this function returns the value of original matrix
  
  setinverse <- function(inverse)  { i <<- inverse }
  # this is called by cachesolve()
  #  access and it will store the value using superassignment
  
  getinverse <- function() { i } # this will return the cached value to cachesolve() on
  
  
  set <- function(y) {    # takes a matrix
    x <<- y         # saves the matrix 
    m <<- NULL      # resets the inverse to NULL
  }
  
  list(get = get,          #  This is accessed each time makeCacheMatrix() is called,       
       setinverse = setinverse,  #   that is, each time we make a new object.  This is a list of 
       getinverse = getinverse,  #   the internal functions ('methods') so a calling function
       set=set)     #   knows how to access those methods.      
  
}

## This function is used to get and/or set the inverse of our cache Matrix object

cacheSolve <- function(x, ...) {   # the input x is an object created by makeCacheMatrix
  i <- x$getinverse()               # accesses the object 'x' and gets the value of the inverse
  if(!is.null(i)) {              # checks if inverse has already been defined
    
    message("getting cached data")  # ... send this message to the console
    return(i)                       # ... and return the mean ... "return" ends 
    
  }
  data <- i$get()        # we reach this code only if x$getinverse() returned NULL
  i <- solve(data, ...)   # if i was NULL then we have to calculate the inverse
  i$setinverse(i)           # store the calculated inverse value in x 
  i               # return the mean to the code that called this function
}

