## The following two functions will cache the calculations of the inverse of a matrix. 
##As the inverse of a matrix is a costly computation its caching will provide performance benefits


##Creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  #Sets the given matrix from the function input to add to the cache list
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #Gets the matrix input to add to the cache list
  get <- function() x
  #Computes the inverse of the matrix and assigns it to variable for input in the cache list
  setinverse <- function(solve) m <<- solve
  
  #Gets the inverse of the matrix from the list
  getinverse <- function() m
  #Creates the list of matrix and results for retrieval in the cacheSolve function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##Computes the inverse of the matrix created by makeCacheMatrix, if the inverse has already been created, 
##and the matix hasn't changed, it retrieves the result from the cache.

#Function gets a matrix as input
cacheSolve <- function(x=matrix(), ...) {
  
  #Checks if the matrix is stored in the cached result list and if it is, it gets the cached data.
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  #If the matrix wasn't stored  in the cached results list, it calculates the inverse and adds it to the list
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
