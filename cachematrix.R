## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix function takes the Input Matrix and contains function 
##which sets and gets the matrix inverse if already exists

makeCacheMatrix <- function (x = matrix()) {
  ## This is the Input Matrix assigning initial value as  NULL
  
  Inv_Matrix <- NULL
  
  ## settting the value of  set function element value to be equal to input matrix from global env 
  set<- function(y) {
    
    x <<- y
    
    Inv_Matrix <<- NULL
    
  } 
  
  ## getting the Inv_Matrix
  
  get<- function() x
  
  ##setting the 
  setInverseMatrix <- function (matrix) Inv_Matrix <<- matrix
  
  getInverseMatrix <- function () Inv_Matrix
  
  list(set=set , get = get , setInverseMatrix = setInverseMatrix , getInverseMatrix = getInverseMatrix)
  
}



## cacheSolve function checks the input matrix in above function 
##already exists in getInverseMatrix of above function and if exists get the value else calculate the inverse of the input matrix
## and print the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Get the Matrix if already in the Cache
  
  Inv_Matrix <- x$getInverseMatrix()
  
  
  ## First Find if already exists
  
  if (!is.null (Inv_Matrix)  ) {
    
    print ("This Inverse Matrix is from Cache")
    
    ## Skip the Computation and return the value from the Cache
    
    return(Inv_Matrix)
    
  }
  

  
  ## Else proceed with the computation and find the inverse of inputted Matrix 
  data <- x$get()
  
 ## Find the Inverse of the Matrix
  
  Inv_Matrix <- solve(data, ...)
  
  ## Add the Inver Matrix to the Cache
  
  x$setInverseMatrix(Inv_Matrix)
  

  ## Output the Inverse Matrix

  Inv_Matrix
    
  
}
