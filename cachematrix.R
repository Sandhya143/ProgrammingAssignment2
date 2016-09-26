## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## 1.Initially given matrix is taken and inverse matrix is set to 0.
## 2. In "makeCacheMatrix" we dont compute anything. We basically 
##  set and get the values using one line functions. 
## 3. We set the functions and values in the parent environment.

makeCacheMatrix <- function(x) {
    in_mat <- NULL
    set <- function(y) {
      x <<- y
      in_mat <<- NULL
    }
    get <- function() x
    setinv <- function(solve) in_mat <<- solve
    getinv <- function() in_mat
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }

## 1. Here in "cacheSolve" we first call the getinv() function to 
##  see if the inverse is calculated.
## 2. if the inverse is calculated for this matrix data, then 
##  in_mat != NULL and thus the if condition becomes TRUE and 
##  the conditions in if loop executes and value form cache is
##  returned.
## 3. If the inverse is not calculated previously then in_mat= NULL
##  so the inverse of the given matrix is then calculated.

cacheSolve <- function(x) {
 in_mat <- x$getinv()
   if(!is.null(in_mat)) {
      message("getting cached data")
      return(in_mat)
    }
    data <- x$get()
    in_mat <- solve(data)
    x$setinv(in_mat)
    in_mat
  }

