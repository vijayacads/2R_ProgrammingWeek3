## Put comments here that give an overall description of what your
## functions do

## Written a short comment describing this function
# makeCacheMatrix takes a Matrix say matrix(a) and returns multiple values,
#a list. If: test_a<-makeCacheMatrix(a) then test_a is now a function with lists.
# When we use cacheSolve on this test_a it will return the inverse of matrix(a)
# from test_a if the inverse has been already calculated. If it has not been 
#calculated then it will calculate the inverse and store it inside 'setinverse'
#inside that of makeCacheMatrix


#To test if this code works, copy this and paste it into R, then press 'Ctr+A'
#to select the whole thing and 'Ctrl+Enter'. It should give you a unitary matrix
#of a, that is defeind below in the end.

#------------------THIS IS MATRIX INVERSION---------------------------


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#-----------------------------------------------------------

## Write a short comment describing this function
#This function is called upon the list created above and will give the inverse
#of a. If its already calculated then it will give it from the cache else it will
#calculate and give the value. When it calculates inverse, it stores it in 
# 'setinverse' so that next time it can read from cache.

cacheSolveInverse <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
#--------------------------Use this to test the code works-------------

#creating a matrix a
a<-matrix(c(runif(6,0,4), runif(6,10,40), runif(6,22,23), runif(6,31,99), runif(6,33,44), runif(6,44,45) ), nrow = 6, ncol = 6)
#creating matrix(ainv) which is the inverse of a
test_a<-makeCacheMatrix(a)
ainv<-cacheSolveInverse(test_a)

#to test if it has worked, you will get a unitary matrix here:

print(round(a%*%ainv))
