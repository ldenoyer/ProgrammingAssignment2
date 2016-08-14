## ldenoyer, 14 August 2016
## Coursera R Programming, Assignment 2

## makeCacheMatrix(x) 
## x = input [square] matrix
## associates methods get, set, getinverse, setinverse with matrix x
## x$get - gets matrix x
## x$set - sets matrix x
## x$getinverse - gets x inverse
## x$setinverse - sets x inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(matrix_inverse) m<<- matrix_inverse
  getinverse<-function() m
  list(set=set, get=get,
   setinverse=setinverse,
   getinverse=getinverse)
}

## cacheSolve(x)
## x = input [square] matrix
## inverse = inverse of user's matrix x
## inverse computed using method solve()
## where %inverse% matrix = b
## in general, b can be a vector or a matrix
## but here we use default value for b = the identity matrix
## NOTE - this method only works for a square matrix, but I
##  do not check that input matrix x is square, 
##  because checking for square matrix is performed by solve()
cacheSolve <- function(x=matrix(), ...) {
    m<-x$getinverse()
    if(!is.null(m)){
      message("getting cached matrix inverse")
      return(m)
    }
    input_matrix<-x$get()
    m<-solve(input_matrix, ...)
    x$setinverse(m)
    m
}
