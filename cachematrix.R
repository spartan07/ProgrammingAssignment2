


#Takes a matrix as input and return a list of parameters for matrix manipulation and cached inverse 
# which is retrieved or set by cachematrix()

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  get<-function()
  {
    x
  }
  setinv<-function(xinv)
  {
    inv<<-xinv
  }
  getinv<-function
{
  inv
}
list(set=set,get=get,setinv=setinv,getinv=getinv)
}

# Takes a matrix created by makecachematrix() and returns cached inverse or newly solved inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv))
  {
    # Returning cached inverse
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat, ...)
  x$setinv(inv)
  inv
}
