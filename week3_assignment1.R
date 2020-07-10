makeCacheMatrix<-function(m=matrix())
{
  inv=NULL
  set<-function(m1){
    m<<-m1
    inv<<-NULL
  }
  get<-function() m
  setinv<-function(inverse)
  {
    inv<<-inverse
  }
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  print("hai")
  print(m)
  print(inv)
}
m<-matrix(rep(1,4),2,2)
x<-makeCacheMatrix(m)

cacheSolve<-function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
y<-cacheSolve(x)
print(m)
print(y)
