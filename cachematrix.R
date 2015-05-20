## 
A pair of functions that cache the inverse of a matrix to avoid computing over and over again.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
    m<-NULL
    set<- function(p)
     {
        x<-p
        m<-NULL
        
     }
    get<-function()
     {
        x
     }
    setInverse<-function(mat)
     {
        m<-mat
     }
    getInverse<-function()
     {
        m
     }

    list(get=get,set=set,setInverse=setInverse,getInverse=getInverse)

}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
 {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getInverse()
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix,...)
    x$setInverse(m)
    m    
}
