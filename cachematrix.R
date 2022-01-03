## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { #creating the function makeCacheMatrix that takes a matrix x as an argument
  inv<-NULL
  setting<<-function(y){ #creating a function in a function so using <<-
    x<<-y
    inv<<-NULL 
  }
  getting<-function(){x} #getting the function with matrix x
  settingInv<-function(inverse){inv<<-inverse} #setting a value for inv
  gettingInv<-function(){inv} #getting the inverse
  list(setting=setting,getting=getting,settingInv=settingInv,gettingInv=gettingInv)
  #creating list for the same
}

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$gettingInv()
  if(!is.null(inv)) { #checking to see if the inverted matrix is NULL
    message('The cached data is') 
    return(inv)
  }
  m<-x$getting() #returning the matrix
  inv<-solve(m,...) #solving the matrix to return the inverse of the makeCacheMatrix
  x$settingInv(inv) 
  inv #printing the inverted matrix
}
