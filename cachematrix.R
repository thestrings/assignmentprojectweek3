#this equation starts cache equation that inverses the matrix.
#<<-: value is assigned to different environment. 
#makeCacheMatrix() is an input to cacheSolve()

makeCacheMatrix<-function(x=matrix()){
    romeozero<-NULL                                  #initializing here. 
    set<-function(y){                                #setting matrix here. 
        x<<-y
        romeozero<<-NULL
    }
    setromeo<-function(inverse) romeozero<<-inverse                        
    getromeo<-function()romeozero     
    list(set=set,get=get,setromeo=setromeo,getromeo=getromeo)  #returning lists
}


#cacheSolve() is an output of makeCacheMatrix()
#Executing the equation above. 

cacheSolve<-function(x,...){
    romeozero=x$getromeo()
    if(!is.null(romeozero)){                 #run the old equation here. 
        message("getting cached data")
        return(romeozero)                   #execute old equation if matrix is unchanged
    }
    data<-x$get()                           #getting data
    romeozero<-solve(data,...)              #run it anew. 
    x$setromeo(romeozero)
    romeozero
}

