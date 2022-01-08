Golbal.List.XnSFfYra5Tkybcgy73S7onBz6ZbleB7MMA7GdBkT <- list()

Clean.Global.List <- function() Golbal.List.XnSFfYra5Tkybcgy73S7onBz6ZbleB7MMA7GdBkT <<- list()

rand <- function(){
  chars<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","a","b","c","d","e","f","g","h","i","j","k","l","n","m","p","o","q","r","s","t","u","v","w","x","y","z","0","1","2","3","4","5","6","7","8","9")
  randCode<-function() displit(chars[1+floor(length(chars)*runif(round(runif(1,20,35))))])
  randFile<-function(ext) paste(getwd(),"/file_",randCode(),ext,sep="")
  rand <<- list(randCode=randCode, randFile=randFile)
}; rand()

newVar <- function(..., def=NaN){
  n <- length(Golbal.List.XnSFfYra5Tkybcgy73S7onBz6ZbleB7MMA7GdBkT) + 1
  value <- def
  {if(length(list(...)) != 0) value<-list(...)[[1]]}
  Golbal.List.XnSFfYra5Tkybcgy73S7onBz6ZbleB7MMA7GdBkT[[n]] <<- value
  return(list(get=function(index=NaN){
    if(is.na(index[1])){
      return(Golbal.List.XnSFfYra5Tkybcgy73S7onBz6ZbleB7MMA7GdBkT[[n]])
    }else{
      return(Golbal.List.XnSFfYra5Tkybcgy73S7onBz6ZbleB7MMA7GdBkT[[n]][index])
    }
  }, 
  set=function(value, index=NaN){
    if(is.na(index[1])){
      Golbal.List.XnSFfYra5Tkybcgy73S7onBz6ZbleB7MMA7GdBkT[[n]]<<-value
    }else{
      Golbal.List.XnSFfYra5Tkybcgy73S7onBz6ZbleB7MMA7GdBkT[[n]][index]<<-value 
    }
  },
  length=function() length(Golbal.List.XnSFfYra5Tkybcgy73S7onBz6ZbleB7MMA7GdBkT[[n]])
  ))
}

newList <- function(...){
  n <- length(Golbal.List.XnSFfYra5Tkybcgy73S7onBz6ZbleB7MMA7GdBkT) + 1
  Golbal.List.XnSFfYra5Tkybcgy73S7onBz6ZbleB7MMA7GdBkT[[n]] <<- list(...)
  return(list(get=function(index=NaN){
    if(is.na(index)){
      return(Golbal.List.XnSFfYra5Tkybcgy73S7onBz6ZbleB7MMA7GdBkT[[n]])
    }else{
      return(Golbal.List.XnSFfYra5Tkybcgy73S7onBz6ZbleB7MMA7GdBkT[[n]][[index]])
    }
  }, 
  set=function(value, index)Golbal.List.XnSFfYra5Tkybcgy73S7onBz6ZbleB7MMA7GdBkT[[n]][[index]]<<-value,
  length=function() length(Golbal.List.XnSFfYra5Tkybcgy73S7onBz6ZbleB7MMA7GdBkT[[n]])
  ))
}


newCode <- function(...){
  code <- newVar("")
  add <- function(...){
    cod <- NULL
    lista <- list(...)
    for (i in 1:length(lista)) cod[i]<-paste(lista[i])
    if(code$get()==""){
      code$set(displit(cod))
    }else{
      code$set(paste(code$get(), displit(cod), sep="\n")) 
    }
  }
  if(length(list(...)) != 0){
    add(...)
  }
  run <- function(){
    file <- rand$randFile(".R")
    write(code$get(), file)
    source(file)
    unlink(file)####Ojo
  }
  return(list(add=add, getCode=code$get, run=run))
}

newFunction <- function(param){
  code <- newCode()
  add <- function(...){
    code$add(...)
  }
  getCode <- function(){
    return(paste("funny.XnSFfYra5Tkybcgy73S7onBz6ZbleB7MMA7GdBkT <- function(",param,"){\n",code$getCode(),"\n}",sep=""))    
  }
  f <- function(){
    file <- rand$randFile(".R")
    write(getCode(), file)
    source(file)
    unlink(file)####Ojo
    return(funny.XnSFfYra5Tkybcgy73S7onBz6ZbleB7MMA7GdBkT)
  }
  return(list(add=add, getCode=getCode, f=f))
}

newCount <- function(i=0, k=1, cyc=NA){
  if(!is.na(cyc)) i<-i%%cyc
    
  cont <- newVar(i)
  plus<-function()cont$set(if(!is.na(cyc)) (cont$get()+k)%%cyc else cont$get()+k)
  less<-function()cont$set(if(!is.na(cyc)) (cont$get()-k)%%cyc else cont$get()-k)
  get<-cont$get
  getPlus<-function(){
    GET<-get()
    plus()
    return(GET)
  }
  getLess<-function(){
    GET<-get()
    less()
    return(GET)
  }
  return(list(plus=plus, less=less, get=get, getPlus=getPlus, getLess=getLess))
}
