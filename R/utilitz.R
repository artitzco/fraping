listToVec <- function(List, index) {vec<-NULL;for (i in index) vec<-c(vec, List[[i]]);return(vec)}

subList<-function(List, index){listy <-list();for (i in 1:length(index)) listy[[i]]<-List[[index[i]]];return(listy)}

split <- function(text) {spl<-NULL; for (i in 1:nchar(text)) spl[i]<-substring(text, i, i); return(spl)}

displit <- function(spl,sp="") {text<-""; for(i in 1:length(spl)) text<-paste(text, spl[i],sp, sep="");return(substring(text,1,nchar(text)-nchar(sp)))}

reparam <- function(x0, x, y) return(y[1]+(x0-x[1])*(y[2]-y[1])/(x[2]-x[1]))

getSeconds <- function() as.numeric(proc.time()[3])

optima <- function(f, interval){
  n <- length(interval)  
  if(n==1){
    optim <- function(f, intX){
      x1 <- optimize(f, interval = intX[[1]])$minimum
      return(list(x1 = x1, min = f(x1)))
    }
  }else if(n==2){
    optim <- function(f, intX){
      f0 <- function(x1, x2) f(x1, x2)
      f1 <- function(x2) optimize(f0, x2 = x2, interval = intX[[1]])$objective
      x2 <- optimize(f1, interval = intX[[2]])$minimum
      x1 <- optimize(f0, x2 = x2, interval = intX[[1]])$minimum
      return(list(x1 = x1, x2 = x2, min = f0(x1, x2)))
    }
  }else if(n==3){
    optim <- function(f, intX){
      f0 <- function(x1, x2) f(x1, x2)
      f1 <- function(x2, x3) optimize(f0, x2 = x2, x3 = x3, interval = intX[[1]])$objective
      f0 <- function(x1, x2, x3) f(x1, x2, x3)
      f2 <- function(x3) optimize(f1, x3 = x3, interval = intX[[2]])$objective
      x3 <- optimize(f2, interval = intX[[3]])$minimum
      x2 <- optimize(f1, x3 = x3, interval = intX[[2]])$minimum
      x1 <- optimize(f0, x2 = x2, x3 = x3, interval = intX[[1]])$minimum
      return(list(x1 = x1, x2 = x2, x3 = x3, min = f0(x1, x2, x3)))
    }
  }else if(n==4){
    optim <- function(f, intX){
      f0 <- function(x1, x2) f(x1, x2)
      f1 <- function(x2, x3, x4) optimize(f0, x2 = x2, x3 = x3, x4 = x4, interval = intX[[1]])$objective
      f0 <- function(x1, x2, x3) f(x1, x2, x3)
      f2 <- function(x3, x4) optimize(f1, x3 = x3, x4 = x4, interval = intX[[2]])$objective
      f0 <- function(x1, x2, x3, x4) f(x1, x2, x3, x4)
      f3 <- function(x4) optimize(f2, x4 = x4, interval = intX[[3]])$objective
      x4 <- optimize(f3, interval = intX[[4]])$minimum
      x3 <- optimize(f2, x4 = x4, interval = intX[[3]])$minimum
      x2 <- optimize(f1, x3 = x3, x4 = x4, interval = intX[[2]])$minimum
      x1 <- optimize(f0, x2 = x2, x3 = x3, x4 = x4, interval = intX[[1]])$minimum
      return(list(x1 = x1, x2 = x2, x3 = x3, x4 = x4, min = f0(x1, x2, x3, x4)))
    }
  }else if(n==5){
    optim <- function(f, intX){
      f0 <- function(x1, x2) f(x1, x2)
      f1 <- function(x2, x3, x4, x5) optimize(f0, x2 = x2, x3 = x3, x4 = x4, x5 = x5, interval = intX[[1]])$objective
      f0 <- function(x1, x2, x3) f(x1, x2, x3)
      f2 <- function(x3, x4, x5) optimize(f1, x3 = x3, x4 = x4, x5 = x5, interval = intX[[2]])$objective
      f0 <- function(x1, x2, x3, x4) f(x1, x2, x3, x4)
      f3 <- function(x4, x5) optimize(f2, x4 = x4, x5 = x5, interval = intX[[3]])$objective
      f0 <- function(x1, x2, x3, x4, x5) f(x1, x2, x3, x4, x5)
      f4 <- function(x5) optimize(f3, x5 = x5, interval = intX[[4]])$objective
      x5 <- optimize(f4, interval = intX[[5]])$minimum
      x4 <- optimize(f3, x5 = x5, interval = intX[[4]])$minimum
      x3 <- optimize(f2, x4 = x4, x5 = x5, interval = intX[[3]])$minimum
      x2 <- optimize(f1, x3 = x3, x4 = x4, x5 = x5, interval = intX[[2]])$minimum
      x1 <- optimize(f0, x2 = x2, x3 = x3, x4 = x4, x5 = x5, interval = intX[[1]])$minimum
      return(list(x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5, min = f0(x1, x2, x3, x4, x5)))
    }
  }
  return(optim(f, interval))
}

color <- function(r=NA, g=NA, b=NA, stand=T){
  RGB<- newVar()
  setCol <- function(r=NA, g=NA, b=NA, stand=T){
    if(is.na(r)){r<-runif(1)}else if(!stand){r<-r/255}
    if(is.na(g)){g<-runif(1)}else if(!stand){g<-g/255}
    if(is.na(b)){b<-runif(1)}else if(!stand){b<-b/255}
    RGB$set(c(r,g,b))
  }
  setCol(r, g, b, stand)
  get<- function() RGB$get()
  getCol<- function(alpha=1, mult=1){
    cls<-mult*get()
    return(rgb(cls[1], cls[2], cls[3], alpha))
  }
  combine<- function(col, r=0.5){
    if(length(r)>1){
      lst<-list()
      for (i in 1:length(r)) {
        cls <- (1-r[i])*get()+r[i]*col$get()
        lst[[i]] <- color(cls[1], cls[2], cls[3])
      }
      return(colorList(col=lst))
    }
    cls <- (1-r)*get()+r*col$get()
    return(color(cls[1], cls[2], cls[3]))
  }
  
  secCol<- function(col, n){
    r<-if(n!=1)seq(0,1, 1/(n-1)) else 0.5
    listCol<- list()
    for (i in 1:n) listCol[[i]]<-combine(col, r[i])
    return(listCol)
  }
  return(list(get=get, getCol=getCol, setCol=setCol, combine=combine, secCol=secCol))
}

colorList<- function(r=0, g=0, b=0, stand=T, col=NULL){
  if(is.null(col)){
    n<-max(length(r),length(g),length(b))
    r<-r[1+((1:n)-1)%%length(r)]
    g<-g[1+((1:n)-1)%%length(g)]
    b<-b[1+((1:n)-1)%%length(b)]
    colist<-list()
    for (i in 1:n)
      colist[[i]]<-color(r[i],g[i],b[i],stand)
  }else if(is.null(col$colist)){
    if(is.null(col$get)){
      colist<-col
    }else{
      colist<-list(col)
    }
  }else{
    colist<-col$colist
  }
  N<-newCount(cyc=length(colist))
  col <- function() colist[[1+N$getPlus()]]
  getCol<- function(...) col()$getCol(...)
  return(list(colist=colist, col=col, getCol=getCol))
}
