getColor <- function(col, alpha=1, brightness=1){
  return(rgb(col[1]*brightness, col[2]*brightness, col[3]*brightness, alpha))
}

pol <- function(xlim, ylim, px, py,lx=NA,ly=NA,labX=NA,labY=NA, cex=1, background=color(0.086,0.086,0.086), col.backlines=color(.5,.5,.5), lwd=2, lty=1, alpha=1){
  mx <- (xlim[2]-xlim[1])/2
  my <- (ylim[2]-ylim[1])/2
  x <-c(xlim[1]-mx, xlim[2]+mx)
  y <-c(ylim[1]-my, ylim[2]+my)
  if(is.na(lx[1])){
    lx <- seq(xlim[1],xlim[2],(xlim[2]-xlim[1])/(px-1))  
  }
  if(is.na(ly[1])){
    ly <- seq(ylim[1],ylim[2],(ylim[2]-ylim[1])/(py-1))
  }
  if(is.na(labX[1])){
    labX<-T
  }
  if(is.na(labY[1])){
    labY<-T
  }
  polygon(c(x[1],x[1],x[2],x[2]),c(y[1],y[2],y[2],y[1]),col=background$getCol())
  mabline(v=lx, h=ly, col=col.backlines, N=lwd, lty=lty, alpha=alpha)
  axis(1,at=lx,labels=labX,cex.axis=cex)
  axis(2,at=ly,labels=labY,cex.axis=cex)
}

mplot <- function(x=0,y=0,xlim=NULL, ylim=NULL, px=6, py=6,lx=NA,ly=NA, labX=NA, labY=NA, 
                  main=NA, xlab=NA, ylab=NA, cex.main=1.3, cex.lab=1.3, cex.axis=1.1, 
                  col.background=color(0.086,0.086,0.086), col.backlines=color(.5,.5,.5), 
                  lwd.backlines=2, lty.backlines=1, alpha.backlines=1){
  if(length(x)>1 & length(y)==1){
    y <- x
    x <- 1:length(y)
  }
  if(is.null(xlim)){
    xmin <- min(x)
    xmax <- max(x)
    if(xmin==xmax){
      xlim <-c(x[1]-1, x[1]+1)
    }else{
      xlim <-c(xmin, xmax)
    }
  }
  if(is.null(ylim)){
    ymin <- min(y)
    ymax <- max(y)
    if(ymin==ymax){
      ylim <-c(y[1]-1, y[1]+1)
    }else{
      ylim <-c(ymin, ymax)
    }
  }
  plot(x, y, xlim=xlim, ylim=ylim, axes=F, main=main, xlab=xlab, ylab=ylab, cex.main=cex.main, cex.lab=cex.lab)
  pol(xlim, ylim, px, py, lx, ly, labX,labY, cex.axis, col.background, col.backlines, lwd.backlines, lty.backlines, alpha.backlines)
}

mlines <- function(x=NULL, y=NULL, lwd=2, col=color(), alpha=1, ...){
  N<-lwd
  if(is.null(y)){
    if(is.null(x)){
      x<-0
      y<-0
    }else{
      y<-x
      x<-1:length(y)
    }
  }
  sq<-1
  if(N!=1) sq<-seq(0, 1, 1.0/N)[-1]
  
  for (i in 1:N) {
    #lines(x, y, lwd=2*(N-i)+1, col="black", ...)
    lines(x, y, lwd=2*(N-i)+1, col=col$getCol(mult=sq[i], alpha=alpha), ...)
  }  
}

mpoints <- function(x=NULL, y=NULL, lwd=2, col=color(), alpha=1, ...){
  N<-lwd
  if(is.null(y)){
    if(is.null(x)){
      x<-0
      y<-0
    }else{
      y<-x
      x<-1:length(y)
    }
  }
  sq<-1
  if(N!=1)sq<-seq(0, 1, 1.0/N)[-1]
  
  for (i in 1:N) {
    #points(x, y, lwd=2*(N-i)+1, col="black", ...)
    points(x, y, lwd=2*(N-i)+1, col=col$getCol(mult=sq[i], alpha=alpha), ...)
  }  
}

mabline <- function(h=NULL,v=NULL, N=2, col=color(1,1,1), alpha=1,...){
  sq<-1
  if(N!=1) sq<-seq(0, 1, 1.0/N)[-1]
  
  mab <- function(fn, x){
    for (j in 1:length(x)) {
      for (i in 1:length(sq)) {
        fn(x[j], lwd=2*(N-i)+1, col=col$getCol(mult=sq[i], alpha=alpha), ...)
      }
    }  
  }
  mab(function(x, ...) abline(v=x, ...), v)
  mab(function(x, ...) abline(h=x, ...), h)
}

bar <- function(x, y, horizontal=T, col="darkgray", ...){
  od <- order(x)
  x <- x[od]
  y <- y[od]
  w <- abs(diff(x)/2)
  w <- c(w[1], w, w[length(w)])
  x <- x - w[1:length(x)]
  w <- w + c(w[2:length(w)], 0)
  x[length(w)] <- x[length(x)] + w[length(x)]
  for (i in 1:length(y)) {
    X <- c(x[i],x[i],x[i+1],x[i+1])
    Y <- c(0, y[i], y[i], 0)
    if(horizontal){
      polygon(X,Y, col=col, ...) 
    }else{
      polygon(Y,X, col=col, ...)
    }
  }
}


histogram <- function(x, col="darkgray", lwd=2,
                      main=NA, xlab=NA, ylab=NA, cex.main=1.3, cex.lab=1.3, cex.axis=1.1,
                      col.background=color(0.086,0.086,0.086), col.backlines=color(.5,.5,.5), 
                      lwd.backlines=2, lty.backlines=1, alpha.backlines=1){
  ht<-hist(x, ylim=c(0,1), axes=F, main=main, xlab=xlab, ylab=ylab, cex.main=cex.main, cex.lab=cex.lab)
  X<-ht$mids
  Y<-ht$counts/max(ht$counts)
  pol(c(min(ht$breaks),max(ht$breaks)),c(0,1),6,6, NA, NA, NA, NA, cex.axis, col.background, col.backlines, lwd.backlines, lty.backlines, alpha.backlines)
  bar(X,Y,lwd=lwd,col=col)
}

printer <- function(prop=3/4, width=9, height=NULL, size=c(1,1)){
  ACTIVE <- newVar(T)
  PROP <- newVar(prop)
  WIDTH <- newVar(width)
  HEIGTH <- newVar(if(is.numeric(height)) height else NA)
  SIZE <- newVar(size)
  
  PDF <- function(file=NULL, prop=NULL, width=NULL, height=NULL, size=NULL){
    if(ACTIVE$get()){
      if(is.null(file)){
        i <- 1
        while(file.exists(paste("output_plot00",i,".pdf",sep=""))){
          i <- i+1
        }
        file<-paste("output_plot00",i,".pdf",sep="")
      }
      prop <- if(is.null(prop)) PROP$get() else prop
      width <- if(is.null(width)) WIDTH$get() else width
      height <- if(is.null(height)) if(is.na(HEIGTH$get())) prop*width else HEIGTH$get() else height
      size <- if(is.null(size)) SIZE$get() else size
      pdf(file = file, width=width*size[2], height=height*size[1])
    }
  }
  OFF <- function(){
    if(ACTIVE$get()){
      dev.off()
    }
  }
  
  setActive <- function(value) ACTIVE$set(value)
  setProp <- function(prop) PROP$set(prop)
  setWidth <- function(width) WIDTH$set(width)
  setHeight <- function(height) HEIGTH$set(if(is.numeric(height)) height else NA)
  setSize <- function(size) SIZE$set(size)
  return(list(PDF=PDF, OFF=OFF, setActive=setActive, setProp=setProp, setWidth=setWidth, setHeight=setHeight, setSize=setSize))
}

tableLine <-function(tim, lin){
  x<-tim
  y<-lin
  newX<-NULL
  for (i in 1:length(x)) newX<-union(newX, x[[i]])
  
  newX<-newX[order(newX)]
  newY<-NULL
  
  for (i in 1:length(x)) {
    z<-rep(NA, length(newX))
    
    for (j in 1:length(newX)) {
      ind<-sum((1:length(x[[i]]))*(x[[i]]==newX[j]))
      if(ind!=0) z[j]<-y[[i]][ind]
    }
    minx<-min(x[[i]])
    maxx<-max(x[[i]])
    
    x1<-NA
    y1<-NA
    for (j in 1:length(newX)) {
      if(!is.na(z[j])) {
        x1<-newX[j]
        y1<-z[j]
      }else if(newX[j]>=minx & newX[j]<=maxx){
        k<-j+1
        while(is.na(z[k])) k<-k+1
        z[j] <- reparam(newX[j], c(x1, newX[k]),c(y1, z[k]))
      }
    }
    newY<- cbind(newY, z)
  }
  ind<-!is.na(apply(newY, 1, sum))
  return(list(time=newX[ind], table=newY[ind, ], apply=function(fun)apply(newY[ind, ], 1, fun)))
}

shadow<- function(x1,y1,x2,y2, col=color(0,0,0), alpha=.5, col.border=color(0,0,0), alpha.border=1){
  inv<-(length(x2):1)
  x<-c(x2[inv], x1,NA)
  y<-c(y2[inv], y1,NA)
  if(is.null(col$getCol)) col<-col[[1]]
  if(is.null(col.border$getCol)) col.border<-col.border[[1]]
  polygon(x,y, col=col$getCol(alpha), border= col.border$getCol(alpha.border))
}
