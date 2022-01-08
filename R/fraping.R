source("utilitz.R")
source("filitz.R")
source("plotitz.R")

newDataSet <- function(bleach){
  fmin<-1-bleach
  varN<-newCount()
  varSeconds      <-newVar()
  varSecondsAB    <-newVar()
  varPhoto        <-newVar()
  varFile         <-newVar()
  varFileControl  <-newVar()
  varFileBack     <-newVar()
  # :)
  base        <-newList()
  baseControl <-newList()
  baseBack    <-newList()
  area        <-newVar()
  areaControl <-newVar()
  areaBack    <-newVar()
  recoverBB        <-newList()
  recoverControlBB <-newList()
  recoverBackBB    <-newList()
  recover        <-newList()
  recoverControl <-newList()
  recoverBack    <-newList()
  timeBB <- newList()
  time   <- newList()
  recoverStandBB  <-newList()
  recoverStand    <-newList()
  recoverStandBB1 <-newList()
  recoverStand1   <-newList()
  recoverStandBB2 <-newList()
  recoverStand2   <-newList()
  recoverStandBB3 <-newList()
  recoverStand3   <-newList()
  recoverStandBB4 <-newList()
  recoverStand4   <-newList()
  
  ##Funciones
  add=function(seconds, photo, file, fileControl, fileBack){
    varN$plus()
    N <- varN$get()
    varSeconds    $set(seconds,     N)
    varPhoto      $set(photo,       N)
    varFile       $set(file,        N)
    varFileControl$set(fileControl, N)
    varFileBack   $set(fileBack,    N)
    # :)
    base       $set(data.frame(read.csv(file)),             N)
    baseControl$set(data.frame(read.csv(fileControl)),      N)
    baseBack   $set(data.frame(read.csv(fileBack)),         N)
    area       $set(base       $get(N)$Area[1], N)
    areaControl$set(baseControl$get(N)$Area[1], N)
    areaBack   $set(baseBack   $get(N)$Area[1], N)
    recoverBB       $set(base       $get(N)$IntDen, N)
    recoverControlBB$set(baseControl$get(N)$IntDen, N)
    recoverBackBB   $set(baseBack   $get(N)$IntDen, N)
    n <- nrow(base$get(N))
    recover       $set(base       $get(N)$IntDen[photo:n], N)
    recoverControl$set(baseControl$get(N)$IntDen[photo:n], N)
    recoverBack   $set(baseBack   $get(N)$IntDen[photo:n], N)
    timeBB$set(seq(0,seconds,seconds/(n-1))    , N)
    time  $set((timeBB$get(N))[1:(n-photo+1)] , N)
    varSecondsAB$set(max(time$get(N)) , N)
    REC <- recoverBB$get(N) / recoverControlBB$get(N)
    REC <- REC / REC[1]
    recoverStandBB$set(REC         , N)
    recoverStand  $set(REC[photo:n], N)
    REC <- (1-bleach)+bleach*(REC-REC[photo])/(1-REC[photo])
    recoverStandBB1$set(REC         , N)
    recoverStand1  $set(REC[photo:n], N)
    REC <- (recoverBB$get(N)/area$get(N)-recoverBackBB$get(N)/areaBack$get(N)) / 
      (recoverControlBB$get(N)/areaControl$get(N)-recoverBackBB$get(N)/areaBack$get(N))
    REC <- REC / REC[1]
    REC <- (1-bleach)+bleach*(REC-REC[photo])/(1-REC[photo])
    recoverStandBB2$set(REC         , N)
    recoverStand2  $set(REC[photo:n], N)
    REC <- (recoverBB$get(N)-recoverBackBB$get(N)) /
      (recoverControlBB$get(N)-recoverBackBB$get(N))
    REC <- REC / REC[1]
    REC <- (1-bleach)+bleach*(REC-REC[photo])/(1-REC[photo])
    recoverStandBB3$set(REC         , N)
    recoverStand3  $set(REC[photo:n], N)
    #
    REC <- recoverBB$get(N)/area$get(N)
    recoverStandBB4$set(REC         , N)
    recoverStand4  $set(REC[photo:n], N)
  }
  
  addDir=function(seconds, photo, dir, dirControl, dirBack){
    DIR  <-paste(dir       ,"/",list.files(dir)       ,sep="")
    DIRC <-paste(dirControl,"/",list.files(dirControl),sep="")
    DIRB <-paste(dirBack   ,"/",list.files(dirBack)   ,sep="")
    for (i in 1:length(DIR)) {
      add(seconds, photo, DIR[i], DIRC[i], DIRB[i])
    }
  }
  return(
    list(
      bleach  =bleach,
      fmin  =fmin,
      add   =add,
      addDir=addDir,
      n          =varN          $get,
      seconds    =varSeconds    $get,
      secondsAB  =varSecondsAB  $get,
      photo      =varPhoto      $get,
      file       =varFile       $get,
      fileControl=varFileControl$get,
      fileBack   =varFileBack   $get,
      # :)
      base       =base       $get,
      baseControl=baseControl$get,
      baseBack   =baseBack   $get,
      area       =area       $get,
      areaControl=areaControl$get,
      areaBack   =areaBack   $get,
      timeBB =timeBB$get,
      time   =time  $get,
      recover.BB       =recoverBB       $get,
      recoverControl.BB=recoverControlBB$get,
      recoverBack.BB   =recoverBackBB   $get,
      recoverStandA.BB =recoverStandBB1 $get,
      recoverStandB.BB =recoverStandBB2 $get,
      recoverStandC.BB =recoverStandBB  $get,
      recoverStandD.BB =recoverStandBB3 $get,
      recoverStandE.BB =recoverStandBB4 $get,
      recover       =recover       $get,
      recoverControl=recoverControl$get,
      recoverBack   =recoverBack   $get,
      recoverStandA =recoverStand1 $get,
      recoverStandB =recoverStand2 $get,
      recoverStandC =recoverStand  $get,
      recoverStandD =recoverStand3 $get,
      recoverStandE =recoverStand4 $get,
      color=color()
    )
  )
}

getRecover <- function(dataSet, type, BB){
  if(BB){
    code<-c("return(dataSet$recover.BB)","return(dataSet$recoverControl.BB)","return(dataSet$recoverBack.BB)","return(dataSet$recoverStandA.BB)","return(dataSet$recoverStandB.BB)","return(dataSet$recoverStandC.BB)","return(dataSet$recoverStandD.BB)","return(dataSet$recoverStandE.BB)")  
  }else{
    code<- c("return(dataSet$recover)","return(dataSet$recoverControl)","return(dataSet$recoverBack)","return(dataSet$recoverStandA)","return(dataSet$recoverStandB)","return(dataSet$recoverStandC)","return(dataSet$recoverStandD)","return(dataSet$recoverStandE)")  
  }
  f<-newFunction("dataSet")
  f$add(code[type])
  return(f$f()(dataSet))  
}

getRecoverList <- function(index, dataSet, type, BB){
  lst<-NULL
  for (i in 1:length(index))  lst[[i]]<-getRecover(dataSet, type, BB)(index[i])
  return(lst)
}

type.recover <- list(Original=1, Control=2, Back=3, StandA=4, StandB=5, StandC=6, StandD=7, StandE=8)

tableFit<- function(fit){
  param <- fit$param
  fn<- newFunction("fun")
  fn$add("lista<-NULL")
  for (i in 1:nrow(param)) {
    fn$add("  lista[[",i,"]]<-function(time) fun(time, ",displit(param[i,],sp=", "),")")
  }
  fn$add("return(lista)")
  lista<-fn$f()(fit$fun)
  
  tab<-function(time, index=NA){
    if(is.na(index)) index<-1:length(lista)
    mat<-matrix(NA, nrow=length(time), ncol=length(index))
    for (j in 1:length(index)) {
      mat[,j] <- lista[[j]](time)
    }
    rownames(mat)<- time
    colnames(mat)<- index
    return(mat)
  }
  return(tab)
}

newFit<-function(fitName, funName, paramName, paramLimit){
  A<-displit(paramLimit,sp=", ")
  B<-displit(paramName,sp=", ")
  C<-displit(paste(paramName,"=",paramName,sep=""),sp=", ")
  D<-displit(paste("limit[[",1:length(paramName),"]]",sep=""),sp=", ")
  E<-displit(paste("\"",paramName,"\"",sep=""),sp=", ")
  G<-displit(paste(paramName,"<-param$",paramName,"[i]",sep=""),sp="\n        ")
  
  fn<-newFunction(paste("dataSet, consider.area=T, limit=list(",A,"), index=NA",sep=""))
  fn$add(
    "    fmin <- dataSet$fmin
    fun <- function(time, fmax, ",B,") fmin+(fmax-fmin)*",funName,"(time, ",C,")
    error <- function(i){
      time<- dataSet$time()[[i]]
      if(consider.area){
        recover<-dataSet$recoverStandB()[[i]]
      }else{
        recover<-dataSet$recoverStandA()[[i]]
      }
      return(function(fmax, ",B,") sum(abs(fun(time, fmax, ",B,")-recover)))##B,C
    }
    fit.i <- function(i){
      optim<-optima(error(i), list(c(0,1), ",D,"))##D
      out<-listToVec(optim, index = c((length(limit)+2),1:(length(limit)+1)))
      names(out)<-c(\"error\",\"fmax\", ",E,")##E
      return(out)
    }
    fit<- function(){
      param <- NULL
      UF <- NULL
      for (i in 1:dataSet$n()) {
        param <- data.frame(rbind(param, fit.i(i)))
        fmax<-param$fmax[i]
        ",G,"
        UF[i]<- (1-fmax)/(1-fmin)
      }
      error<-param$error
      param$error<-NULL
      ret<-list(dataSet=dataSet, consider.area=consider.area, simulated=F, groupName=dataSet$groupName, fun=fun, table=NULL, param=param, error=error, fitName=\"",fitName,"\", seconds=dataSet$seconds, secondsAB=dataSet$secondsAB, fmin=fmin, UF=UF, MF=1-UF, color=dataSet$color)
      ret$table<-tableFit(ret)
      return(ret)
    }
    if(is.na(index)){
      return(fit())
    }
    return(fit.i(index))"
  )
  return(fn$f())
}

fitExp<-newFit("Exponential","pexp","rate","c(0,1)")
fitGam<-newFit("Gamma","pgamma",c("shape","rate"),c("c(0,5)","c(0,1)"))
fitWei<-newFit("Weibull","pweibull",c("shape","scale"),c("c(0,5)","c(0,1000)"))

simulate <- function(x, nsim){
  return(as.numeric(quantile(ecdf(x), runif(nsim))))
}

simParam <- function(param, nsim){
  paramMod<-newFunction("param")
  paramMod$add("return(lm(param$fmax~",displit(paste("param$",colnames(param)[-1],sep=""),sp="+"),"))")
  paramSim<-newFunction("param,N")
  paramSim$add("return(cbind(1, ",displit(paste("simulate(param$",colnames(param)[-1],",N)",sep=""),sp=", "),"))")
  N<-2*nsim
  sim <- as.matrix(paramSim$f()(param,N))
  coef<- as.matrix(paramMod$f()(param)$coefficients)
  sim[,1]<-sim%*%coef
  sim<-data.frame(sim[sim[,1]>=min(param[,1])&sim[,1]<=max(param[,1]),][1:nsim,])
  colnames(sim)<-colnames(param)
  rownames(sim)<-NULL
  return(sim)
}

simFit<- function(fit, nsim=50){
  param<- simParam(fit$param, nsim)
  UF<-simulate(fit$UF, nsim)
  MF<-1-UF
  ret<-list(dataSet=fit$dataSet, consider.area=fit$consider.area, simulated=T, groupName=fit$groupName, fun=fit$fun, table=NULL, param=param, seconds=fit$seconds, secondsAB=fit$secondsAB, fmin=fit$fmin, UF=UF, MF=MF,color=fit$color)
  ret$table<-tableFit(ret)
  return(ret)
}

compareMobilFraction <- function(fitA, fitB){
  test <- t.test(fitA$MF, fitB$MF)
  p.value <-test$p.value
  quantileA<-function(probs=seq(0,1,0.01))quantile(fitA$MF, probs=probs)
  quantileB<-function(probs=seq(0,1,0.01))quantile(fitB$MF, probs=probs)
  index<-fitA$simulated+fitB$simulated
  if(index==0){
    col=color(1, 0.5, 0)
  }else if(index==1){
    col=color(0.750,0.315,0.25)
  }else{
    col=color(0.5, 0.13, 0.5)
  }
  col<-(fitA$color$combine(fitB$color))$combine(col, 0.6)
  return(list(t.test=test, p.value=p.value, quantileA=quantileA, quantileB=quantileB, color=col))
}


compareUnmobilFraction <- function(fitA, fitB){
  test <- t.test(fitA$UF, fitB$UF)
  p.value <-test$p.value
  quantileA<-function(probs=seq(0,1,0.01))quantile(fitA$UF, probs=probs)
  quantileB<-function(probs=seq(0,1,0.01))quantile(fitB$UF, probs=probs)
  index<-fitA$simulated+fitB$simulated
  if(index==0){
    col=color(1, 0.5, 0)
  }else if(index==1){
    col=color(0.750,0.315,0.25)
  }else{
    col=color(0.5, 0.13, 0.5)
  }
  col<-(fitA$color$combine(fitB$color))$combine(col, 0.6)
  return(list(t.test=test, p.value=p.value, quantileA=quantileA, quantileB=quantileB, color=col))
}

compareMeanCurve <- function(fitA, fitB, time=NULL){
  if(is.null(time)){
    time <- seq(0, max(fitA$secondsAB(), fitB$secondsAB()),0.5)    
  }
  p.value <- function(){
    tableA<-fitA$table(time)
    tableB<-fitB$table(time)
    p.value<-newVar()
    for (i in 1:length(time)) {
      tryCatch( 
        {
          p.value$set(t.test(tableA[i,], tableB[i,])$p.value, i)
        }, error=function(cond) {
          p.value$set(NA, i)
        }
      )  
    }
    return(p.value$get(1:p.value$length()))
  }
  index<-fitA$simulated+fitB$simulated
  if(index==0){
    col=color(1, 0.5, 0)
  }else if(index==1){
    col=color(0.750,0.315,0.25)
  }else{
    col=color(0.5, 0.13, 0.5)
  }
  col<-(fitA$color$combine(fitB$color))$combine(col, 0.6)
  return(list(p.value=p.value(), time=time, color=col))
}

#############
####plots####
#############

shadowRecover <- function(dataSet, index=NULL, type=type.recover$StandB, BB=T,
                          col.shadow=NULL, alpha.shadow=.4,
                          col.border=NULL, alpha.border=1,
                          col.mean=NULL, lwd.mean=2, lty.mean=1){
  if(is.null(col.shadow)) col.shadow<-dataSet$color
  if(is.null(col.mean)) col.mean<-dataSet$color
  if(is.null(col.border)) col.border <-dataSet$color
  
  if(is.null(index)) index <- 1:dataSet$n()
  
  if(BB) tim<-dataSet$timeBB() else tim<-dataSet$time()
  
  tab<-tableLine(subList(tim,index), getRecoverList(index, dataSet, type, BB))
  
  plot.shadow<- function() shadow(tab$time, tab$apply(min), tab$time, tab$apply(max), col=col.shadow, alpha = alpha.shadow, col.border =col.border,alpha.border = alpha.border)
  
  plot.mean<- function() mlines(tab$time, tab$apply(mean), lwd=lwd.mean, col=col.mean, lty=lty.mean)
  
  return(if(length(index)<=1) list(plot.shadow=function() NULL, plot.mean=function() NULL)
         else list(plot.shadow=plot.shadow, plot.mean=plot.mean))
}



linesRecover <- function(dataSet, index=NULL, col=NULL, type=type.recover$StandB, BB=T, alpha.line=1, lwd.line=1, lty.line=1){
  if(is.null(col))col<-dataSet$color
  
  if(is.null(index)) index <- 1:dataSet$n()
  
  for (j in index) {
    if(BB) tim<-dataSet$timeBB()[[j]] else tim<-dataSet$time()[[j]]
    
    mlines(tim, getRecover(dataSet, type, BB)(j), col=col, alpha=alpha.line, lwd=lwd.line, lty=lty.line)
  }
}

pointsRecover <- function(dataSet, index=NULL, col=NULL,type=type.recover$StandB, BB=T, alpha.point=1, lwd.point=2){
  if(is.null(col))col<-dataSet$color
  
  if(is.null(index)) index <- 1:dataSet$n()
  
  for (j in index) {
    if(BB) tim<-dataSet$timeBB()[[j]] else tim<-dataSet$time()[[j]]
    
    mpoints(tim, getRecover(dataSet, type, BB)(j), col=col, alpha=alpha.point, lwd=lwd.point)
  }
}

plotRecover <- function(..., type=type.recover$StandB, BB=T, plot.point=F, plot.line=T,
                        xlim=NULL, ylim=NULL, main=NA, xlab="Time (min)", ylab="Fluorescence", cex.main=1.3, cex.lab=1.3, cex.axis=1.2,
                        col.line=NULL, col.point=NULL, col.background=color(1,1,1), col.backlines=color(.7,.7,.7), lwd.backlines=1, 
                        lty.backlines=1, alpha.backlines=1, alpha.line=1, lwd.line=1, lty.line=1,
                        alpha.point=1, lwd.point=1, col.area=NULL, 
                        plot.shadow=F, plot.mean=F,
                        col.shadow=NULL, alpha.shadow=.4,
                        col.border=NULL, alpha.border=1,
                        col.mean=NULL, lwd.mean=2, lty.mean=1){
  dataSetList <- list(...)
  seconds <- 0
  minRec <- Inf
  maxRec <- -Inf
  maxTim <- -Inf
  n<-0
  area<-NULL
  for (i in 1:length(dataSetList)) {
    dataSet<-dataSetList[[i]]
    seconds<- max(seconds, dataSet$seconds())
    n<-n+dataSet$n()
    area<-c(area, dataSet$area())
    for (j in 1:dataSet$n()) {
      if(BB)tim<-dataSet$timeBB()[[j]] else tim<-dataSet$time()[[j]]
      
      rec <- getRecover(dataSet, type, BB)(j)
      minRec <-min(minRec, rec)
      maxRec <-max(maxRec, rec)
      maxTim<-max(maxTim,tim)
    }
  }
  if(is.null(xlim)) xlim<-c(0, round(maxTim))
  
  if(is.null(ylim)) ylim<-c(round(minRec*10)/10, round(maxRec*10)/10)
  
  if(!is.null(col.area)){
    r<-seq(0, 1,1/(n-1))[order(area, decreasing = T)]
    col<-col.area[[2]]$combine(col.area[[1]], r=r)
    col.line<-col
    col.point<-col
  }
  
  mplot(xlim=xlim, ylim=ylim, main=main, xlab=xlab, ylab=ylab, cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col.background=col.background, col.backlines=col.backlines, lwd.backlines=lwd.backlines, lty.backlines=lty.backlines, alpha.backlines=alpha.backlines)
  
  listShad <-list()
  if(plot.shadow | plot.mean){
    for (i in 1:length(dataSetList)){
      listShad[[i]] <- shadowRecover(dataSetList[[i]],index=NULL, type=type, BB=BB,
                                     col.shadow=col.shadow, alpha.shadow=alpha.shadow,
                                     col.border=col.border, alpha.border=alpha.border,
                                     col.mean=col.mean, lwd.mean=lwd.mean, lty.mean=lty.mean)
      if(plot.shadow) listShad[[i]]$plot.shadow()
    }
  }
  
  for (i in 1:length(dataSetList)) {
    dataSet<-dataSetList[[i]]
    if(plot.line) linesRecover(dataSet, col=col.line,type=type, BB=BB, alpha=alpha.line, lwd=lwd.line, lty=lty.line)
    
    if(plot.point) pointsRecover(dataSet, col=col.point,type=type, BB=BB, alpha=alpha.point, lwd=lwd.point)
  }
  
  if(plot.mean) for (i in 1:length(dataSetList)) listShad[[i]]$plot.mean()
}

compareFit <- function(dataSet, ..., rgbCol=NULL,cex.main=1.3, cex.lab=1.3, cex.axis=1.2, col.background=color(1,1,1), col.backlines=color(.7,.7,.7), 
                       lwd.backlines=1, lty.backlines=1, alpha.backlines=1,
                       col.QQ=color(1,1,0), col.refLine=color(1,0,0), col.mean=color(0.71,0.58,0.75),
                       alpha.QQ=1, alpha.refLine=1, alpha.mean=1,
                       lwd.QQ=2, lwd.refLine=2, lwd.mean=2,
                       lty.QQ=1, lty.refLine=2, lty.mean=2,
                       cex.mean=1.1, digits.mean=3){
  mod<-list(...)
  n<-length(mod)
  if(is.null(rgbCol)){
    rgbCol <- rainbow(n, alpha=0.5)
  }
  fits<-NULL
  error<-NULL
  quant<-NULL
  mn <- Inf
  mx <- -Inf
  for (i in 1:n) {
    fits[[i]] <- mod[[i]](dataSet)
    error[[i]] <- fits[[i]]$error
    quant[[i]] <- quantile(error[[i]], seq(0,1,0.01))
    mn <- min(mn, error[[i]])
    mx <- max(mx, error[[i]])
  }
  mn <- round(mn*100)/100
  mx <- round(mx*100)/100
  r<-(mx-mn)/2
  par(mfrow=c(n,n))
  for (i in 1:n) {
    nameI<-fits[[i]]$fitName
    for (j in 1:n) {
      nameJ<-fits[[j]]$fitName
      if(i==j){
        histogram(error[[i]], col=rgbCol[i], ylab="Density",xlab="Error", main=paste("Histogram of error\n","(",nameI,")",sep=""), cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col.background=col.background, col.backlines=col.backlines, lwd.backlines=lwd.backlines, lty.backlines=lty.backlines, alpha.backlines=alpha.backlines)
        mabline(v=mean(error[[i]]), N=lwd.mean, col=col.mean, alpha=alpha.mean, lty=lty.mean)
        meantext<- trunc(mean(error[[i]])*10^digits.mean)/10^digits.mean
        legend("topright",paste("mean=",meantext,sep=""), bg="white" , lwd=4 ,lty=1,col=rgb(.2,.2,.2), cex=cex.mean)
        legend("topright",paste("mean=",meantext,sep=""), lwd=2 ,lty=1,col= col.mean$getCol(), cex=cex.mean)
      }else{
        mplot(xlim=c(mn,mx), ylim=c(mn,mx), xlab=nameJ, ylab=nameI, main="QQ-Plot", cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col.background=col.background, col.backlines=col.backlines, lwd.backlines=lwd.backlines, lty.backlines=lty.backlines, alpha.backlines=alpha.backlines)
        mlines(c(mn-r,mx+r),c(mn-r,mx+r), col=col.refLine,lwd=lwd.refLine, alpha=alpha.refLine, lty=lty.refLine)
        mlines(quant[[j]],quant[[i]], col=col.QQ, lwd=lwd.QQ, alpha=alpha.QQ, lty=lty.QQ)
      }
    }
  }
  par(mfrow=c(1, 1))
}


shadowFit <- function(fit, time=NULL, index=NULL,
                          col.shadow=NULL, alpha.shadow=.4,
                          col.border=NULL, alpha.border=1){
  
  if(is.null(time)) time <- seq(0, max(fit$secondsAB()), 0.5)
  if(is.null(col.shadow)) col.shadow <-fit$color
  if(is.null(col.border)) col.border <-fit$color
  tabFit <-fit$table(time)
  if(is.null(index)) index <- 1:ncol(tabFit)
  tim<-list()
  lin<-list()
  for (i in 1:length(index)) {
    tim[[i]]<-time
    lin[[i]]<-tabFit[ ,index[i]]
  }
  
  tab<-tableLine(tim, lin)
  
  plot.shadow<- function() shadow(tab$time, tab$apply(min), tab$time, tab$apply(max), col=col.shadow, alpha=alpha.shadow, col.border=col.border, alpha.border=alpha.border)
  
  return(if(length(index)<=1) list(plot.shadow=function() NULL)
         else list(plot.shadow=plot.shadow))
}

linesFit <- function(fit, time=NULL, index=NULL, col=NULL, alpha.fit=1, lwd.fit=1, lty.fit=1){
  if(is.null(time)) time <- seq(0, max(fit$secondsAB()), 0.5)
  tab <-fit$table(time)
  
  if(is.null(col)) col <- fit$color
  
  if(is.null(index)) index <- 1:ncol(tab)
    
  for (j in index) mlines(time, tab[,j], col=col, alpha=alpha.fit, lwd=lwd.fit, lty=lty.fit)
}

linesMean <- function(fit, time=NULL, col=NULL, alpha.mean=1, lwd.mean=2, lty.mean=1){
  if(is.null(col)) col <- fit$color
  
  if(is.null(time)) time <- seq(0, max(fit$secondsAB()), 0.5)
  
  tab <-fit$table(time)
  mlines(time, apply(tab , MARGIN=1, FUN = mean), col=col, alpha=alpha.mean, lwd=lwd.mean, lty=lty.mean)
}

plotFit <- function(..., time=NULL, plot.fit=T, plot.mean=F, plot.point=F, plot.line=F, plot.shadow=F,
                    xlim=NULL, ylim=NULL, main=NA, xlab="Time (min)", ylab="Fluorescence", cex.main=1.3, cex.lab=1.3, cex.axis=1.2,
                    col.background=color(1,1,1), col.backlines=color(.7,.7,.7), lwd.backlines=1,
                    lty.backlines=1, alpha.backlines=1,
                    alpha.fit=1, lwd.fit=1, lty.fit=1, alpha.mean=1, lwd.mean=2, lty.mean=1, alpha.line=1, lwd.line=1, lty.line=1,
                    alpha.point=1, lwd.point=1,
                    col.shadow=NULL, alpha.shadow=.4,
                    col.border=NULL, alpha.border=1){
  fitList<- list(...)
  tmax<- -Inf
  fmin<- Inf
  shadList<-list()
  for (i in 1:length(fitList)) {
    fit<-fitList[[i]] 
    tmax <- max(tmax, max(fit$secondsAB()))
    fmin <- min(fmin, fit$fmin)
  }
  
  if(is.null(time)) time <- seq(0, tmax, 0.5)
  
  if(is.null(xlim)) xlim<-c(min(time),max(time))
  
  if(is.null(ylim)) ylim<-c(fmin,1)
  
  for (i in 1:length(fitList)) if(plot.shadow) shadList[[i]] <- shadowFit(fitList[[i]], time=time,index=NULL,col.shadow=col.shadow, alpha.shadow=alpha.shadow,col.border=col.border, alpha.border=alpha.border)
  
  mplot(xlim=xlim, ylim=ylim, main=main, xlab=xlab, ylab=ylab, cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col.background=col.background, col.backlines=col.backlines, lwd.backlines=lwd.backlines, lty.backlines=lty.backlines, alpha.backlines=alpha.backlines)
  
  if(plot.shadow) for (i in 1:length(fitList)) shadList[[i]]$plot.shadow()
  
  for (i in 1:length(fitList)) {
    dataSet<-fitList[[i]]$dataSet
    if(fitList[[i]]$consider.area) type<- type.recover$StandB
      else type<- type.recover$StandA
      
    if(plot.line) linesRecover(dataSet, type=type, BB=F, alpha=alpha.line, lwd=lwd.line, lty=lty.line)
    
    if(plot.point) pointsRecover(dataSet, type=type, BB=F, alpha=alpha.point, lwd=lwd.point)
  }
  
  if(plot.fit)
    for (i in 1:length(fitList))
      linesFit(fitList[[i]], time, alpha.fit=alpha.fit, lwd.fit=lwd.fit, lty.fit=lty.fit)
  
  if(plot.mean)
    for (i in 1:length(fitList))
      linesMean(fitList[[i]], time,alpha.mean=alpha.mean, lwd.mean=lwd.mean, lty.mean=lty.mean)
}

linesCF <- function(CF, col=NULL, alpha.QQ=1, lwd.QQ=2, lty.QQ=1){
  if(is.null(col)) col<-CF$color
  mlines(CF$quantileA(), CF$quantileB(), col=col, alpha=alpha.QQ, lwd=lwd.QQ, lty=lty.QQ)
}

plotCF <- function(..., plot.refLine=T, plot.line=T,
                   xlim=NULL, ylim=NULL, main="QQ-Plot", xlab=NA, ylab=NA, cex.main=1.3, cex.lab=1.3, cex.axis=1.2,
                   col.background=color(1,1,1), col.backlines=color(.7,.7,.7), lwd.backlines=1,
                   alpha.QQ=1, lwd.QQ=2, lty.QQ=1, lty.backlines=1, alpha.backlines=1,
                   col.refLine=color(1,0,0), alpha.refLine=1, lwd.refLine=2, lty.refLine=2){
  CFList <- list(...)
  mn<- Inf
  mx<--Inf
  for (i in 1:length(CFList)) {
    CF <- CFList[[i]]
    mn <- min(mn, CF$quantileA(), CF$quantileB())
    mx <- max(mx, CF$quantileA(), CF$quantileB())
  }
  lim <- round(c(mn, mx)*100)/100
  if(is.null(xlim)) xlim<-lim
  
  if(is.null(ylim)) ylim<-lim
  
  mplot(xlim=xlim, ylim=ylim, main=main, xlab=xlab, ylab=ylab, cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col.background=col.background, col.backlines=col.backlines, lwd.backlines=lwd.backlines, lty.backlines=lty.backlines, alpha.backlines=alpha.backlines)
  if(plot.refLine)
    mlines(c(-1,2), c(-1,2), col=col.refLine,lwd=lwd.refLine, lty=lty.refLine)
  
  if(plot.line)
    for (i in 1:length(CFList)) 
      linesCF(CFList[[i]], alpha.QQ=alpha.QQ, lwd.QQ=lwd.QQ, lty.QQ=lty.QQ)
    
}

linesCMC <- function(CMC, col=NULL, alpha.Pvalue=1, lwd.Pvalue=2, lty.Pvalue=1){
  if(is.null(col)) col<-CMC$color
  
  mlines(CMC$time, CMC$p.value, col=col, alpha=alpha.Pvalue, lwd=lwd.Pvalue, lty=lty.Pvalue)
}

plotPvalue <- function(..., refLine=0.05, plot.refLine=T, plot.Pvalue=T,
                       xlim=NULL, ylim=NULL, main="P-Value", xlab="Time (min)", ylab="Probability", cex.main=1.3, cex.lab=1.3, cex.axis=1.2,
                       col.background=color(1,1,1), col.backlines=color(.7,.7,.7), lwd.backlines=1,
                       alpha.Pvalue=1, lwd.Pvalue=2, lty.Pvalue=1, lty.backlines=1, alpha.backlines=1,
                       col.refLine=color(1,0,0), alpha.refLine=1, lwd.refLine=2, lty.refLine=2){
  CMCList <- list(...)
  mint <- Inf
  maxt <--Inf
  for (i in 1:length(CMCList)) {
    CMC <- CMCList[[i]]
    mint <- min(mint, min(CMC$time))
    maxt <- max(maxt, max(CMC$time))
  }
  if(is.null(xlim)){
    xlim<-c(mint,maxt)
  }
  if(is.null(ylim)){
    ylim<-c(0,1)
  }
  mplot(xlim=xlim, ylim=ylim, main=main, xlab=xlab, ylab=ylab, cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col.background=col.background, col.backlines=col.backlines, lwd.backlines=lwd.backlines, lty.backlines=lty.backlines, alpha.backlines=alpha.backlines)
  if(plot.refLine){
    mabline(h=refLine, lty=lty.refLine, col=col.refLine, alpha=alpha.refLine, N=lwd.refLine)
  }
  if(plot.Pvalue){
    for (i in 1:length(CMCList)) {
      linesCMC(CMCList[[i]], alpha.Pvalue=alpha.Pvalue, lwd.Pvalue=lwd.Pvalue, lty.Pvalue=lty.Pvalue)
    }
  }
}
