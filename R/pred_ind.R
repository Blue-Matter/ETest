
bclnorm = function (reps, mu, cv){
  if (all(is.na(mu)))
    return(rep(NA, reps))
  if (all(is.na(cv)))
    return(rep(NA, reps))
  if (reps == 1)
    return(mu)
  return(rlnorm(reps, bcmconv(mu, mu * cv), bcsdconv(mu, mu * cv)))
}

bcmconv = function (m, sd)  log(m) - 0.5 * log(1 + ((sd^2)/(m^2)))
bcsdconv = function (m, sd)   (log(1 + ((sd^2)/(m^2))))^0.5

emp_comp_plot = function(sdata, pred){
  par(mfrow=c(1,1),mai=c(0.9,0.9,0.05,0.05))
  yrs = as.numeric(sapply(row.names(sdata[[2]]),function(x){strsplit(x,"_")[[1]][2]}))
  ny = length(yrs)
  LB = UB = rep(NA,length(yrs))
  for(i in 1:ny){
    LB[i]= qlnorm(0.05,log(sdata$Brel$Value[i]),sdata$Brel$StdDev[i])
    UB[i] = qlnorm(0.95,log(sdata$Brel$Value[i]),sdata$Brel$StdDev[i])
  }
  dat = cbind(LB,sdata$Brel$Value,UB)
  matplot(yrs,dat,ylim=c(0,max(dat)),col=c("darkgrey","black","darkgrey"),type="l", lty=1,lwd=2,xlab="Year",ylab="SSB/SSBMSY"); grid()

  pred_CI = quantile(pred,c(0.05,0.95))
  ly = yrs[ny]
  lines(c(ly,ly),c(pred_CI[1],pred_CI[2]),col="red",lwd=2)
  points(ly,mean(pred),pch=19,cex=1.1,col="red")

}


pred_ind = function(Ind){
  model = Ind$model
  sdata = Ind$sdata
  pred = exp((model %>% predict(sdata[[1]]))[,1])
  if(length(sdata)>1){
    emp_comp_plot(sdata, pred)
  }
}


