#library(ETest)


train_data_comp = function(train, strain,tdata, sdata){
  # transformed data
  mintrain = apply(train,2,min)
  maxtrain = apply(train,2,max)
  datamu = apply(tdata[[1]],2,mean)

  # standardized data
  minstrain = apply(strain,2,min)
  maxstrain = apply(strain,2,max)
  sdatamu = apply(sdata[[1]],2,mean)

  data.frame(
    mu_trainorg = round(apply(train,2,mean),3),
    min_trainorg = round(mintrain,3),
    max_trainorg = round(maxtrain,3),
    mu_data = round(datamu,3),
    mu_strain = round(apply(strain,2,mean),3),
    min_strain = round(minstrain,3),
    max_strain = round(maxstrain,3),
    mu_sdata = round(sdatamu,3),
    inrange = sdatamu>minstrain & sdatamu<maxstrain)
}


data_comp=function(td, data){

  mutd = apply(td[,2:ncol(td)],2,mean)
  mintd = apply(td[,2:ncol(td)],2,quantile,p=0.05)
  maxtd = apply(td[,2:ncol(td)],2,quantile,p=0.95)
  mudata = apply(data[[1]],2,mean)

  data.frame(mutd=round(mutd,3),
             mintd=round(mintd,3),
             maxtd=round(maxtd,3),
             mudata=round(mudata,3),
             inside = mudata<maxtd & mudata>mintd)

}
# data = Shark_1_data; nodes = c(4, 2); nepoch = 20; plot = T; model=NULL; model_savefile=NA; validation_split=0.1; test_split = 0.1; lev=c(0.5,1)

train_ind=function(data, nodes = c(6, 3), nepoch = 20, plot = T, model=NULL, model_savefile=NA,
                    validation_split=0.1, test_split = 0.1, lev=c(0.5,1)){

  # some kind of information on the run!

  dind = match(c("Res",names(data[[1]])),names(TD))
  td = TD[,dind] # training data is reduced to the size of the real data and ordered

  tdata = data
  tdata[[1]] = dolog_2(tdata[[1]])                                  # log imperfect fractions
  tdata[[1]] = dologit(tdata[[1]],types = "VML")                    # logit proportions (but rescaled 0.05 - 0.95 prior to logit)
  keep = data_comp(td,tdata)$inside

  td = td[,c(TRUE,keep)]
  nr<-nrow(td);   nc<-ncol(td)
  p_nontrain = test_split
  train_switch = sample(c(TRUE,FALSE),nr,replace=T,prob=c(1-p_nontrain,p_nontrain))

  # Training dataset
  train_target<-td[train_switch, 1]
  train<-td[train_switch, 2:nc]
  mu = apply(train, 2, mean)
  sd = apply(train, 2, sd)
  strain = scale(train, center=mu, scale=sd)

  # Empirical dataset
  sdata = tdata
  sdata[[1]] = scale(tdata[[1]][,keep], center=mu, scale=sd)

  #train_data_comp(train, strain, tdata, sdata)

  # Testing dataset
  ind = (1:nr)[!train_switch]
  testy<-td[ind,2:nc]
  testy=scale(testy,center=mu,scale=sd)
  testy_target<-td[ind,1]

  if(is.null(model))model = keras_model_sequential()

  model %>%
    layer_dense(units = nodes[1], activation = "relu") %>%
    layer_dense(units = nodes[2], activation = "relu") %>%
    layer_dense(units = 1)

  model %>%
    compile(
      loss = 'MSE', #mean_squared_error',#"mse",#mae",#"mse",
      optimizer =  optimizer_adam(), #'adam',#optimizer_rmsprop(),'rmsprop',
      metrics = "MAE"
    )

  history <- model %>% fit(strain, train_target,
                           epochs = nepoch,
                           validation_split = validation_split,
                           verbose = 2)


  # predicted vs simulated
  pred = exp((model %>% predict(testy))[,1])
  sim = exp(testy_target)
  tab = NN_fit(sim, pred, history, lev=lev, addpow=T,nepoch=nepoch,plot=F)

  MAE = history$metrics$val_MAE[nepoch]

  if(!is.na(model_savefile))save_model(model,filepath = model_savefile, overwrite=T)

  if(!all(colnames(train)==names(sdata[[1]]))) stop("Training dataset does not match size of submitted data object")


  outlist=list(MAE = MAE, sim=sim, pred=pred, grid=grid, tab = tab, model=model, train=train,
               train_target=train_target, nepoch = nepoch, r2 = cor(sim,pred)^2, history = history,
               lev=lev, mu = mu, sd = sd, data = data, sdata = sdata)

  if(plot)(pred_plot(outlist))

  outlist

}




