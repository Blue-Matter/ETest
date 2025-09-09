#library(ETest)

#data = Shark_1_data; nodes = c(4, 2); nepoch = 20; plot = T; model=NULL; model_savefile=NA; validation_split=0.1; test_split = 0.1; lev=c(0.5,1)

train_ind=function(data, nodes = c(4, 2), nepoch = 20, plot = T, model=NULL, model_savefile=NA,
                    validation_split=0.1, test_split = 0.1, lev=c(0.5,1)){

  dind = match(c("Brel_s1",names(data[[1]])),names(TD))
  td = TD[,..dind] # training data is reduced to the size of the real data and ordered
  nr<-nrow(td);   nc<-ncol(td)
  p_nontrain = test_split
  train_switch = sample(c(TRUE,FALSE),nr,replace=T,prob=c(1-p_nontrain,p_nontrain))

  # Training dataset
  train<-td[train_switch, 2:nc]
  mu = apply(train, 2, mean)
  sd = apply(train, 2, sd)
  train = scale(train, center=mu, scale=sd)
  train_target<-td[train_switch, 1]

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

  history <- model %>% fit(train, train_target,
                           epochs = nepoch,
                           validation_split = validation_split,
                           verbose = 2
  )

  # predicted vs simulated
  pred = exp((model %>% predict(testy))[,1])
  sim = exp(testy_target)
  tab = NN_fit(sim, pred, history, lev=lev, addpow=T,nepoch=nepoch,plot=F)

  MAE = history$metrics$val_MAE[nepoch]

  if(!is.na(model_savefile))save_model(model,filepath = model_savefile, overwrite=T)

  outlist=list(MAE = MAE, sim=sim, pred=pred, grid=grid, tab = tab, model=model, train=train,
       train_target=train_target, nepoch = nepoch, r2 = cor(sim,pred)^2, history = history,
       lev=lev, mu = mu, sd = sd)

  if(plot)(pred_plot(outlist))

  outlist

}




