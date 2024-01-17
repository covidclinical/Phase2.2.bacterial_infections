rm(list=ls())
setwd("../results/sites_outputs/")
library(dplyr)
library(glmnet)
library(ggplot2)
library(gridExtra)

# ------------- load data and pre-processing ---------

# please change the study sites here
# "a2Tables.RData" and "a1Tables.RData" are stored in folders called "BCH", "CCMC", ..... 
sites = c("BCH","CCMC","GOSH","H12O","HCUV","MICHIGAN")

dat_combined = NULL
design.mats = vector("list",length(sites))
# allow a possible change in slope every gap=3 months
gap = 3 #use gap=2 as sensitivity analysis, change points identified are very consistent

for (kk in 1:length(sites)){
  load(paste0(sites[kk],"/a2Tables.RData"))
  load(paste0(sites[kk],"/a1Tables.RData"))
  
  # this line looks at total bacterial infection
  #dat = a2_total_bacterial
  # this line looks at a subgroup of bacterial infections
  dat = (a2_subgroups %>% filter(subgroup=="Respiratory"))[,-2]
  #dat = (a2_subgroups %>% filter(subgroup=="IBI_simple"))[,-2]
  colnames(dat)[2] = "bacterial"
  dat = merge(dat,a2_total_all,all.y=TRUE)
  colnames(dat)[3] = "all"
  dat$bacterial[is.na(dat$bacterial)] = 0
  dat$frac = dat$bacterial/dat$all
  
  # creating the covariate matrix corresponding to age group
  age.frac = NULL
  for (age.group in c("childhood","infancy","middle childhood")){
    this_group = a2_age_all %>% filter(category == age.group) %>% arrange(timeToGroup)
    this_group = merge(dat,this_group,all.x=TRUE)
    this_group$count = ifelse(is.na(this_group$count),0,this_group$count)
    age.frac = cbind(age.frac,this_group$count/dat$all)
  }
  colnames(age.frac) = c("cFrac","iFrac","mFrac")
  dat = cbind(dat,age.frac)
  
  # parse date into month, relative month, and season
  dat$month = sapply(dat$timeToGroup,function(x){as.numeric(strsplit(as.character(x),split="-")[[1]][2])})
  dat$year = sapply(dat$timeToGroup,function(x){as.numeric(strsplit(as.character(x),split="-")[[1]][1])})
  dat$time = 12*(dat$year-2019) + dat$month
  print(c(sites[kk],max(dat$time),nrow(dat)))
  dat$season = rep(NA,nrow(dat))
  for (i in 1:nrow(dat)){
    if (dat$month[i] %in% c(1,2,12)){
      dat$season[i] = "winter"
    } else if (dat$month[i] %in% c(3,4,5)){
      dat$season[i] = "spring"
    } else if (dat$month[i] %in% c(6,7,8)){
      dat$season[i] = "summer"
    } else if (dat$month[i] %in% c(9,10,11)){
      dat$season[i] = "autumn"
    }
  }
  dat$site = rep(sites[kk],nrow(dat))
  dat = dat %>% filter(timeToGroup <= as.Date("2023-03-01"))
  #print(dim(dat))
  #print(range(dat$timeToGroup))
  dat_combined = rbind(dat_combined,dat)
  
  # creating the covariate matrix corresponding to season
  design.season = matrix(0,nrow(dat),3)
  seasons = c("summer","autumn","winter")
  for (j in 1:length(seasons)){
    design.season[,j] = 1*(dat$season == seasons[j])
  }
  colnames(design.season) = seasons
  
  # creating the covariate matrix corresponding to the linear spline basis
  nbasis = ceiling(51/gap)-1
  basis = matrix(0,nrow(dat),nbasis)
  for (i in 1:nbasis){
    basis[,i] = (dat$time - gap*i)*(dat$time > (gap*i))
  }
  design.mat = cbind(dat[,c("all","cFrac","iFrac","mFrac","time")],basis)
  colnames(design.mat)[-c(1:5)] = sapply(1:nbasis,function(i){paste0("time",gap*i)})
  design.mat = as.matrix(design.mat)
  design.mat = cbind(design.mat,design.season)
  #print(dim(design.mat))
  
  # each site has its own design matrix, store them in a list
  design.mats[[kk]] = design.mat
}

# creating the design matrix corresponding to site indicator
design.sites = matrix(0,nrow(dat_combined),length(sites)-1)
for (jj in 1:length(sites[-1])){
  design.sites[,jj] = 1*(dat_combined$site == sites[-1][jj])
}
colnames(design.sites) = sites[-1]

# combine all the pieces of design matrix
# the final combined design matrix is "design_combined"
design_combined = NULL
for (kk in 1:length(sites)){
  design_combined = rbind(design_combined,design.mats[[kk]])
}
design_combined = cbind(design_combined,design.sites)

# ------------- adaptive lasso and variable selection ---------
# ----------------------------
# --- glm linear model -------
# ----------------------------
# set.seed(99)
# mod.lasso = cv.glmnet(x=design_combined,y=dat_combined$bacterial)
# coef(mod.lasso,s="lambda.1se")
# pred.lasso = predict(mod.lasso,s="lambda.1se",newx=design_combined)
# dat_combined$pred = pred.lasso

# ----------------------------------------------------
# --- glm poisson model with total as covariate ------
# --- I think this is the preferred approach ---------
# ----------------------------------------------------
set.seed(99)
mod.lasso.pois = cv.glmnet(x=design_combined, y=dat_combined$bacterial,family=poisson)
beta.init = as.vector(coef(mod.lasso.pois,s="lambda.1se"))[-1]
set.seed(99)
mod.lasso.pois = cv.glmnet(x=design_combined, y=dat_combined$bacterial,family=poisson,penalty.factor=1/abs(beta.init))
beta.alasso = coef(mod.lasso.pois,s="lambda.1se")
beta.alasso

pred.lasso = predict(mod.lasso.pois, s="lambda.1se",newx=design_combined,type="response")
dat_combined$pred = pred.lasso

# -------------------------------------------------
# --- glm poisson model with total as offset ------
# -------------------------------------------------
# set.seed(99)
# mod.lasso.pois = cv.glmnet(x=design_combined[,-1], y=dat_combined$bacterial,family=poisson,offset=log(design_combined[,1]))
# beta.init = as.vector(coef(mod.lasso.pois,s="lambda.1se"))[-1]
# mod.lasso.pois = cv.glmnet(x=design_combined[,-1], y=dat_combined$bacterial,family=poisson,offset=log(design_combined[,1]),penalty.factor=1/abs(beta.init))
# beta.alasso = coef(mod.lasso.pois,s="lambda.1se")
# pred.lasso = predict(mod.lasso.pois, s="lambda.1se",newx=design_combined[,-1],newoffset=log(design_combined[,1]),type="response")
# dat_combined$pred = pred.lasso

# ggplot(dat_combined) +
#   geom_line(aes(x = timeToGroup, y = bacterial, colour = site)) + labs(x="time",y="number of infection")
# p1= ggplot(dat_combined %>% filter(site=="BCH")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("BCH")+labs(x="time",y="bacterial infection")
# p2= ggplot(dat_combined %>% filter(site=="MICHIGAN")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("Michigan")+labs(x="time",y="bacterial infection")
# p3= ggplot(dat_combined %>% filter(site=="HCUV")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("HCUV")+labs(x="time",y="bacterial infection")
# p4= ggplot(dat_combined %>% filter(site=="H12O")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("H12O")+labs(x="time",y="bacterial infection")
# grid.arrange(p1,p2,p3,p4,ncol=2)


# add_level = matrix(0,nrow(design_combined),nbasis)
# for (jj in 1:nbasis){
#   add_level[,jj] = 1*(design_combined[,"time"]>=(jj*gap))
# }
# colnames(add_level) = sapply(c(1:nbasis)*gap,function(x){paste0("level",x)})
# 
# design_combined_level_only = cbind(design_combined[,-c(which(sapply(colnames(design_combined),function(x){substr(x,1,4)}) == "time"))],add_level)
# set.seed(9)
# mod.lasso.pois = cv.glmnet(x=design_combined_level_only, y=dat_combined$bacterial,family=poisson)
# beta.init = as.vector(coef(mod.lasso.pois,s="lambda.1se"))[-1]
# mod.lasso.pois = cv.glmnet(x=design_combined_level_only, y=dat_combined$bacterial,family=poisson,penalty.factor=1/abs(beta.init))
# beta.alasso = coef(mod.lasso.pois,s="lambda.1se")
# pred.lasso = predict(mod.lasso.pois, s="lambda.1se",newx=design_combined[,-1],newoffset=log(design_combined[,1]),type="response")
# dat_combined$pred = pred.lasso

# ----------- refit model with selected variables + hypothesis testing ---------
# -----------
# -- refit --
# -----------
# we now refit the model with variables selected by adaptive lasso
# this is to facilitate hypothesis testing
selected_var = rownames(beta.alasso)[which(beta.alasso != 0)][-1]
# alternatively, we can manually specify which variables we want to
# include in the model and do hypothesis testing
# selected_var = c("time","time15","time27",
#                       "cFrac","iFrac","mFrac",
#                       "winter",
#                       "CCMC","GOSH","H12O","HCUV","MICHIGAN")

design_reduced = design_combined[,colnames(design_combined) %in% selected_var]
time_points = as.numeric(sapply(selected_var[sapply(selected_var,function(x){substr(x,1,4)}) == "time"],function(x){substr(x,5,nchar(x))}))
# level_change_design = matrix(0,nrow(design_reduced),length(time_points))
# for (jj in 1:length(time_points)){
#   level_change_design[,jj] = 1*(design_combined[,"time"] >= time_points[jj])
# }
# level_change_design = data.frame(level_change_design)
# colnames(level_change_design) = sapply(time_points,function(x){paste0("indicator",x)})
# design_slope_level = cbind(design_reduced,level_change_design)
# design_level = cbind(design_reduced[,-c(which(sapply(colnames(design_reduced),function(x){substr(x,1,4)})=="time" ))],level_change_design)
design_slope = data.frame(design_reduced)

# model with only slope change
dat_slope = cbind(dat_combined$bacterial,design_combined[,1],design_slope)
colnames(dat_slope)[1:2] = c("bacterial","all")
myformula = paste0("bacterial~",paste0(colnames(dat_slope)[-1],collapse="+"))
mod_slope = glm(as.formula(myformula),family=poisson(),data=dat_slope)
summary(mod_slope)

time12_index = which(names(coefficients(mod_slope)) == "time12")
time24_index = which(names(coefficients(mod_slope)) == "time24")
time45_index = which(names(coefficients(mod_slope)) == "time45")
time48_index = which(names(coefficients(mod_slope)) == "time48")

# testing linear hypothesis in GLM
# need to give specify the linear combination
# for example, the level in month 45 is (45-12)*time12 + (45-24)*time24 + ...
# for example, the slope after month 24 is time12 + time24
LH1 = LH2 = LH3 =LH4 = LH5 = matrix(rep(0,length(coefficients(mod_slope))),nrow=1)
LH1[1,time12_index] =1 ; LH1[1,time24_index] =1
LH4[1,c(time12_index,time24_index,time45_index)] = 1
LH5[1,c(time12_index,time24_index,time45_index,time48_index)] = 1
#LH2[1,time12_index] =24; LH2[1,time24_index] =12
LH2[1,time12_index] =33; LH2[1,time24_index] =21
LH3[1,c(time12_index,time24_index,time45_index)] = 48-c(12,24,45)
library(car)
LH1 %*% coefficients(mod_slope)
linearHypothesis(mod_slope,LH1)

LH4 %*% coefficients(mod_slope)
linearHypothesis(mod_slope,LH4)

LH5 %*% coefficients(mod_slope)
linearHypothesis(mod_slope,LH5)

LH2 %*% coefficients(mod_slope)
linearHypothesis(mod_slope,LH2)
LH3 %*% coefficients(mod_slope)
linearHypothesis(mod_slope,LH3)

# -------------- visualization ----------------
### plot the model fit
pred = predict(mod_slope,newdata=dat_slope,type="response")
dat_combined$pred = pred
ggplot(dat_combined) +
  geom_line(aes(x = timeToGroup, y = bacterial, colour = site)) + labs(x="time",y="number of infection")
p1= ggplot(dat_combined %>% filter(site=="BCH")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("BCH")+labs(x="time",y="bacterial infection")
p2= ggplot(dat_combined %>% filter(site=="CCMC")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("CCMC")+labs(x="time",y="bacterial infection")
p3= ggplot(dat_combined %>% filter(site=="GOSH")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("GOSH")+labs(x="time",y="bacterial infection")
p4= ggplot(dat_combined %>% filter(site=="H12O")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("H12O")+labs(x="time",y="bacterial infection")
p5= ggplot(dat_combined %>% filter(site=="HCUV")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("HCUV")+labs(x="time",y="bacterial infection")
p6= ggplot(dat_combined %>% filter(site=="MICHIGAN")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("MICHIGAN")+labs(x="time",y="bacterial infection")
grid.arrange(p1,p2,p3,p4,p5,p6,ncol=3)

### now plot the fit using a standard population
all_pred = NULL
for (kk in 1:length(sites)){
  standard_site = sites[kk]
  if (standard_site == "BCH"){
    dat_standard_site = dat_slope[which(rowSums(dat_slope[,c("CCMC","GOSH","H12O","HCUV","MICHIGAN")])==0),]
  } else {
    dat_standard_site = dat_slope[which(dat_slope[,standard_site]==1),]
  }
  dat_standard_site$all = mean(dat_standard_site$all)
  dat_standard_site$cFrac = mean(dat_standard_site$cFrac)
  dat_standard_site$iFrac = mean(dat_standard_site$iFrac)
  dat_standard_site$mFrac = mean(dat_standard_site$mFrac)
  dat_standard_site$winter = rep(0,nrow(dat_standard_site))
  
  pred_standard_site = predict(mod_slope,newdata=dat_standard_site)
  pred_standard_site = predict(mod_slope,newdata=dat_standard_site,type="response")
  this_pred = data.frame(time=dat_combined$time[dat_combined$site == standard_site],
                         pred = pred_standard_site,
                         site = rep(standard_site,length(pred_standard_site)))
  all_pred = rbind(all_pred,this_pred)
}
library(ggplot2)
all_pred$date = dat_combined$timeToGroup
ggplot(data=all_pred,aes(x=date,y=pred,colour=site)) + geom_line()+ylab("predicted bacterial infection")+
  geom_vline(xintercept=as.Date("2020-01-01"),linetype=2,colour="blue")+geom_vline(xintercept=as.Date("2021-01-01"),linetype=2,colour="blue")+geom_vline(xintercept=as.Date("2022-09-01"),linetype=2,colour="blue")+geom_vline(xintercept=as.Date("2022-12-01"),linetype=2,colour="blue")+
  geom_vline(xintercept=as.Date("2020-03-01"),linetype=2)+geom_vline(xintercept=as.Date("2021-03-01"),linetype=2)


# dat_combined$pred = predict(mod_slope,type="response")
# p1= ggplot(dat_combined %>% filter(site=="BCH")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("BCH")+labs(x="time",y="bacterial infection")
# p2= ggplot(dat_combined %>% filter(site=="MICHIGAN")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("Michigan")+labs(x="time",y="bacterial infection")
# p3= ggplot(dat_combined %>% filter(site=="HCUV")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("HCUV")+labs(x="time",y="bacterial infection")
# p4= ggplot(dat_combined %>% filter(site=="H12O")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("H12O")+labs(x="time",y="bacterial infection")
# grid.arrange(p1,p2,p3,p4,ncol=2)
# 
# # model with only level change
# dat_level = cbind(dat_combined$bacterial,design_combined[,1],design_level)
# colnames(dat_level)[1:2] = c("bacterial","all")
# myformula = paste0("bacterial~",paste0(colnames(dat_level)[-1],collapse="+"))
# mod_level = glm(as.formula(myformula),family=poisson(),data=dat_level)
# summary(mod_level)
# 
# # model with both slope and level changes
# dat_slope_level = cbind(dat_combined$bacterial,design_combined[,1],design_slope_level)
# colnames(dat_slope_level)[1:2] = c("bacterial","all")
# myformula = paste0("bacterial~",paste0(colnames(dat_slope_level)[-c(1:2)],collapse="+"))
# mod_slope_level = glm(as.formula(myformula),family=poisson(),data=dat_slope_level)
# 
# ### anova test
# anova(mod_slope,mod_slope_level,test="LRT")
# anova(mod_level,mod_slope_level,test="LRT")
# 
# png("spline.png",width=4,height=4,units="in",res=300)
# ggplot(dat_combined %>% filter(site=="MICHIGAN")) +geom_line(aes(x=timeToGroup,y=pred))+labs(x="time",y="bacterial infection")
# dev.off()
# png("pred.png",width=8,height=4,units="in",res=300)
# grid.arrange(p1,p2,ncol=2)
# dev.off()
# design_fake = matrix(0,51,4)
# set.seed(28)
# mod.lasso.pois = cv.glmnet(x=design_combined,y=dat_combined$bacterial,family=poisson)
# coef(mod.lasso.pois,s="lambda.1se")
# 
# set.seed(28)
# mod.lasso.pois.offset = cv.glmnet(x=design_combined[,-1],y=dat_combined$bacterial,family=poisson,offset=log(design_combined[,1]))
# coef(mod.lasso.pois.offset,s="lambda.1se")
# 
# 
# #visualization
# par(mar = c(2.1, 4.1, 4.1, 2.1))
# par(mfrow=c(2,1))
# for (kk in 1:length(sites)){
#   dat_plot = dat_combined %>% filter(site == sites[kk])
#   plot(dat_plot$timeToGroup,dat_plot$bacterial,main = sites[kk])
#   plot(dat_plot$timeToGroup,dat_plot$frac,main = sites[kk])
# }
# 
# plot(dat_combined$timeToGroup,dat_combined$frac)
# 
# # UPitt has weird peaks in Aprils??
# 
# 
# ### total counts does not match
# ### probably the different bacterial infections are
# ### NOT mutually exclusive
# 
# # this should match a2_age_bacterial? seems it does not
# a2_age_bact_infection %>% group_by(timeToGroup,category) %>% summarise(tt = sum(count))
# 
# # this should match a2_total_bacterial? seems it does not
# a2_bact_infection %>% group_by(timeToGroup) %>% summarise(tt = sum(count))
# 
# # this should match a2_total_bacterial? seems it does
# a2_age_bacterial %>% group_by(timeToGroup) %>% summarise(tt = sum(count))
# 
# # this should match a2_bact_infection? Seems it does
# a2_age_bact_infection %>% group_by(timeToGroup,disorder_group) %>% summarise(tt = sum(count))
# 
# 
# 
# 
# 
# 
# # fit a piecewise linear function??
# mod = lm(bacterial~all+time,data=dat)
# mod.pois = glm(bacterial~all+time,data=dat,family=poisson)
# 
# 
# library(glmnet)
# mod.lasso = cv.glmnet(x=design.mat,y=dat$bacterial)
# coef(mod.lasso,s="lambda.1se")
# 
# mod.lasso.pois = cv.glmnet(x=design.mat,y=dat$bacterial,family=poisson)
# coef(mod.lasso.pois,s="lambda.1se")
# 
# ### poisson model or linear model?
# ### do we need offset if poisson model?
# 
# 
