###############################################################################
##  Title:  Trust Measure Development Analysis
##  Author:  Patrick E. McKnight (pmcknigh@gmu.edu)
##           Simone Erchov (sfranz1@gmu.edu)
##           Lisa Alexander (lalexan8@gmu.edu)
##  Created:  11/20/2016
##  Last Edited By:  Patrick
##  Lasted Edited Date:  3/14/2017
##  Description:  Data analysis for the Trust Measurement development paper 
##                where we documented the ETI item performance.
##  Publication:  TBA
###############################################################################

############################ LOAD ENVIRONMENT #################################
### NOTE:  include all relevant packages here
library(car)
library(psych)
library(ggplot2)

### functions listed below in order of use

###### Trust data read in from Qualtrics #######
## Procedure:
## 1.  Download data from qualtrics (legacy format)
## 2.  Delete 2nd line from the csv file
## 3.  Use this function as follows:
##     mydat <- TrustDataFcn("filename.csv",cut.time=XX,source="putNameHere")
##     where
##          filename.csv is the legacy file after the 2nd line is deleted
##          cut.time is the time in minutes you DEMAND the subjects to take for validity reasons
##          source is the name you want to identify the source of the data (for plotting purposes and such)
##
TrustDataFcn <- function(file=NULL,cut.time=10,source=NULL){
  dat <- read.csv(file,header=T) 
  datTIME <- as.numeric((strptime(as.character(dat$V9),"%m/%d/%Y %H:%M") - strptime(as.character(dat$V8),"%m/%d/%Y %H:%M"))/60)
  ## Ngood <- table(datTIME > cut.time)
  dat <- dat[datTIME > cut.time,]
  dat$ID <- c(1:nrow(dat))
  Tvars <- list(c("Q102","Q103","Q104","Q105","Q106","Q85","Q86"),
                c("Q170","Q171","Q173","Q175","Q176","Q89","Q90"),
                c("Q64","Q161","Q65","Q165","Q166","Q87","Q88"),
                c("Q179","Q180","Q182","Q184","Q185","Q91","Q92"),
                c("Q188","Q189","Q191","Q193","Q194","Q93","Q94"),
                c("Q197","Q198","Q200","Q201","Q203","Q95","Q96"),
                c("Q206","Q207","Q209","Q212","Q213","Q97","Q98"),
                c("Q215","Q216","Q217","Q221","Q222","Q99","Q100"))
  out <- data.frame(id=NA,scen=NA,G=NA,U1=NA,U2=NA,R=NA,T=NA,B=NA,U3=NA)
  for(i in 1:8){
    tmp <- data.frame(id=dat$ID,scen=i,dat[,Tvars[[i]]])
    names(tmp) <- c("id","scen","G","U1","U2","R","T","B","U3")
    out <- rbind(out,tmp)
  }
  out$source <- source
  out <- out[-1,]
  return(out)
}

TrustDataFcn2 <- function(x=NULL,cut.time=10,source=NULL){
  dat <- read.csv(x,header=T)
  #datTIME <- as.numeric((strptime(as.character(dat$V9),"%Y-%m-%d %H:%M:%S") - strptime(as.character(dat$V8),"%Y-%m-%d %H:%M:%S"))/60)
  datTIME <- as.numeric((strptime(as.character(dat$V9),"%m/%d/%Y %H:%M") - strptime(as.character(dat$V8),"%m/%d/%Y %H:%M"))/60)
  ## Ngood <- table(datTIME > cut.time)
  dat <- dat[datTIME > cut.time,]
  dat$ID <- c(1:nrow(dat))
  Tvars <- list(c("Q102","Q103","Q104","Q105","Q106","Q85","Q86"),
                c("Q170","Q171","Q173","Q175","Q176","Q89","Q90"),
                c("Q64","Q161","Q65","Q165","Q166","Q87","Q88"),
                c("Q179","Q180","Q182","Q184","Q185","Q91","Q92"),
                c("Q188","Q189","Q191","Q193","Q194","Q93","Q94"),
                c("Q197","Q198","Q200","Q201","Q203","Q95","Q96"),
                c("Q206","Q207","Q209","Q212","Q213","Q97","Q98"),
                c("Q215","Q216","Q217","Q221","Q222","Q99","Q100"),
                c("Q194","Q296","Q198","Q200","Q201","Q204","Q205"),
                c("Q358","Q359","Q362","Q365","Q366","Q368","Q369"),
                c("Q373","Q374","Q377","Q380","Q381","Q383","Q384"),
                c("Q388","Q389","Q392","Q395","Q396","Q398","Q399"),
                c("Q403","Q404","Q407","Q410","Q411","Q413","Q414"),
                c("Q418","Q419","Q422","Q425","Q426","Q428","Q429"),
                c("Q433","Q434","Q437","Q440","Q441","Q443","Q444"),
                c("Q448","Q449","Q452","Q455","Q456","Q458","Q459"))
  out <- data.frame(id=NA,scen=NA,G=NA,U1=NA,U2=NA,R=NA,T=NA,B=NA,U3=NA)
  for(i in 1:16){
    tmp <- data.frame(id=dat$ID,scen=i,dat[,Tvars[[i]]])
    names(tmp) <- c("id","scen","G","U1","U2","R","T","B","U3")
    out <- rbind(out,tmp)
  }
  out$source <- source
  out <- out[-1,]
  return(out)
}

TrustDataFcn3 <- function(x1=NULL,cut.time=10,source=NULL){
  dat <- read.csv(x1,header=T)
  #datTIME <- as.numeric((strptime(as.character(dat$V9),"%Y-%m-%d %H:%M:%S") - strptime(as.character(dat$V8),"%Y-%m-%d %H:%M:%S"))/60)
  datTIME <- as.numeric((strptime(as.character(dat$V9),"%m/%d/%Y %H:%M") - strptime(as.character(dat$V8),"%m/%d/%Y %H:%M"))/60)
  ## Ngood <- table(datTIME > cut.time)
  dat <- dat[datTIME > cut.time,]
  dat$ID <- c(1:nrow(dat))
  Tvars <- list(c("Q102","Q103","Q104","Q105","Q106","Q85","Q86"),
                c("Q170","Q171","Q173","Q175","Q176","Q89","Q90"),
                c("Q64","Q161","Q65","Q165","Q166","Q87","Q88"),
                c("Q179","Q180","Q182","Q184","Q185","Q91","Q92"),
                c("Q188","Q189","Q191","Q193","Q194","Q93","Q94"),
                c("Q197","Q198","Q200","Q201","Q203","Q95","Q96"),
                c("Q206","Q207","Q209","Q212","Q213","Q97","Q98"),
                c("Q215","Q216","Q217","Q221","Q222","Q99","Q100"),
                # Flyers
                c("Q299","Q300","Q303","Q306","Q307","Q309","Q310"),
                c("Q314","Q315","Q318","Q321","Q322","Q324","Q325"),
                c("Q329","Q330","Q333","Q336","Q337","Q339","Q340"),
                c("Q344","Q345","Q348","Q351","Q352","Q354","Q355"),
                c("Q359","Q360","Q363","Q366","Q367","Q369","Q370"),
                c("Q374","Q375","Q378","Q381","Q382","Q384","Q385"),
                c("Q389","Q390","Q393","Q396","Q397","Q399","Q400"),
                c("Q404","Q405","Q408","Q411","Q412","Q414","Q415"))
  
  out <- data.frame(id=NA,scen=NA,G=NA,U1=NA,U2=NA,R=NA,T=NA,B=NA,U3=NA)
  for(i in 1:16){
    tmp <- data.frame(id=dat$ID,scen=i,dat[,Tvars[[i]]])
    names(tmp) <- c("id","scen","G","U1","U2","R","T","B","U3")
    out <- rbind(out,tmp)
  }
  out$source <- source
  out <- out[-1,]
  return(out)
}

TrustDataFcn4 <- function(x2=NULL,cut.time=10,source=NULL){
  dat <- read.csv(x2,header=T)
  #datTIME <- as.numeric((strptime(as.character(dat$V9),"%Y-%m-%d %H:%M:%S") - strptime(as.character(dat$V8),"%Y-%m-%d %H:%M:%S"))/60)
  datTIME <- as.numeric((strptime(as.character(dat$V9),"%m/%d/%Y %H:%M") - strptime(as.character(dat$V8),"%m/%d/%Y %H:%M"))/60)
  ## Ngood <- table(datTIME > cut.time)
  dat <- dat[datTIME > cut.time,]
  dat$ID <- c(1:nrow(dat))
  Tvars <- list(c("Q102","Q103","Q104","Q105","Q106","Q85","Q86"),
                c("Q170","Q171","Q173","Q175","Q176","Q89","Q90"),
                c("Q64","Q161","Q65","Q165","Q166","Q87","Q88"),
                c("Q179","Q180","Q182","Q184","Q185","Q91","Q92"),
                c("Q188","Q189","Q191","Q193","Q194","Q93","Q94"),
                c("Q197","Q198","Q200","Q201","Q203","Q95","Q96"),
                c("Q206","Q207","Q209","Q212","Q213","Q97","Q98"),
                c("Q215","Q216","Q217","Q221","Q222","Q99","Q100"),
                # MTurk
                c("Q300","Q301","Q304","Q307","Q308","Q310","Q311"),
                c("Q315","Q316","Q319","Q322","Q323","Q325","Q326"),
                c("Q330","Q331","Q334","Q337","Q338","Q340","Q341"),
                c("Q345","Q346","Q349","Q351","Q352","Q354","Q355"),
                c("Q360","Q361","Q364","Q367","Q368","Q370","Q371"),
                c("Q375","Q376","Q379","Q381","Q382","Q384","Q385"),
                c("Q389","Q390","Q393","Q396","Q397","Q399","Q400"),
                c("Q404","Q405","Q408","Q411","Q412","Q414","Q415"))
  
  out <- data.frame(id=NA,scen=NA,G=NA,U1=NA,U2=NA,R=NA,T=NA,B=NA,U3=NA)
  for(i in 1:16){
    tmp <- data.frame(id=dat$ID,scen=i,dat[,Tvars[[i]]])
    names(tmp) <- c("id","scen","G","U1","U2","R","T","B","U3")
    out <- rbind(out,tmp)
  }
  out$source <- source
  out <- out[-1,]
  return(out)
}

#Sona
TraitMeasFcn <- function(x=NULL,cut.time=10,source=NULL){ 
  dat <- read.csv(x,header=T)
  #datTIME <- as.numeric((strptime(as.character(dat$V9),"%Y-%m-%d %H:%M:%S") - strptime(as.character(dat$V8),"%Y-%m-%d %H:%M:%S"))/60)
  datTIME <- as.numeric((strptime(as.character(dat$V9),"%m/%d/%Y %H:%M") - strptime(as.character(dat$V8),"%m/%d/%Y %H:%M"))/60)
  dat <- dat[datTIME > cut.time,]
  dat$id <- c(1:nrow(dat))
  out <- dat[,c("id","Q17",paste("Q18",1:6,sep="_"),paste("Q25",1:6,sep="_"),"Q30","Q30.1","Q2","Q4","Q5","Q6","Q8","Q9","Q10","Q12","Q13","Q14","Q146")]
  names(out) <- c("id","GSS",paste("GTS",1:6,sep="."),paste("WVS",1:6,sep="."),"mood","energy","female","age","ethnicity","country","RelStatus","educ","employ","livingArr","income","religion","followup")
  out$female <- out$female - 1
  out$followup <- 1 - round(out$followup/3)
  return(out)
 }
 

#MTurk
TraitMeasFcn2 <- function(x=NULL,cut.time=10,source=NULL){
  dat <- read.csv(x,header=T)
  #datTIME <- as.numeric((strptime(as.character(dat$V9),"%Y-%m-%d %H:%M:%S") - strptime(as.character(dat$V8),"%Y-%m-%d %H:%M:%S"))/60)
  datTIME <- as.numeric((strptime(as.character(dat$V9),"%m/%d/%Y %H:%M") - strptime(as.character(dat$V8),"%m/%d/%Y %H:%M"))/60)
  dat <- dat[datTIME > cut.time,]
  dat$id <- c(1:nrow(dat))
  out <- dat[,c("id","Q17",paste("Q18",1:6,sep="_"),paste("Q25",1:6,sep="_"),"Q30","Q30.1","Q2","Q4","Q5","Q6","Q8","Q9","Q10","Q12","Q13","Q14","Q148")]
  names(out) <- c("id","GSS",paste("GTS",1:6,sep="."),paste("WVS",1:6,sep="."),"mood","energy","female","age","ethnicity","country","RelStatus","educ","employ","livingArr","income","religion","followup")
  out$female <- out$female - 1
  out$followup <- 1 - round(out$followup/3)
  return(out)
}

#Flyer
TraitMeasFcn3 <- function(x=NULL,cut.time=10,source=NULL){
  dat <- read.csv(x,header=T)
  #datTIME <- as.numeric((strptime(as.character(dat$V9),"%Y-%m-%d %H:%M:%S") - strptime(as.character(dat$V8),"%Y-%m-%d %H:%M:%S"))/60)
  datTIME <- as.numeric((strptime(as.character(dat$V9),"%m/%d/%Y %H:%M") - strptime(as.character(dat$V8),"%m/%d/%Y %H:%M"))/60)
  dat <- dat[datTIME > cut.time,]
  dat$id <- c(1:nrow(dat))
  out <- dat[,c("id","Q17",paste("Q18",1:6,sep="_"),paste("Q25",1:6,sep="_"),"Q30","Q30.1","Q2","Q4","Q5","Q6","Q8","Q9","Q10","Q12","Q13","Q14","Q268")]
  names(out) <- c("id","GSS",paste("GTS",1:6,sep="."),paste("WVS",1:6,sep="."),"mood","energy","female","age","ethnicity","country","RelStatus","educ","employ","livingArr","income","religion","followup")
  out$female <- out$female - 1
  out$followup <- 1 - round(out$followup/3)
  return(out)
}

##### subONE function ####
subONE <- function(x,cols){
  for(i in cols){
    x[,i] <- x[,i] - 1
  }
  return(x)
}

##### Ufold function ######
Ufold <- function(x,cols){
  for(i in cols){
    x[,i] <- recode(x[,i],"5=10;c(4,6)=8;c(3,7)=6;c(2,8)=4;c(1,9)=2;c(0,10)=0",F,T)
  }
  return(x)
}

###### QualBeh function #####
QualBeh <- function(x,col){
  x[,col] <- abs(x[,col] - 2)
  return(x)
}

#### Function for the study - use this for all analyses
ETM.Fcn <- function(x,outfile){
  if ("U1" %in% names(x)){
    model <- lm(T~G:U1:R,data=x)
    x.l <- reshape(x,varying=names(x[c(3:6,9)]),new.row.names=1:(nrow(x)*5),v.names="value",timevar="Measure",times=c("G","U1","U2","R","U3"),idvar=c("id","scen"),direction="long")
    dat.hat <- data.frame(G=c(9,9,2,2,9,9,2,2),R=c(9,2,9,2,9,2,9,2),U1=c(9,9,9,9,2,2,2,2))
    lm.hat <- predict(model,newdata = dat.hat,se.fit=T)
    dat.hat <- cbind(dat.hat,lm.hat$fit,lm.hat$se.fit)
    dat.hat$U1 <- factor(dat.hat$U1,labels=c("L","H"))
  } else {
    model <- lm(T~G:U:R,data=x)
    x.l <- reshape(x,varying=names(x[3:5]),new.row.names=1:(nrow(x)*3),v.names="value",timevar="Measure",times=c("G","U","R"),idvar=c("id","scen"),direction="long")
    dat.hat <- data.frame(G=c(9,9,2,2,9,9,2,2),R=c(9,2,9,2,9,2,9,2),U=c(9,9,9,9,2,2,2,2))
    lm.hat <- predict(model,newdata = dat.hat,se.fit=T)
    dat.hat <- cbind(dat.hat,lm.hat$fit,lm.hat$se.fit)
    dat.hat$U <- factor(dat.hat$U,labels=c("L","H"))
  }
  print(summary(model))
  p1 <- ggplot(x.l,aes(x=T,y=value,colour=Measure)) + geom_smooth()
  print(p1)
  p1b <- ggplot(x.l,aes(x=T,y=value,colour=Measure)) + geom_smooth() + facet_wrap(~scen)
  print(p1b)
  dat.hat$G <- factor(dat.hat$G,labels=c("Low","High"))
  dat.hat$R <- factor(dat.hat$R,labels=c("Low","High"))
  names(dat.hat) <- c("G","R","U","T","T.se")
  Unc.labels <- c(L = "Low Uncertainty",H="High Uncertainty")
  p2 <- ggplot(dat.hat,aes(x=factor(G),y=T,colour=factor(R), ymin=T-2*T.se, ymax=T+2*T.se)) + geom_point(size=2) + facet_wrap(~U, labeller = labeller(U=Unc.labels)) + guides(col=guide_legend(title="Reliance")) + xlab("Goal Importance") + ylab("Overall Trust Rating (0-10)") + ylim(c(0,10)) + geom_errorbar(width=.2,col="black")
  print(p2)
  p3 <- ggplot(x.l,aes(x=as.factor(scen),y=value,colour=Measure))+geom_boxplot() + xlab("Scenario") + ylab("Level (0-10)")
  print(p3)
  pdf(outfile,title=outfile)
  plot(p1)
  plot(p1b)
  plot(p2)
  plot(p3)
  dev.off()
  out <- list(x.l, model, dat.hat, p1, p2)
  return(out)
}




####################### TRUST MODEL DATA SOURCES ##############################
## NOTE:  load data source here and conclude each section with a final object
## RESULT:  Each study produced slightly different data.  The data structure
##          looks like this:
##          id, scen, G, R, U, T, B and sometimes something else
##          also the order may change
##          The files are always LONG after this section

### Study 1 Data:  Retrospectively Recalled Trust Scenarios ####
dat1 <- read.csv("./Data/S1 Trust Data - cleaned_Jan2016.csv", header=T)
datTS <- dat1[,c(4,11:16,20:25,29:32,34:35,39:44,48:53)]
datTS.Ach <- datTS[,c(1,2,8,14,20,26)]# achieve the outcome you wanted
datTS.Agent <- datTS[,c(1,3,9,15,21,27)] # what agent did you trust
# by measure
datTS.T <- datTS[,c(1,4,10,16,22,28)] # GS_6_1 - Overall Trust
datTS.GI <- datTS[,c(1,5,11,17,23,29)] # GS_6_2 - Goal Importance
datTS.R <- datTS[,c(1,6,12,18,24,30)] # GS_6_3 - Reliance
datTS.U <- datTS[,c(1,7,13,19,25,31)] # GS_6_4 - Uncertainty
# by scenario
dat.1 <- datTS[,c(1,4:7)]
dat.2 <- datTS[,c(1,10:13)]
dat.3 <- datTS[,c(1,16:19)]
dat.4 <- datTS[,c(1,22:25)]
dat.5 <- datTS[,c(1,28:31)]
names(dat.1) <- c("id","TrustSR","GoalImp","Rel","Uncert")
names(dat.2) <- c("id","TrustSR","GoalImp","Rel","Uncert")
names(dat.3) <- c("id","TrustSR","GoalImp","Rel","Uncert")
names(dat.4) <- c("id","TrustSR","GoalImp","Rel","Uncert")
names(dat.5) <- c("id","TrustSR","GoalImp","Rel","Uncert")

dat.1$scenario <- 1
dat.1$trustLVL <- 100
dat.2$scenario <- 2
dat.2$trustLVL <- 75
dat.3$scenario <- 3
dat.3$trustLVL <- 50
dat.4$scenario <- 4
dat.4$trustLVL <- 25
dat.5$scenario <- 5
dat.5$trustLVL <- 0
dat1.l <- rbind(dat.1,dat.2,dat.3,dat.4,dat.5) ## NOTE change of name from original file
names(dat1.l) <- c("id","T","G","R","U","scen","T.manip")
dat1.l <- dat1.l[,c(1,2,3,5,4,6,7)]
str(dat1.l)

### Study 2 Data:  27 Vignette Post-Extensive Editing (N=9) ####
tmp2 <- read.csv("./Data/S2 TrustVig2.csv",header=T)
dat1 <- data.frame(id=1:nrow(tmp2),scen=1,tmp2[,c(13:17,20:24,27:31)])
dat2 <- data.frame(id=1:nrow(tmp2),scen=2,tmp2[,c(34:38,41:45,48:52)])
dat3 <- data.frame(id=1:nrow(tmp2),scen=3,tmp2[,c(55:59,62:66,69:73)])
dat4 <- data.frame(id=1:nrow(tmp2),scen=4,tmp2[,c(76:80,83:87,90:94)])
dat5 <- data.frame(id=1:nrow(tmp2),scen=5,tmp2[,c(97:101,104:108,111:115)])
dat6 <- data.frame(id=1:nrow(tmp2),scen=6,tmp2[,c(118:122,125:129,132:136)])
dat7 <- data.frame(id=1:nrow(tmp2),scen=7,tmp2[,c(139:143,146:150,153:157)])
dat8 <- data.frame(id=1:nrow(tmp2),scen=8,tmp2[,c(160:164,167:171,174:178)])

## rename to bind data.frames
names(dat1) <- c("id","scen","G.1","U.1","R.1","T.1","B.1","G.2","U.2","R.2","T.2","B.2","G.3","U.3","R.3","T.3","B.3")
names(dat2) <- c("id","scen","G.1","U.1","R.1","T.1","B.1","G.2","U.2","R.2","T.2","B.2","G.3","U.3","R.3","T.3","B.3")
names(dat3) <- c("id","scen","G.1","U.1","R.1","T.1","B.1","G.2","U.2","R.2","T.2","B.2","G.3","U.3","R.3","T.3","B.3")
names(dat4) <- c("id","scen","G.1","U.1","R.1","T.1","B.1","G.2","U.2","R.2","T.2","B.2","G.3","U.3","R.3","T.3","B.3")
names(dat5) <- c("id","scen","G.1","U.1","R.1","T.1","B.1","G.2","U.2","R.2","T.2","B.2","G.3","U.3","R.3","T.3","B.3")
names(dat6) <- c("id","scen","G.1","U.1","R.1","T.1","B.1","G.2","U.2","R.2","T.2","B.2","G.3","U.3","R.3","T.3","B.3")
names(dat7) <- c("id","scen","G.1","U.1","R.1","T.1","B.1","G.2","U.2","R.2","T.2","B.2","G.3","U.3","R.3","T.3","B.3")
names(dat8) <- c("id","scen","G.1","U.1","R.1","T.1","B.1","G.2","U.2","R.2","T.2","B.2","G.3","U.3","R.3","T.3","B.3")
dat.l1 <- rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8)
dat2.l <- reshape(dat.l1,varying=list(c(3,8,13),c(4,9,14),c(5,10,15),c(6,11,16),c(7,12,17)),direction="long",idvar='id',timevar="GROUP",v.names=c("G","U","R","T","B"),new.row.names =1:216)
dat2.l <- dat2.l[,c(1,2,4,5,6,7,8,3)]

### Study 3 Data:  9 of 27 Vignettes Presented with U broke into 3 - data from 3 sources #####

############ :SONA data ########
dat3s <- read.csv("./Data/S3 Vignettes_Round_3_SONA_10302016.csv",header=T) ## SONA
dat3sTIME <- as.numeric((strptime(as.character(dat3s$V9),"%m/%d/%Y %H:%M") - strptime(as.character(dat3s$V8),"%m/%d/%Y %H:%M"))/60)
table(dat3sTIME > 10)
dat3s <- dat3s[dat3sTIME > 10,]
dat1 <- data.frame(id=dat3s[,c(5)],scen=1,dat3s[,c(9:10,13,16:17,19:20)])
dat2 <- data.frame(id=dat3s[,c(5)],scen=2,dat3s[,c(23:24,27,30:31,33:34)])
dat3 <- data.frame(id=dat3s[,c(5)],scen=3,dat3s[,c(37:38,41,44:45,47:48)])
dat4 <- data.frame(id=dat3s[,c(5)],scen=4,dat3s[,c(51:52,55,58:59,61:62)])
dat5 <- data.frame(id=dat3s[,c(5)],scen=5,dat3s[,c(65:66,69,72:73,75:76)])
dat6 <- data.frame(id=dat3s[,c(5)],scen=6,dat3s[,c(79:80,83,86:87,89:90)])
dat7 <- data.frame(id=dat3s[,c(5)],scen=7,dat3s[,c(93:94,97,100:101,103:104)])
dat8 <- data.frame(id=dat3s[,c(5)],scen=8,dat3s[,c(107:108,111,114:115,117:118)])
names(dat1) <- c("id","scen","G.1","U.1","U.2","R.1","T.1","B.1","U.3")
names(dat2) <- c("id","scen","G.1","U.1","U.2","R.1","T.1","B.1","U.3")
names(dat3) <- c("id","scen","G.1","U.1","U.2","R.1","T.1","B.1","U.3")
names(dat4) <- c("id","scen","G.1","U.1","U.2","R.1","T.1","B.1","U.3")
names(dat5) <- c("id","scen","G.1","U.1","U.2","R.1","T.1","B.1","U.3")
names(dat6) <- c("id","scen","G.1","U.1","U.2","R.1","T.1","B.1","U.3")
names(dat7) <- c("id","scen","G.1","U.1","U.2","R.1","T.1","B.1","U.3")
names(dat8) <- c("id","scen","G.1","U.1","U.2","R.1","T.1","B.1","U.3")
dat3s.l <- rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8)
names(dat3s.l) <- c("id","scen","G","U1","U2","R","T","B","U3")
dat3s.l$source <- "SONA"

######### :mTurk data ###############
dat3m.l <- TrustDataFcn("./Data/S3 Vignettes_Round_3__MTurk_Updated.csv",10,"mTurk")

############ :Reddit data ##################
dat3r.l <- TrustDataFcn("./Data/S3 Vignettes_Round_3__Reddit.csv",10,"Reddit")

############# :Flyer data ###################
dat3f.l <- TrustDataFcn("./Data/S3 Vignettes_Round_3__Flyers.csv",10,"Flyers")

############# :Combine all Study 3 data files
dat3all.l <- rbind(dat3s.l,dat3m.l,dat3r.l,dat3f.l)
dat3all.l$source <- as.factor(dat3all.l$source) # refactor the source for later

### Study 4 Data:  8 Vignettes Presented with U broke into 3 - data from SONA #####
dat4.l <- TrustDataFcn("./Data/S4 Vignettes_Round_4__SONA__NewVigOnly.csv",5,"SONA")

### Study 5 Data:  Simone's dissertation data

######### :SONA data ############## 
dat5s.l <- TrustDataFcn2("./Data/S5 Vignettes_Round_5__SONA.csv",10,"SONA")

######### :mTurk data ###############
dat5m.l <- TrustDataFcn4("./Data/S5 Vignettes_Round_5__MTurk.csv",10,"mTurk")

############ :Reddit data ##################
#dat5r.l <- TrustDataFcn2("./Data/S5 Vignettes_Round_5__Reddit.csv",15,"Reddit")

############# :Flyer data ###################
dat5f.l <- TrustDataFcn3("./Data/S5 Vignettes_Round_5__Flyers.csv",10,"Flyers")

############# :Combine all Study 5 data files
dat5all.l <- rbind(dat5s.l,dat5m.l,dat5f.l)
# dat5all.l <- rbind(dat5s.l,dat5m.l,dat5r.l,dat5f.l)
dat5all.l$source <- as.factor(dat5all.l$source) # refactor the source for later

dat5.w <- reshape(dat5all.l,v.names=names(dat5all.l)[3:9],timevar="scen",idvar="id",direction="wide")

######################### CARELESS RESPONSE/MISSING DATA ###########################
## We only have CR variables for Studies 1 and 3 because they needed to be embedded in the 
## longer discriminant measures.  However, we do have the time cut-offs as a good indicator
## of carelessness in responding.  We can gauge whether that is the best metric on which to
## rely for selection of the data.

## Study 1
dat.cr1 <- read.csv("./Data/S1 Trust Data - cleaned_Jan2016.csv", header=T)
dat.cr1 <- dat.cr1[,c("IRI_25","MACHIV_5")]
names(dat.cr1) <- c("CR1","CR2")

# recode
dat.cr1$CR1 <- recode(dat.cr1[,1],"2=1;else=NA")
dat.cr1$CR2 <- recode(dat.cr1[,2],"5=1;else=NA")

summary(dat.cr1)
summary(is.na(dat.cr1))

## Study 3
## Note: I know there's a more efficient way but just bear with me here...quick and dirty...
dat3s <- read.csv("./Data/S3 Vignettes_Round_3_SONA_10302016.csv",header=T) 
dat3sTIME <- as.numeric((strptime(as.character(dat3s$V9),"%m/%d/%Y %H:%M") - strptime(as.character(dat3s$V8),"%m/%d/%Y %H:%M"))/60)
table(dat3sTIME > 10)
dat3s <- dat3s[dat3sTIME > 10,]
dat3s <- dat3s[,c("Q28_2","Q19_7","Q27_4","Q131_9")]
dat3m <- read.csv("./Data/S3 Vignettes_Round_3__MTurk_Updated.csv",header=T)
dat3mTIME <- as.numeric((strptime(as.character(dat3m$V9),"%m/%d/%Y %H:%M") - strptime(as.character(dat3m$V8),"%m/%d/%Y %H:%M"))/60)
table(dat3mTIME > 10)
dat3m <- dat3m[dat3mTIME > 10,]
dat3m <- dat3m[,c("Q28_2","Q19_7","Q27_4","Q131_9")]
dat3r <- read.csv("./Data/S3 Vignettes_Round_3__Reddit.csv",header=T)
dat3rTIME <- as.numeric((strptime(as.character(dat3r$V9),"%m/%d/%Y %H:%M") - strptime(as.character(dat3r$V8),"%m/%d/%Y %H:%M"))/60)
table(dat3rTIME > 10)
dat3r <- dat3r[dat3sTIME > 10,]
dat3r <- dat3r[,c("Q28_2","Q19_7","Q27_4","Q131_9")]
dat3f <- read.csv("./Data/S3 Vignettes_Round_3__Flyers.csv",header=T)
dat3fTIME <- as.numeric((strptime(as.character(dat3f$V9),"%m/%d/%Y %H:%M") - strptime(as.character(dat3f$V8),"%m/%d/%Y %H:%M"))/60)
table(dat3fTIME > 10)
dat3f <- dat3f[dat3fTIME > 10,]
dat3f <- dat3f[,c("Q28_2","Q19_7","Q27_4","Q131_9")]
dat.cr3 <- rbind(dat3s,dat3m,dat3r,dat3f)
names(dat.cr3) <- c("CR1","CR2","CR3","CR4")

#recode
for (i in c("CR1","CR2","CR4"))
  dat.cr3[,i] <- recode(dat.cr3[,i],"5=1;else=NA")

dat.cr3$CR3 <- recode(dat.cr3[,3],"2=1;else=NA")

summary(dat.cr3)
summary(is.na(dat.cr3))

## No missing values for any scenarios across studies
summary(is.na(dat1.l)) ## Study 1
summary(is.na(dat2.l)) ## Study 2 
summary(is.na(dat3all.l)) ## Study 3 
summary(is.na(dat4.l)) ## Study 4
summary(is.na(dat5all.l)) ## Study 5 

############################# CLEAN UP ENVIRONMENT #################################
rm(list=ls()[!(ls() %in% c('dat1.l','dat2.l','dat3s.l','dat3m.l','dat3r.l','dat3f.l','dat4.l','dat3all.l','TrustDataFcn',
                           'TrustDataFcn2','subONE','QualBeh','Ufold','ETM.Fcn','dat5s.l','dat5f.l','dat5m.l','dat5all.l',
                           'dat5.w','TrustDataFcn3','TrustDataFcn4','TraitMeasFcn','TraitMeasFcn2','TraitMeasFcn3'))])

## reload packages
library(car)
library(psych)
library(ggplot2)


######################### DATA RECODING AND MANIPULATION ###########################

########### Study 1 --------------
dat1.l <- subONE(dat1.l,2:5)
dat1.lUr <- Ufold(dat1.l,4)

########### Study 2 --------------
dat2.lUr <- Ufold(dat2.l,5)

########## Study 3 ---------------
dat3all.l <- subONE(dat3all.l,c(3:7,9)) # subtract one from qualtrics data
dat3all.l <- QualBeh(dat3all.l,8) # create binary behavior scale
dat3all.lUr <- Ufold(dat3all.l,c(4,5,9)) # fold uncertainty scale

########## Study 4 ---------------
dat4.l <- subONE(dat4.l,c(3:7,9)) # subtract one from qualtrics data
dat4.l <- QualBeh(dat4.l,8) # create binary behavior scale
dat4.lUr <- Ufold(dat4.l,c(4,5,9)) # fold uncertainty scale

########## Study 5 ---------------
dat5.l <- subONE(dat5all.l,c(3:7,9)) # subtract one from qualtrics data
dat5.l <- QualBeh(dat5all.l,8) # create binary behavior scale
dat5.lUr <- Ufold(dat5all.l,c(4,5,9)) # fold uncertainty scale

########################### LINEAR MODELS AND PLOTS ###################################

############# Study 1 ----------

S1 <- ETM.Fcn(dat1.l,"S1out.pdf")
S1T <- ETM.Fcn(dat1.lUr,"S1outT.pdf")
## Interaction for Study 1 with regular U and folded U
## Regular U: Significant, explaining 3.5% variance.
## Folded U: Significant, explaining 3.9% variance.

############# Study 2 ----------

S2 <- ETM.Fcn(dat2.l,"S2out.pdf")
S2T <- ETM.Fcn(dat2.lUr,"S2outT.pdf")
## Interaction for Study 2 (First Vignettes Pilot w/ overall U) with regular U and folded U
## Regular U: Significant, explaining 20.2% variance.
## Folded U: Significant, explaining 22% variance.

############# Study 3 ----------
S3 <- ETM.Fcn(dat3all.l,"S3out.pdf")
S3T <- ETM.Fcn(dat3all.l,"S3outT.pdf")
## Interaction for Study 3 (Revised Vignettes - best 8 and U1) with regular U and folded U
## Regular U: Significant, explaining 21.6% variance.
## Folded U: Significant, explaining 9.28% variance.

############ Study 4 ----------
S4 <- ETM.Fcn(dat4.l,"S4out.pdf")
S4 <- ETM.Fcn(dat4.lUr,"S4outT.pdf")
## Interaction for Study 4 (Just Vignettes with U1) with regular U and folded U
## Regular U: Significant, explaining 19.5% variance.
## Folded U: Significant, explaining 10.8% variance.

############# Study 5 ----------
S5a <- ETM.Fcn(subset(dat5.l,dat5.l$scen < 9),"S5outA.pdf")
S5Ua <- ETM.Fcn(subset(dat5.lUr,dat5.lUr$scen < 9),"S5outTA.pdf")

S5b <- ETM.Fcn(subset(dat5.l,dat5.l$scen > 8),"S5outB.pdf")
S5Ub <- ETM.Fcn(subset(dat5.lUr,dat5.lUr$scen > 8),"S5outTB.pdf")

S5 <- ETM.Fcn(dat5.l,"S5out.pdf")
S5 <- ETM.Fcn(dat5.lUr,"S5outT.pdf")

## Interaction for Study 5 (Dissertation) with regular U and folded U
## Regular U: Significant, explaining 23% variance.
## Folded U: Significant, explaining 9% variance.


####################### Latent Variable Models ################

library(lavaan)

### for the long data


## Study 1
LVmodel <- '
F =~ G + U + R
T ~ F
'
fit1 <- cfa(LVmodel,data=dat1.l,auto.fix.first=T)
summary(fit1, fit.measures=TRUE)

## Study 2
fit2 <- cfa(LVmodel,data=dat2.l,auto.fix.first=T)
summary(fit2, fit.measures=TRUE)

## Study 3
LVmodel2 <- '
F =~ G + U1 + R
T ~ F
'
fit3 <- cfa(LVmodel2,data=dat3all.l,auto.fix.first=T)
summary(fit3, fit.measures=TRUE)

## Study 4
fit4 <- cfa(LVmodel2,data=dat4.l,auto.fix.first=T)
summary(fit4, fit.measures=TRUE)

## Study 5
fit5 <- cfa(LVmodel2,data=dat5.l,auto.fix.first=T)
summary(fit5, fit.measures=TRUE)


fit5a <- cfa(LVmodel2,data=dat5.l,auto.fix.first=T)
summary(fit5a, fit.measures=TRUE)
parameterEstimates(fit5a)
standardizedSolution(fit5a)
fitMeasures(fit5a)
modificationIndices(fit5a,T,T)


## based upon the modification indices, we ought to run...
LVmodelMOD <- '
F =~ G + U1 + R + T
'
fit5m <- cfa(LVmodelMOD,data=dat5.l,auto.fix.first=T)
summary(fit5m, fit.measures=TRUE)
parameterEstimates(fit5m)
standardizedSolution(fit5m)
fitMeasures(fit5m)
modificationIndices(fit5m,T,T)

### for the wide data

## (see below where I created the dat5.w for the trait analysis)


####### Latent variable structures now tested with the wide data format

# Study 5

dat5.w <- reshape(dat5.l,v.names=names(dat5.l)[3:9],timevar="scen",idvar="id",direction="wide")

library(lavaan)

LVmodel2 <- '
G =~ G.1 + G.2 + G.3 + G.4 + G.5 + G.6 + G.7 + G.8 + G.9 + G.10 + G.11 + G.12 + G.13 + G.14 + G.15 + G.16
U =~ U1.1 + U1.2 + U1.3 + U1.4 + U1.5 + U1.6 + U1.7 + U1.8 + U1.9 + U1.10 + U1.11 + U1.12 + U1.13 + U1.14 + U1.15 + U1.16
R =~ R.1 + R.2 + R.3 + R.4 + R.5 + R.6 + R.7 + R.8 + R.9 + R.10 + R.11 + R.12 + R.13 + R.14 + R.15 + R.16
T =~ G + U + R
'
fitw <- cfa(LVmodel2,data=dat5.w)
summary(fitw,fit.measures=TRUE)


####################### Mediation Models ######################
MedModel <- '
T ~ c1*U1 + c2*G
R ~ a1*U1 + a2*G
T ~ b1*R
inE := a1*b + a2*b
tot := c1 + c2 + (a1*b + a2*b)
'

fitm1a <- sem(MedModel,data=dat3all.l,se="bootstrap")
summary(fitm1a)
# lavaan (0.5-23.1097) converged normally after  15 iterations
# 
# Used       Total
# Number of observations                          1172        1192
# 
# Estimator                                         ML
# Minimum Function Test Statistic                0.000
# Degrees of freedom                                 0
# Minimum Function Value               0.0000000000000
# 
# Parameter Estimates:
#   
#   Information                                 Observed
# Standard Errors                            Bootstrap
# Number of requested bootstrap draws             1000
# Number of successful bootstrap draws            1000
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)
# T ~                                                 
#   U1        (c1)    0.169    0.022    7.563    0.000
# G         (c2)   -0.112    0.019   -5.856    0.000
# R ~                                                 
#   U1        (a1)   -0.040    0.036   -1.091    0.275
# G         (a2)    0.008    0.027    0.315    0.753
# T ~                                                 
#   R          (b)    0.735    0.022   33.659    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)
# .T                 3.405    0.194   17.519    0.000
# .R                 7.843    0.240   32.620    0.000
# 
# Defined Parameters:
#   Estimate  Std.Err  z-value  P(>|z|)
# inE              -0.023    0.029   -0.799    0.424
# tot               0.034    0.037    0.900    0.368

fitm1b <- sem(MedModel,data=dat4.l,se="bootstrap")
summary(fitm1b)
# lavaan (0.5-23.1097) converged normally after  15 iterations
# 
# Used       Total
# Number of observations                          1138        1192
# 
# Estimator                                         ML
# Minimum Function Test Statistic                0.000
# Degrees of freedom                                 0
# 
# Parameter Estimates:
#   
#   Information                                 Observed
# Standard Errors                            Bootstrap
# Number of requested bootstrap draws             1000
# Number of successful bootstrap draws            1000
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)
# T ~                                                 
#   U1        (c1)    0.091    0.023    3.970    0.000
# G         (c2)   -0.114    0.019   -6.123    0.000
# R ~                                                 
#   U1        (a1)   -0.033    0.035   -0.938    0.348
# G         (a2)    0.054    0.025    2.122    0.034
# T ~                                                 
#   R          (b)    0.745    0.023   32.150    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)
# .T                 3.033    0.204   14.903    0.000
# .R                 7.039    0.242   29.128    0.000
# 
# Defined Parameters:
#   Estimate  Std.Err  z-value  P(>|z|)
# inE               0.016    0.029    0.544    0.586
# tot              -0.007    0.039   -0.185    0.853

fitm1c <- sem(MedModel,data=dat5.l,se="bootstrap")
summary(fitm1c)
# lavaan (0.5-23.1097) converged normally after  16 iterations
# 
# Used       Total
# Number of observations                          3698        4048
# 
# Estimator                                         ML
# Minimum Function Test Statistic                0.000
# Degrees of freedom                                 0
# Minimum Function Value               0.0000000000000
# 
# Parameter Estimates:
#   
#   Information                                 Observed
# Standard Errors                            Bootstrap
# Number of requested bootstrap draws             1000
# Number of successful bootstrap draws            1000
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)
# T ~                                                 
#   U1        (c1)    0.097    0.017    5.842    0.000
# G         (c2)   -0.132    0.013  -10.111    0.000
# R ~                                                 
#   U1        (a1)    0.080    0.024    3.392    0.001
# G         (a2)    0.231    0.018   12.958    0.000
# T ~                                                 
#   R          (b)    0.655    0.014   47.225    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)
# .T                 4.016    0.137   29.387    0.000
# .R                 8.263    0.155   53.369    0.000
# 
# Defined Parameters:
#   Estimate  Std.Err  z-value  P(>|z|)
# inE               0.204    0.015   14.034    0.000
# tot               0.169    0.020    8.310    0.000


## Create Interaction Variables

# Data set 3
dat3all.l$GU1 <- dat3all.l$G*dat3all.l$U1
dat3all.l$RU2 <- dat3all.l$R*dat3all.l$U2
# Data set 4
dat4.l$GU1 <- dat4.l$G*dat4.l$U1
dat4.l$RU2 <- dat4.l$R*dat4.l$U2
# Data set 5
dat5.l$GU1 <- dat5.l$G*dat5.l$U1
dat5.l$RU2 <- dat5.l$R*dat5.l$U2

MedModel2 <- '
T ~ c1*GU1
R ~ a1*GU1 
T ~ b*RU2
inE := a1*b 
tot := c1 + a1*b
'
fitm2a <- sem(MedModel2,data=dat3all.l,se="bootstrap")
summary(fitm2a)
# lavaan (0.5-23.1097) converged normally after  35 iterations
# 
# Used       Total
# Number of observations                          1172        1192
# 
# Estimator                                         ML
# Minimum Function Test Statistic             1553.491
# Degrees of freedom                                 1
# P-value (Chi-square)                           0.000
# 
# Parameter Estimates:
#   
#   Information                                 Observed
# Standard Errors                            Bootstrap
# Number of requested bootstrap draws             1000
# Number of successful bootstrap draws            1000
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)
# T ~                                                 
#   GU1       (c1)   -0.005    0.002   -2.456    0.014
# R ~                                                 
#   GU1       (a1)    0.000    0.003    0.124    0.901
# T ~                                                 
#   RU2        (b)    0.033    0.004    7.421    0.000
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)
# .T ~~                                                
#   .R                 3.547    0.423    8.379    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)
# .T                 5.048    0.338   14.945    0.000
# .R                 7.853    0.266   29.467    0.000
# 
# Defined Parameters:
#   Estimate  Std.Err  z-value  P(>|z|)
# inE               0.000    0.000    0.121    0.904
# tot              -0.005    0.002   -2.398    0.016

fitm2b <- sem(MedModel2,data=dat4.l,se="bootstrap")
summary(fitm2b)
# lavaan (0.5-23.1097) converged normally after  38 iterations
# 
# Used       Total
# Number of observations                          1138        1192
# 
# Estimator                                         ML
# Minimum Function Test Statistic             1549.249
# Degrees of freedom                                 1
# P-value (Chi-square)                           0.000
# 
# Parameter Estimates:
#   
#   Information                                 Observed
# Standard Errors                            Bootstrap
# Number of requested bootstrap draws             1000
# Number of successful bootstrap draws            1000
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)
# T ~                                                 
#   GU1       (c1)   -0.007    0.002   -3.126    0.002
# R ~                                                 
#   GU1       (a1)    0.004    0.003    1.431    0.152
# T ~                                                 
#   RU2        (b)    0.026    0.004    6.123    0.000
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)
# .T ~~                                                
#   .R                 3.564    0.381    9.364    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)
# .T                 4.816    0.346   13.900    0.000
# .R                 7.054    0.245   28.823    0.000
# 
# Defined Parameters:
#   Estimate  Std.Err  z-value  P(>|z|)
# inE               0.000    0.000    1.352    0.176
# tot              -0.007    0.002   -3.028    0.002

fitm2c <- sem(MedModel2,data=dat5.l,se="bootstrap")
summary(fitm2c)

## Combined data set 3/4 results from Pat's code using Sdat1
# lavaan (0.5-23.1097) converged normally after  35 iterations
# 
# Used       Total
# Number of observations                          2310        2384
# 
# Estimator                                         ML
# Minimum Function Test Statistic             3100.072
# Degrees of freedom                                 1
# P-value (Chi-square)                           0.000
# 
# Parameter Estimates:
#   
#   Information                                 Observed
# Standard Errors                            Bootstrap
# Number of requested bootstrap draws             1000
# Number of successful bootstrap draws            1000
# 
# Regressions:
#                  Estimate  Std.Err  z-value  P(>|z|)
# T ~                                                 
#   GU1       (c1)   -0.006    0.002   -3.800    0.000
# R ~                                                 
#   GU1       (a1)    0.002    0.002    1.132    0.258
# T ~                                                 
#   RU2        (b)    0.030    0.003    9.364    0.000
# 
# Covariances:
#                   Estimate  Std.Err  z-value  P(>|z|)
# .T ~~                                                
#   .R                 3.554    0.293   12.114    0.000
# 
# Variances:
#                 Estimate  Std.Err  z-value  P(>|z|)
# .T                 4.933    0.248   19.900    0.000
# .R                 7.471    0.179   41.831    0.000
# 
# Defined Parameters:
#                Estimate  Std.Err  z-value  P(>|z|)
# inE               0.000    0.000    1.098    0.272
# tot              -0.006    0.002   -3.691    0.000


MedModel3 <- '
T ~ c1*U1 + c2*G
R ~ a1*U1 + a2*G
T ~ b1*R + b2*U2
inE := a1*b1 + a2*b1 + a1*b2 + a2*b2
tot := c1 + c2 + (a1*b1 + a2*b1 + a1*b2 + a2*b2)
'

fitm3a <- sem(MedModel3,data=dat3all.l,se="bootstrap")
summary(fitm3a)
# lavaan (0.5-23.1097) converged normally after  16 iterations
# 
# Used       Total
# Number of observations                          1172        1192
# 
# Estimator                                         ML
# Minimum Function Test Statistic              285.920
# Degrees of freedom                                 1
# P-value (Chi-square)                           0.000
# 
# Parameter Estimates:
#   
#   Information                                 Observed
# Standard Errors                            Bootstrap
# Number of requested bootstrap draws             1000
# Number of successful bootstrap draws            1000
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)
# T ~                                                 
#   U1        (c1)    0.151    0.023    6.456    0.000
# G         (c2)   -0.150    0.019   -7.700    0.000
# R ~                                                 
#   U1        (a1)   -0.040    0.036   -1.107    0.268
# G         (a2)    0.008    0.027    0.314    0.754
# T ~                                                 
#   R         (b1)    0.658    0.027   24.108    0.000
# U2        (b2)    0.170    0.026    6.509    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)
# .T                 3.240    0.181   17.881    0.000
# .R                 7.843    0.256   30.641    0.000
# 
# Defined Parameters:
#   Estimate  Std.Err  z-value  P(>|z|)
# inE              -0.026    0.033   -0.790    0.430
# tot              -0.024    0.041   -0.590    0.555

fitm3b <- sem(MedModel3,data=dat4.l,se="bootstrap")
summary(fitm3b)
# lavaan (0.5-23.1097) converged normally after  15 iterations
# 
# Used       Total
# Number of observations                          1138        1192
# 
# Estimator                                         ML
# Minimum Function Test Statistic              311.153
# Degrees of freedom                                 1
# P-value (Chi-square)                           0.000
# 
# Parameter Estimates:
#   
#   Information                                 Observed
# Standard Errors                            Bootstrap
# Number of requested bootstrap draws             1000
# Number of successful bootstrap draws            1000
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)
# T ~                                                 
#   U1        (c1)    0.084    0.023    3.657    0.000
# G         (c2)   -0.141    0.018   -7.725    0.000
# R ~                                                 
#   U1        (a1)   -0.033    0.036   -0.909    0.364
# G         (a2)    0.054    0.027    1.992    0.046
# T ~                                                 
#   R         (b1)    0.686    0.028   24.182    0.000
# U2        (b2)    0.120    0.027    4.420    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)
# .T                 2.956    0.194   15.276    0.000
# .R                 7.039    0.225   31.351    0.000
# 
# Defined Parameters:
#   Estimate  Std.Err  z-value  P(>|z|)
# inE               0.017    0.033    0.515    0.606
# tot              -0.040    0.042   -0.943    0.346

fitm3c <- sem(MedModel3,data=dat5.l,se="bootstrap")
summary(fitm3c)
# lavaan (0.5-23.1097) converged normally after  17 iterations
# 
# Used       Total
# Number of observations                          3698        4048
# 
# Estimator                                         ML
# Minimum Function Test Statistic             1163.959
# Degrees of freedom                                 1
# P-value (Chi-square)                           0.000
# 
# Parameter Estimates:
#   
#   Information                                 Observed
# Standard Errors                            Bootstrap
# Number of requested bootstrap draws             1000
# Number of successful bootstrap draws            1000
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)
# T ~                                                 
#   U1        (c1)    0.087    0.017    5.277    0.000
# G         (c2)   -0.168    0.013  -13.280    0.000
# R ~                                                 
#   U1        (a1)    0.080    0.023    3.477    0.001
# G         (a2)    0.231    0.017   13.364    0.000
# T ~                                                 
#   R         (b1)    0.573    0.017   34.558    0.000
# U2        (b2)    0.178    0.019    9.272    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)
# .T                 3.863    0.130   29.739    0.000
# .R                 8.263    0.162   51.083    0.000
# 
# Defined Parameters:
#   Estimate  Std.Err  z-value  P(>|z|)
# inE               0.233    0.017   13.981    0.000
# tot               0.152    0.022    6.984    0.000

####################### MISC ######################################

### Study 1
## correlation matrix and plot
round(cor(dat1.l[,2:5]),2)
cor.plot(cor(dat1.l[,2:5]))

### Study 2
## correlation matrix and plot
round(cor(dat2.l[,3:7],use="pairwise.complete.obs"),2)
cor.plot(cor(dat2.l[,3:7],use="pairwise.complete.obs"))

### Study 3
round(cor(dat3all.l[,3:9],use="pairwise.complete.obs"),2)
cor.plot(cor(dat3all.l[,3:9],use="pairwise.complete.obs"))

### Study 4
round(cor(dat4.l[,3:9],use="pairwise.complete.obs"),2)
cor.plot(cor(dat4.l[,3:9],use="pairwise.complete.obs"))

### Study 5
round(cor(dat5.l[,3:9],use="pairwise.complete.obs"),2)
cor.plot(cor(dat5.l[,3:9],use="pairwise.complete.obs"))

### Descriptives
## Study 1
library(doBy)

summaryBy(T~scen, data=dat1.l, FUN=c(mean,sd))
#   scen   T.mean     T.sd
# 1    1 9.303371 1.621916
# 2    2 7.157303 1.771710
# 3    3 5.067416 2.106207
# 4    4 3.438202 2.244668
# 5    5 1.544944 2.178659

summaryBy(G~scen, data=dat1.l, FUN=c(mean,sd))
#   scen   G.mean     G.sd
# 1    1 8.780899 2.102534
# 2    2 8.348315 2.139756
# 3    3 7.803371 2.486254
# 4    4 7.348315 2.597701
# 5    5 7.101124 3.164224

summaryBy(R~scen, data=dat1.l, FUN=c(mean,sd))
#   scen   R.mean     R.sd
# 1    1 8.887640 2.093510
# 2    2 8.106742 2.046041
# 3    3 7.365169 2.598635
# 4    4 6.674157 3.145350
# 5    5 5.949438 3.827159

# Unflipped
summaryBy(U~scen, data=dat1.l, FUN=c(mean,sd))
#   scen   U.mean     U.sd
# 1    1 2.151685 3.166656
# 2    2 5.174157 2.736653
# 3    3 6.651685 2.235316
# 4    4 6.910112 2.714849
# 5    5 6.904494 3.521826

# Flipped
summaryBy(U~scen, data=dat1.lUr, FUN=c(mean,sd))
#   scen   U.mean     U.sd
# 1    1 1.988764 2.864134
# 2    2 5.426966 3.008041
# 3    3 5.685393 3.498492
# 4    4 4.179775 3.176758
# 5    5 2.730337 3.325179

########### EMERGENT MODEL TESTED VIA PCA ##############

# Study 1
pca1 <- princomp(dat1.l[complete.cases(dat1.l[,c("G","U","R")]),c("G","U","R")])
# Importance of components:
#                           Comp.1    Comp.2    Comp.3
# Standard deviation     3.5132647 3.2112558 2.1923603
# Proportion of Variance 0.4494644 0.3755116 0.1750239
# Cumulative Proportion  0.4494644 0.8249761 1.0000000

round(summary(lm(dat1.l[complete.cases(dat1.l[,c("G","U","R")]),"T"]~pca1$scores[,1]))$adj.r.squared,2)
### 37%

# Study 2
pca2 <- princomp(dat2.l[complete.cases(dat2.l[,c("G","U","R")]),c("G","U","R")])
# Importance of components:
#                           Comp.1    Comp.2    Comp.3
# Standard deviation     3.7160806 2.5915810 2.4958303
# Proportion of Variance 0.5161428 0.2510321 0.2328251
# Cumulative Proportion  0.5161428 0.7671749 1.0000000

round(summary(lm(dat2.l[complete.cases(dat2.l[,c("G","U","R")]),"T"]~pca2$scores[,1]))$adj.r.squared,2)
### 17%

# Study 3
pca3 <- princomp(dat3all.l[complete.cases(dat3all.l[,c("G","U1","R")]),c("G","U1","R")])
# Importance of components:
#                           Comp.1    Comp.2    Comp.3
# Standard deviation     3.4623242 2.8069810 2.4060140
# Proportion of Variance 0.4672518 0.3071104 0.2256378
# Cumulative Proportion  0.4672518 0.7743622 1.0000000

round(summary(lm(dat3all.l[complete.cases(dat3all.l[,c("G","U1","R")]),"T"]~pca3$scores[,1]))$adj.r.squared,2)
### 1%

summary(lm(dat3all.l[complete.cases(dat3all.l[,c("G","U1","R")]),"T"]~pca3$scores[,1]))

tmp <- dat3all.l[complete.cases(dat3all.l[,c("G","U1","R")]),"T"]
tmp <- data.frame(trust=tmp,pca1=pca3$scores[,1])
str(tmp)
ggplot(tmp,aes(x=pca1,y=trust))+geom_smooth()
## well shit!  something is wrong with our PCA results.  Do not fear!

### We discovered a problem with princomp, let's use psych::principal

pca3a <- psych::principal(dat3all.l[complete.cases(dat3all.l[,c("G","U1","R")]),c("G","U1","R")])
summary(pca3a)


# Study 4
pca4 <- princomp(dat4.l[complete.cases(dat4.l[,c("G","U1","R")]),c("G","U1","R")])
# Importance of components:
#                           Comp.1    Comp.2    Comp.3
# Standard deviation     3.2291908 2.6585070 2.3899224
# Proportion of Variance 0.4493319 0.3045478 0.2461203
# Cumulative Proportion  0.4493319 0.7538797 1.0000000

round(summary(lm(dat4.l[complete.cases(dat4.l[,c("G","U1","R")]),"T"]~pca4$scores[,1]))$adj.r.squared,2)
### 0%

# Study 5
pca5 <- princomp(dat5.l[complete.cases(dat5.l[,c("G","U1","R")]),c("G","U1","R")])
# Importance of components:
#                           Comp.1    Comp.2    Comp.3
# Standard deviation     3.7966597 2.7016722 2.1481482
# Proportion of Variance 0.5474976 0.2772325 0.1752699
# Cumulative Proportion  0.5474976 0.8247301 1.0000000

round(summary(lm(dat5.l[complete.cases(dat5.l[,c("G","U1","R")]),"T"]~pca5$scores[,1]))$adj.r.squared,2)
### 13%

########### LATENT MODEL TESTED VIA EFA ##############

# Study 1
efa1 <- factanal(dat1.l[complete.cases(dat1.l[,c("G","U","R")]),c("G","U","R")],1,scores="regression")
# Uniquenesses:
#   G     U     R 
# 0.653 0.987 0.616 
# 
# Loadings:
#   Factor1
# G  0.589 
# U -0.114 
# R  0.620 
# 
# Factor1
# SS loadings      0.744
# Proportion Var   0.248
# The degrees of freedom for the model is 0 and the fit was 0 

round(summary(lm(dat1.l[complete.cases(dat1.l[,c("G","U","R")]),"T"]~efa1$scores[,1]))$adj.r.squared,2)
## 18% 

# Study 2
efa2 <- factanal(dat2.l[complete.cases(dat2.l[,c("G","U","R")]),c("G","U","R")],1,scores="regression")
# Uniquenesses:
#   G     U     R 
# 0.576 0.838 0.765 
# 
# Loadings:
#   Factor1
# G 0.651  
# U 0.403  
# R 0.485  
# 
# Factor1
# SS loadings      0.821
# Proportion Var   0.274
# 
# The degrees of freedom for the model is 0 and the fit was 0 

round(summary(lm(dat2.l[complete.cases(dat2.l[,c("G","U","R")]),"T"]~efa2$scores[,1]))$adj.r.squared,2)
## 12% 

# Study 3
efa3 <- factanal(dat3all.l[complete.cases(dat3all.l[,c("G","U1","R")]),c("G","U1","R")],1,scores="regression")
# Uniquenesses:
#   G    U1     R 
# 0.960 0.005 0.999 
# 
# Loadings:
#   Factor1
# G   0.200 
# U1  0.997 
# R         
# 
# Factor1
# SS loadings      1.036
# Proportion Var   0.345
# 
# The degrees of freedom for the model is 0 and the fit was 1e-04 

round(summary(lm(dat3all.l[complete.cases(dat3all.l[,c("G","U1","R")]),"T"]~efa3$scores[,1]))$adj.r.squared,2)
## 1% 

# Study 4
efa4 <- factanal(dat4.l[complete.cases(dat4.l[,c("G","U1","R")]),c("G","U1","R")],1,scores="regression")
# Uniquenesses:
#   G    U1     R 
# 0.005 0.974 0.996 
# 
# Loadings:
#   Factor1
# G  0.997  
# U1 0.161  
# R         
# 
# Factor1
# SS loadings      1.025
# Proportion Var   0.342
# 
# The degrees of freedom for the model is 0 and the fit was 9e-04 

round(summary(lm(dat4.l[complete.cases(dat4.l[,c("G","U1","R")]),"T"]~efa4$scores[,1]))$adj.r.squared,2)
## 1% 

# Study 5
efa5 <- factanal(dat5.l[complete.cases(dat5.l[,c("G","U1","R")]),c("G","U1","R")],1,scores="regression")
# Uniquenesses:
#   G    U1     R 
# 0.330 0.746 0.883 
# 
# Loadings:
#   Factor1
# G  0.819  
# U1 0.504  
# R  0.342  
# 
# Factor1
# SS loadings      1.041
# Proportion Var   0.347
# 
# The degrees of freedom for the model is 0 and the fit was 0 

round(summary(lm(dat5.l[complete.cases(dat5.l[,c("G","U1","R")]),"T"]~efa5$scores[,1]))$adj.r.squared,2)
## 4% 


####################### Correlation Matrices for ETM ######################################

### Study 1
## correlation matrix and plot
round(cor(dat1.l[,2:5]),2)
cor.plot(cor(dat1.l[,2:5]))
#       T     G     R     U
# T  1.00  0.25  0.39 -0.49
# G  0.25  1.00  0.37 -0.07
# R  0.39  0.37  1.00 -0.07
# U -0.49 -0.07 -0.07  1.00

### Study 2
## correlation matrix and plot
round(cor(dat2.l[,4:8],use="pairwise.complete.obs"),2)
cor.plot(cor(dat2.l[,4:8],use="pairwise.complete.obs"))
#       G     U     R     T     B
# G  1.00  0.26  0.32  0.06 -0.05
# U  0.26  1.00  0.20  0.23 -0.09
# R  0.32  0.20  1.00  0.66 -0.56
# T  0.06  0.23  0.66  1.00 -0.48
# B -0.05 -0.09 -0.56 -0.48  1.00

### Study 3
round(cor(dat3all.l[,3:9],use="pairwise.complete.obs"),2)
cor.plot(cor(dat3all.l[,3:9],use="pairwise.complete.obs"))
#        G    U1   U2     R     T     B   U3
# G   1.00  0.20 0.28  0.00 -0.10 -0.20 0.02
# U1  0.20  1.00 0.13 -0.03  0.10  0.02 0.55
# U2  0.28  0.13 1.00  0.44  0.43  0.35 0.17
# R   0.00 -0.03 0.44  1.00  0.73  0.47 0.17
# T  -0.10  0.10 0.43  0.73  1.00  0.51 0.31
# B  -0.20  0.02 0.35  0.47  0.51  1.00 0.19
# U3  0.02  0.55 0.17  0.17  0.31  0.19 1.00

### Study 4
round(cor(dat4.l[,3:9],use="pairwise.complete.obs"),2)
cor.plot(cor(dat4.l[,3:9],use="pairwise.complete.obs"))
#        G    U1   U2     R     T     B   U3
# G   1.00  0.16 0.29  0.06 -0.08 -0.19 0.02
# U1  0.16  1.00 0.08 -0.02  0.05 -0.05 0.51
# U2  0.29  0.08 1.00  0.48  0.42  0.31 0.12
# R   0.06 -0.02 0.48  1.00  0.74  0.38 0.09
# T  -0.08  0.05 0.42  0.74  1.00  0.42 0.27
# B  -0.19 -0.05 0.31  0.38  0.42  1.00 0.10
# U3  0.02  0.51 0.12  0.09  0.27  0.10 1.00

### Study 5
round(cor(dat5.l[,3:9],use="pairwise.complete.obs"),2)
cor.plot(cor(dat5.l[,3:9],use="pairwise.complete.obs"))
#        G   U1   U2    R    T     B   U3
# G   1.00 0.41 0.40 0.28 0.08 -0.04 0.19
# U1  0.41 1.00 0.24 0.17 0.15  0.06 0.38
# U2  0.40 0.24 1.00 0.57 0.47  0.28 0.26
# R   0.28 0.17 0.57 1.00 0.68  0.40 0.29
# T   0.08 0.15 0.47 0.68 1.00  0.39 0.38
# B  -0.04 0.06 0.28 0.40 0.39  1.00 0.02
# U3  0.19 0.38 0.26 0.29 0.38  0.02 1.00

## Overall
## Study 3-5
## U1: strength increased between U1 and G across studies. Medium strength with U3 too.
## U2: Med-Strong relationship with R and T (as expected). Med w/ G but weak with B.
## U3: Weak-Med with T. Asked after B but almost no relationship to B.
## G: Med relationship with U1 and U2, improved across studies. No relationship with T.
## R: Strongest relationship with T and Med with B, U3. Something important with R - big weight?


################## TRAIT MEASURES #######################
## Study 1
TraitMeasFcnDat1 <- function(x=NULL,cut.time=10,source=NULL){
  dat <- read.csv(x,header=T)
  datTIME <- as.numeric((strptime(as.character(dat$V9),"%m/%d/%Y %H:%M") - strptime(as.character(dat$V8),"%m/%d/%Y %H:%M"))/60)
  dat <- dat[datTIME > cut.time,]
  dat$id <- c(1:nrow(dat))
  out <- dat[,c("id","GSS",paste("GTS",1:6,sep="_"),paste("WVS",1:6,sep="_"),paste("ERI",1:32,sep="_"),paste("NGSE",1:8,sep="_"),paste("BMIS",1:16,sep="_"),"BMIS2",paste("IPIP",1:20,sep="_"),paste("Q60",1:6,sep="_"),paste("RITS",c(1:25),sep="_"),"D1","D3","D4","D5","D7","D8","D9","D11","D12","D13")] #,paste("Q295",1:32,sep="_"),paste("Q294",1:24,sep="_")] ## No ERI or BISBAS except for Turk
  names(out) <- c("id","GSS",paste("GTS",1:6,sep="."),paste("WVS",1:6,sep="."),paste("ERI",1:32,sep="."),paste("NGSE",1:8,sep="."),paste("BMIS",1:16,sep="."),"BMIS.mood",paste("IPIP",1:20,sep="."),paste("LOTR",1:6,sep="."),paste("RITS",1:25,sep="."),"female","age","ethnicity","country","RelStatus","educ","employ","livingArr","income","religion") #,paste("ERI",1:32,sep="."),paste("BISBAS",1:24,sep="."))
  out$female <- out$female - 1
  return(out)
}

dat1 <- read.csv("./Data/S1 Trust Data - cleaned_Jan2016.csv", header=T)
S1traits <- dat1[,c(4,54:73,74:97,99:102,103:108,109,110:115,116:119,121:136,137:160,161:173,174:205,206:230,231:238,239:255,256:272)]
names(S1traits)
names(S1traits) <- c("id",paste("IPIP",1:20,sep="."),paste("IRI",1:28,sep="."),paste("GTS",1:6,sep="."),"GSS",paste("WVS",1:6,sep="."),
                     paste("MACHIV",1:20,sep="."),paste("BISBAS",1:24,sep="."),paste("RSQ",1:13,sep="."),paste("ERI",1:32,sep="."),
                     paste("RITS",1:25,sep="."),paste("NGSE",1:8,sep="."),paste("BMIS1",1:16,sep="."),"BMIS2","female","DOB","age","ethnicity",
                     "eth_text","country","local","RelStatus","Rel_text","educ","employ","employ_text","job","livingArr","livingArr_text","income","religion") 
names(S1traits)
S1traits <- S1traits[,c(1:202,204,205,207,209,211,212,214,215,217,218)]
names(S1traits)
S1traits$female <- S1traits$female-1
S1traits$female <- as.factor(S1traits$female)
S1traits$ethnicity <- as.factor(S1traits$ethnicity)
S1traits$RelStatus <- as.factor(S1traits$RelStatus)
S1traits$educ <- as.factor(S1traits$educ)
S1traits$employ <- as.factor(S1traits$employ)
S1traits$livingArr <- as.factor(S1traits$livingArr)
S1traits$income <- as.factor(S1traits$income)
S1traits$religion <- as.factor(S1traits$religion)
str(S1traits)


# ## Something is wrong here. Identify FIX.
# S1traits <- TraitMeasFcnDat1("./Data/S1 Trust Data - cleaned_Jan2016.csv",10,"mTurk")
# str(S1traits)
# summary(complete.cases(S1traits))

## IRI 24, MACHIV 5 - CR variables 

## Study 2
## No data collected - vignette pilot

## Study 3
TraitMeasFcnDat3 <- function(x=NULL,cut.time=10,source=NULL){
  dat <- read.csv(x,header=T)
  datTIME <- as.numeric((strptime(as.character(dat$V9),"%m/%d/%Y %H:%M") - strptime(as.character(dat$V8),"%m/%d/%Y %H:%M"))/60)
  dat <- dat[datTIME > cut.time,]
  dat$id <- c(1:nrow(dat))
  out <- dat[,c("id","Q17",paste("Q18",1:6,sep="_"),paste("Q25",1:6,sep="_"),paste("Q28",1:13,sep="_"),paste("Q19",1:9,sep="_"),"Q30","Q30.1",paste("Q27",1:21,sep="_"),paste("Q60",1:6,sep="_"),paste("Q131",c(1:16),sep="_"),"Q2","Q4","Q5","Q6","Q8","Q9","Q10","Q12","Q13","Q14")] #,paste("Q295",1:32,sep="_"),paste("Q294",1:24,sep="_")] ## No ERI or BISBAS except for Turk
  names(out) <- c("id","GSS",paste("GTS",1:6,sep="."),paste("WVS",1:6,sep="."),paste("IUS",1:13,sep="."),paste("NGSE",1:9,sep="."),"mood","energy",paste("IPIP",1:21,sep="."),paste("LOTR",1:6,sep="."),paste("AUT",1:16,sep="."),"female","age","ethnicity","country","RelStatus","educ","employ","livingArr","income","religion") #,paste("ERI",1:32,sep="."),paste("BISBAS",1:24,sep="."))
  out$female <- out$female - 1
  return(out)
}

S3traitsM <- TraitMeasFcnDat3("./Data/S3 Vignettes_Round_3__MTurk_Updated.csv",10,"mTurk")
S3traitsR <- TraitMeasFcnDat3("./Data/S3 Vignettes_Round_3__Reddit.csv",10,"Reddit")
S3traitsF <- TraitMeasFcnDat3("./Data/S3 Vignettes_Round_3__Flyers.csv",10,"Flyers")
S3traitsS <- TraitMeasFcnDat3("./Data/S3 Vignettes_Round_3__Flyers.csv",10,"SONA")
S3traitsAll <- rbind(S3traitsS,S3traitsM,S3traitsF,S3traitsR)
## remove IUS 2, NGSE 7, IPIP 4, AUT 9 
S3traitsAll <- S3traitsAll[,c(1:15,17:33,35:41,43:73,75:91)]
names(S3traitsAll) <- c("id","GSS",paste("GTS",1:6,sep="."),paste("WVS",1:6,sep="."),paste("IUS",1:12,sep="."),paste("NGSE",1:8,sep="."),"mood","energy",paste("IPIP",1:20,sep="."),paste("LOTR",1:6,sep="."),paste("AUT",1:15,sep="."),"female","age","ethnicity","country","RelStatus","educ","employ","livingArr","income","religion") #,paste("ERI",1:32,sep="."),paste("BISBAS",1:24,sep="."))
str(S3traitsAll)
summary(complete.cases(S3traitsAll))
## 46 complete cases

## Study 4
## No data collected - vignette only

## Study 5
S5traitsS <- TraitMeasFcn("./Data/S5 Vignettes_Round_5__SONA.csv",10,"SONA")
S5traitsM <- TraitMeasFcn2("./Data/S5 Vignettes_Round_5__MTurk.csv",10,"mTurk")
S5traitsF <- TraitMeasFcn3("./Data/S5 Vignettes_Round_5__Flyers.csv",10,"Flyers")
## 1. rescore the measures for traits
## 2. try to reduce the responses by id for G, U1, R, T, U2, U3, and B to averages and SD's (use aggregate)
## 3. merge the data frames

S5traitsAll <- rbind(S5traitsS,S5traitsM,S5traitsF)
str(S5traitsAll)
summary(complete.cases(S5traitsAll))
## Looks like we have 216 complete sets of demographics and traits measures

########################################################################
############################   SCALES   ################################
########################################################################

#################### General Social Survey - Trust Question ############################
## Study 1

## Study 3

## Study 5

################################ General Trust Survey ##################################
## Study 1
round(cor(S1traits[,50:55],use="pairwise.complete.obs"),2)
factanal(S1traits[complete.cases(S1traits[,50:55]),50:55],2) ## two factors seem to fit best
S1traits$GTS <- rowMeans(S1traits[,50:55]) ## scoring routine - average across all 6 items
library(psych)
psych::alpha(S1traits[,50:55])

## Study 3
round(cor(S3traitsAll[,3:8],use="pairwise.complete.obs"),2)
factanal(S3traitsAll[complete.cases(S3traitsAll[,3:8]),3:8],2) ## two factors seem to fit best
S3traitsAll$GTS <- rowMeans(S3traitsAll[,3:8]) ## scoring routine - average across all 6 items
library(psych)
psych::alpha(S3traitsAll[,3:8])

## Study 5

# GTS <- data.frame(S5all[,c(1:2,10,12:17)])
## Total Score
# GTS$GTStotal <- rowSums(GTS[,4:ncol(GTS)])  
# S5all$GTStotal <- GTS$GTStotal
## see:  http://fetzer.org/sites/default/files/images/stories/pdf/selfmeasures/Self_Measures_for_Love_and_Compassion_Research_TRUST.pdf
round(cor(S5traitsAll[,3:8],use="pairwise.complete.obs"),2)
factanal(S5traitsAll[complete.cases(S5traitsAll[,3:8]),3:8],2) ## two factors seem to fit best
S5traitsAll$GTS <- rowMeans(S5traitsAll[,3:8]) ## scoring routine - average across all 6 items
library(psych)
psych::alpha(S5traitsAll[,3:8])

##################### World Values Survey - Trust Questions ############################
## Study 1
round(cor(S1traits[,57:62],use="pairwise.complete.obs"),2)
factanal(S1traits[complete.cases(S1traits[,57:62]),57:62],2) ## two factors seem to fit best
S1traits$WVS <- rowMeans(S1traits[,57:62]) ## due to high alpha, kept same scoring as GTS by average across all 6 items
psych::alpha(S1traits[,57:62])

## Study 3
round(cor(S3traitsAll[,9:14],use="pairwise.complete.obs"),2)
factanal(S3traitsAll[complete.cases(S3traitsAll[,9:14]),9:14],2) ## two factors seem to fit best
S3traitsAll$WVS <- rowMeans(S3traitsAll[,9:14]) ## due to high alpha, kept same scoring as GTS by average across all 6 items
psych::alpha(S3traitsAll[,9:14])

## Study 5
# WVS <- data.frame(S5all[,c(1:2,10,18:23)])
## Total Score
# WVS$WVStotal <- rowSums(WVS[,4:ncol(WVS)])  
# S5all$WVStotal <- WVS$WVStotal
round(cor(S5traitsAll[,9:14],use="pairwise.complete.obs"),2)
factanal(S5traitsAll[complete.cases(S5traitsAll[,9:14]),9:14],2) ## two factors seem to fit best
S5traitsAll$WVS <- rowMeans(S5traitsAll[,9:14]) ## due to high alpha, kept same scoring as GTS by average across all 6 items
psych::alpha(S5traitsAll[,9:14])

# create a wide data set
dat5.w <- reshape(dat5.l,v.names=names(dat5.l)[3:9],timevar="scen",idvar="id",direction="wide")
str(dat5.w)

S5vigSums <- data.frame(id=unique(dat5.l$id))
for (i in 3:9){
  tmp <- merge(aggregate(dat5.l[,i],by=list(dat5.l$id),mean),aggregate(dat5.l[,i],by=list(dat5.l$id),sd),by="Group.1")
  names(tmp) <- c("id",paste(names(dat5.l)[i],"X",sep="."),paste(names(dat5.l)[i],"SD",sep="."))
  S5vigSums <- merge(S5vigSums,tmp,by="id")
}
str(S5vigSums)


S5all <- merge(dat5.w,S5traitsAll,by="id")
S5all <- merge(S5all,S5vigSums,by="id")
str(S5all)

names(S5all)

summary(lm(T.X~GSS+GTS+WVS+as.factor(female),data=S5all))
summary(lm(T.X~G.X+U1.X+R.X+as.factor(female),data=S5all)) ## b=-0.041, 0.12%
summary(lm(T.X~G.X+U1.X+R.X,data=S5all)) ## 72.3%
summary(lm(T.X~G.X:U1.X:R.X+as.factor(female),data=S5all))
summary(lm(T.X~G.X:U1.X:R.X,data=S5all))

summary(lm(GSS~as.factor(female),data=S5all)) ## b=0.493, 2%
summary(lm(GTS~as.factor(female),data=S5all)) ## b=0.195, 1%
summary(lm(WVS~as.factor(female),data=S5all)) ## b=0.177, 2%
summary(lm(T.X~as.factor(female),data=S5all)) ## b=-0.219, 0.2%

## Dispositional trust ratings, women reported they were more trusting than men.  
## However, scenario-based trust ratings, women on average were less trusting than men.

##################### Rotter Interpersonal Trust Scale Questions ############################
## Study 1
round(cor(S1traits[,152:176],use="pairwise.complete.obs"),2)
library(car)
for (i in c(157,159,163,165,167,168,169,172,174,176)){
  S1traits[,i] <- recode(S1traits[,i],"1=5;2=4;3=3;2=4;1=5")
}
factanal(S1traits[complete.cases(S1traits[,152:176]),152:176],3) ##  three factors seem to fit best
factanal(S1traits[complete.cases(S1traits[,152:176]),152:176],4) 
S1traits$RITS <- rowSums(S1traits[,152:176]) ## Means or sums?
psych::alpha(S1traits[,152:176])

## Finish these for Lisa
# ################################# ERI ############################################

S1traits$ERI <- rowSums(S1traits[,120:144]) ## Check sums vs means in scoring
# factanal(S1traits[complete.cases(S1traits[,152:176]),152:176],3) ##  three factors seem to fit best
# factanal(S1traits[complete.cases(S1traits[,152:176]),152:176],4) 
psych::alpha(S1traits[,152:176])

# ################################# IPIP ############################################

round(cor(S1traits[,2:21],use="pairwise.complete.obs"),2)
library(car)
for (i in c("IPIP.6","IPIP.7","IPIP.8","IPIP.9","IPIP.10","IPIP.15","IPIP.16","IPIP.17","IPIP.18","IPIP.19","IPIP.20")){
  S1traits[,i] <- recode(S1traits[,i],"1=5;2=4;3=3;2=4;1=5")
}

S1traits$IPIP.E <- rowSums(S1traits[,c("IPIP.1","IPIP.6","IPIP.11","IPIP.16")])
S1traits$IPIP.A <- rowSums(S1traits[,c("IPIP.2","IPIP.7","IPIP.12","IPIP.17")])
S1traits$IPIP.C <- rowSums(S1traits[,c("IPIP.3","IPIP.8","IPIP.13","IPIP.18")])
S1traits$IPIP.N <- rowSums(S1traits[,c("IPIP.4","IPIP.9","IPIP.14","IPIP.19")])
S1traits$IPIP.II <- rowSums(S1traits[,c("IPIP.5","IPIP.10","IPIP.15","IPIP.20")])
 
S1traits$IPIP <- S1traits$IPIP.E + S1traits$IPIP.A + S1traits$IPIP.C + S1traits$IPIP.N + S1traits$IPIP.II
#  
# factanal(S1traits[complete.cases(S1traits[,152:176]),152:176],3) ##  three factors seem to fit best
# factanal(S1traits[complete.cases(S1traits[,152:176]),152:176],4) 
psych::alpha(S1traits[,2:21])

# 
# ################################# NGSE ############################################

round(cor(S1traits[,177:184],use="pairwise.complete.obs"),2)
S1traits$NGSE <- rowSums(S1traits[,c(177:184)])
psych::alpha(S1traits[,177:184])

# ################################# BIS/BAS ############################################
library(car)
for (i in c(83,85:103,105:106)){
  S1traits[,i] <- recode(S1traits[,i],"4=1;3=2;2=3;1=4")
}

## Drive
S1traits$BAS_D <- rowSums(S1traits[,c(85,91,94,103)])
psych::alpha(S1traits[,c(85,91,94,103)])

## Fun Seeking
S1traits$BAS_FS <- rowSums(S1traits[,c(87,92,97,102)])
psych::alpha(S1traits[,c(87,92,97,102)])

## Reward R**
S1traits$BAS_RR <- rowSums(S1traits[,c(86,89,96,100,105)])
psych::alpha(S1traits[,c(86,89,96,100,105)])

## BIS
S1traits$BIS <- rowSums(S1traits[,c(84,90,95,98,101,104,106)])
psych::alpha(S1traits[,c(84,90,95,98,101,104,106)])

## BAS
S1traits$BAS <- rowSums(S1traits[,c(85,86,87,89,91,92,94,96,97,100,102,103,105)])
psych::alpha(S1traits[,c(85,86,87,89,91,92,94,96,97,100,102,103,105)])

round(cor(S1traits[,83:106],use="pairwise.complete.obs"),2)

# ################################# BMIS ############################################
# BMIS <- dat[,c(239:255)]
# 
# ## Pleasant-Unpleasant
# BMIS1 <- BMIS
# library(car)
# for (i in c(3,4,7:10,12,15)){
#   BMIS1[,i] <- recode(BMIS1[,i],"4=1;3=2;2=3;1=4")
# }
# 
# BMIS$P <- rowSums(BMIS1[,c(1,2,5,6,11,13,14,16)])
# BMIS$U <- rowSums(BMIS1[,c(3,4,7:10,12,15)])
# BMIS$PU <- BMIS$P + BMIS$U
# 
# ## Arousal-Calm
# BMIS2 <- BMIS
# library(car)
# for (i in c(4,13)){
#   BMIS2[,i] <- recode(BMIS2[,i],"4=1;3=2;2=3;1=4")
# }
# 
# BMIS$AC <- rowSums(BMIS2[,c(1,3:5,7,8,11:16)])
# 
# ## Positive-Tired
# BMIS3 <- BMIS
# library(car)
# for (i in c(4,9)){
#   BMIS3[,i] <- recode(BMIS3[,i],"4=1;3=2;2=3;1=4")
# }
# 
# BMIS$PT <- rowSums(BMIS3[,c(1,4,5,9,11,14,16)])
# 
# ## Negative-Relaxed
# BMIS4 <- BMIS
# library(car)
# for (i in c(13)){
#   BMIS4[,i] <- recode(BMIS4[,i],"4=1;3=2;2=3;1=4")
# }
# 
# BMIS$NR <- rowSums(BMIS4[,c(3,7,8,12,13,15)])


####################### Correlation Plots for Trait Measures #######################

S1traitall <- merge.data.frame(S1traits,dat1.l,by="id") 

## Trait Trust Measures
cor.plot(cor(S1traitall[,c(229,56,213,214,215)])) ## problems with this...graphics issue with the plotting. might be my RStudio
round(cor(S1traitall[,c(229,56,213,214,215)],use="pairwise.complete.obs"),2)
#          T   GSS   GTS   WVS  RITS
# T     1.00  0.04  0.04 -0.05  0.04
# GSS   0.04  1.00  0.81 -0.62  0.13
# GTS   0.04  0.81  1.00 -0.63  0.04
# WVS  -0.05 -0.62 -0.63  1.00 -0.16
# RITS  0.04  0.13  0.04 -0.16  1.00

## Trait Discriminant Measures
cor.plot(cor(S1traitall[,c(229,227,228,223,216)])) ## problems with this...graphics issue with the plotting. might be my RStudio
round(cor(S1traitall[,c(229,227,228,223,216)],use="pairwise.complete.obs"),2)
#          T   BIS   BAS  NGSE   ERI
# T     1.00 -0.04  0.02  0.05  0.00
# BIS  -0.04  1.00 -0.10 -0.28 -0.05
# BAS   0.02 -0.10  1.00  0.49  0.14
# NGSE  0.05 -0.28  0.49  1.00  0.13
# ERI   0.00 -0.05  0.14  0.13  1.00

## Full Correlation Table - ETI, Trait Trust, Discriminant Measures
cor.plot(cor(S1traitall[,c(229,230,231,232,56,213,214,215,227,228,223,216)])) ## problems with this...graphics issue with the plotting. might be my RStudio
round(cor(S1traitall[,c(229,230,231,232,56,213,214,215,227,228,223,216)],use="pairwise.complete.obs"),2)
#          T     G     U     R   GSS   GTS   WVS  RITS   BIS   BAS  NGSE   ERI
# T     1.00  0.25 -0.49  0.39  0.04  0.04 -0.05  0.04 -0.04  0.02  0.05  0.00
# G     0.25  1.00 -0.07  0.37  0.04  0.07 -0.10 -0.01  0.04  0.05  0.11  0.02
# U    -0.49 -0.07  1.00 -0.07 -0.10 -0.11  0.06  0.01  0.07  0.09 -0.03  0.02
# R     0.39  0.37 -0.07  1.00  0.06  0.04 -0.08  0.08  0.07  0.04  0.09  0.16
# GSS   0.04  0.04 -0.10  0.06  1.00  0.81 -0.62  0.13 -0.18 -0.01  0.20  0.27
# GTS   0.04  0.07 -0.11  0.04  0.81  1.00 -0.63  0.04 -0.17  0.10  0.33  0.24
# WVS  -0.05 -0.10  0.06 -0.08 -0.62 -0.63  1.00 -0.16  0.10 -0.12 -0.34 -0.19
# RITS  0.04 -0.01  0.01  0.08  0.13  0.04 -0.16  1.00 -0.10 -0.07  0.07  0.05
# BIS  -0.04  0.04  0.07  0.07 -0.18 -0.17  0.10 -0.10  1.00 -0.10 -0.28 -0.05
# BAS   0.02  0.05  0.09  0.04 -0.01  0.10 -0.12 -0.07 -0.10  1.00  0.49  0.14
# NGSE  0.05  0.11 -0.03  0.09  0.20  0.33 -0.34  0.07 -0.28  0.49  1.00  0.13
# ERI   0.00  0.02  0.02  0.16  0.27  0.24 -0.19  0.05 -0.05  0.14  0.13  1.00

####################### Trait Measure Analyses ################################
## Study 1

## Study 3

## Study 5
## Need to fix this code according to the other data set - TODAY 3/2
lmCS <- lm(T.X~GTS+WVS+as.factor(female)+followup,data=S5all)
summary(lmCS)
summary(lmCS)[complete.cases(S5all[,9:14]),9:14],2)  ## Why does this not work?  The parentheses aren't matched. Where should it go?

# Call:
# lm(formula = T.X ~ GTS + WVS + as.factor(female) + followup, 
#     data = S5all)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -2.28072 -0.56012  0.03736  0.55947  2.60590 
# 
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          4.6341     0.5106   9.077 1.25e-15 ***
# GTS                  0.2602     0.1384   1.881  0.06217 .  
# WVS                  0.6380     0.2005   3.183  0.00181 ** 
# as.factor(female)1  -0.3619     0.1999  -1.810  0.07253 .  
# followup            -0.1713     0.1308  -1.309  0.19267    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8683 on 134 degrees of freedom
#   (82 observations deleted due to missingness)
# Multiple R-squared:  0.1826,	Adjusted R-squared:  0.1582 
# F-statistic: 7.482 on 4 and 134 DF,  p-value: 1.802e-05

# S5all$WVS <- rowMeans(S5all[,9:14]) ## due to high alpha, kept same scoring as GTS by average across all 6 items
# psych::alpha(S5all[,9:14])
# dat5.w <- reshape(dat5.l,v.names=names(dat5.l)[3:9],timevar="scen",idvar="id",direction="wide")
# #str(dat5.w)

lmCS2 <- lm(T.SD^2~GTS+WVS+as.factor(female)+followup,data=S5all) ## SD NOT variance

# summary(lmCS2)
# Call:
# lm(formula = T.SD^2 ~ GTS + WVS + as.factor(female) + followup, 
#     data = S5all)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -5.1875 -2.5386  0.1294  1.6623 10.4162 
# 
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         10.2637     1.7781   5.772 5.18e-08 ***
# GTS                  0.2614     0.4818   0.543   0.5883    
# WVS                 -1.4479     0.6982  -2.074   0.0400 *  
# as.factor(female)1   0.2141     0.6963   0.308   0.7589    
# followup            -0.9280     0.4555  -2.037   0.0436 *  
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.024 on 134 degrees of freedom
#   (82 observations deleted due to missingness)
# Multiple R-squared:  0.06367,	Adjusted R-squared:  0.03572 
# F-statistic: 2.278 on 4 and 134 DF,  p-value: 0.06415

## With the combined data set we have a shift from just the Sona data.  We're now explaining even less 
## variance (3% vs 6%). The female predictor is still non-sig and WVS is still negative. 
## WTF?  Think on it...

## Checked out a couple other variables.  What else showed significance?
## Age (very small estimate but 10% variance explained); Black Ethnicity; Long-term, committed romantic relationship; 
## Jobs (unemployed, PT, Retired); and Mood all were significant.
## All predicted trust in a positive direction.
## Interesting...think about it and talk with Pat and Lisa.

lmCS3 <- lm(T.SD^2~GTS+WVS+mood,data=S5all) ## SD NOT variance
summary(lmCS3)



### This section is partly redundant. Need to clean up what's not needed. ###

################################ Scale Validity/Reliability Analyses #########################################
##### General trust survey (Trait) #####
ctt.GTS <- psych::alpha(GTS[,4:9])
ctt.GTS

# Reliability analysis   
# Call: psych::alpha(x = GTS[, 4:9])
# 
# raw_alpha std.alpha G6(smc) average_r S/N    ase mean  sd
#      0.86      0.87    0.86      0.52 6.5 0.0025  3.5 0.7
# 
# lower alpha upper     95% confidence boundaries
# 0.86 0.86 0.87 
# 
# Reliability if an item is dropped:
#       raw_alpha std.alpha G6(smc) average_r S/N alpha se
# GTS.1      0.82      0.82    0.81      0.49 4.7   0.0034
# GTS.2      0.81      0.82    0.80      0.48 4.7   0.0035
# GTS.3      0.82      0.83    0.82      0.49 4.9   0.0033
# GTS.4      0.83      0.84    0.83      0.51 5.3   0.0031
# GTS.5      0.88      0.88    0.87      0.59 7.2   0.0022
# GTS.6      0.86      0.86    0.86      0.56 6.4   0.0028
# 
# Item statistics 
#          n raw.r std.r r.cor r.drop mean   sd
# GTS.1 5920  0.85  0.85  0.85   0.77  3.3 0.88
# GTS.2 5920  0.86  0.86  0.86   0.78  3.2 0.92
# GTS.3 5920  0.82  0.83  0.80   0.74  3.4 0.84
# GTS.4 5920  0.79  0.79  0.75   0.68  3.2 0.88
# GTS.5 5920  0.65  0.62  0.49   0.46  3.8 1.07
# GTS.6 5920  0.68  0.69  0.58   0.55  3.9 0.83
# 
# Non missing response frequency for each item
#          1    2    3    4    5 miss
# GTS.1 0.02 0.19 0.32 0.44 0.04 0.19
# GTS.2 0.03 0.21 0.32 0.40 0.04 0.19
# GTS.3 0.01 0.16 0.31 0.48 0.04 0.19
# GTS.4 0.02 0.23 0.34 0.39 0.03 0.19
# GTS.5 0.04 0.11 0.15 0.44 0.26 0.19
# GTS.6 0.02 0.05 0.16 0.58 0.20 0.19

lm.gts <- lm(GTStotal~T*G*R*U1,data=S5all)
summary(lm.gts)

##### General social survey (Trait) #####
lm.gss <- lm(GSS~GTStotal,data=S5all)
summary(lm.gss)
lm.gss1 <- lm(GSS~GTStotal+T+G*R*U1,data=S5all)
summary(lm.gss1)

##### World Values survey (Trait) #####
ctt.WVS <- psych::alpha(WVS[,4:9])
ctt.WVS

# Reliability analysis   
# Call: psych::alpha(x = WVS[, 4:9])
# 
# raw_alpha std.alpha G6(smc) average_r S/N    ase mean   sd
#      0.81      0.81    0.83      0.41 4.2 0.0032  2.7 0.49
# 
# lower alpha upper     95% confidence boundaries
#  0.81 0.81 0.82 
# 
# Reliability if an item is dropped:
#       raw_alpha std.alpha G6(smc) average_r S/N alpha se
# WVS.1      0.83      0.84    0.84      0.50 5.1   0.0031
# WVS.2      0.77      0.76    0.79      0.39 3.3   0.0040
# WVS.3      0.79      0.77    0.81      0.41 3.4   0.0037
# WVS.4      0.79      0.79    0.81      0.42 3.7   0.0037
# WVS.5      0.76      0.76    0.73      0.38 3.1   0.0043
# WVS.6      0.75      0.74    0.73      0.37 2.9   0.0044
# 
# Item statistics 
#          n raw.r std.r r.cor r.drop mean   sd
# WVS.1 5920  0.45  0.50  0.34   0.29  3.7 0.52
# WVS.2 5920  0.78  0.76  0.69   0.63  2.3 0.77
# WVS.3 5920  0.70  0.73  0.63   0.58  3.1 0.55
# WVS.4 5920  0.71  0.69  0.59   0.55  1.8 0.75
# WVS.5 5920  0.81  0.79  0.81   0.69  2.5 0.74
# WVS.6 5920  0.83  0.82  0.85   0.73  2.6 0.70
# 
# Non missing response frequency for each item
#          1    2    3    4 miss
# WVS.1 0.00 0.02 0.31 0.67 0.19
# WVS.2 0.13 0.48 0.33 0.06 0.19
# WVS.3 0.00 0.08 0.69 0.23 0.19
# WVS.4 0.35 0.52 0.10 0.04 0.19
# WVS.5 0.06 0.42 0.43 0.09 0.19
# WVS.6 0.04 0.43 0.45 0.08 0.19

lm.wvs <- lm(WVStotal~GSS,dat=S5all)
summary(lm.wvs)
lm.wvs1 <- lm(WVStotal~GTStotal,dat=S5all)
summary(lm.wvs1)
lm.wvs2 <- lm(WVStotal~GTStotal+GSS,dat=S5all)
summary(lm.wvs2)
lm.wvs3 <- lm(WVStotal~GTStotal+GSS+T+G*R*U1,dat=S5all)
summary(lm.wvs3)

#### Visual Analog Mood & Energy Scales #####
dat2$MOOD <- MOOD$MOOD
dat2$ENERGY <- ENERGY$ENERGY

## Correlations of our variables and the trait totals

## Study 3
S3all.l <- merge(dat3all.l,S3traitsAll,by="id")
str(S3all.l)
names(S3all.l)

cor.plot(cor(S3all.l[,c(3:9,11,98,97,44,45)])) ## problems with this...graphics issue with the plotting. might be my RStudio
round(cor(S3all.l[,c(3:9,11,98,97,44,45)],use="pairwise.complete.obs"),2)
#            G    U1   U2     R     T     B   U3  GSS  GTS  WVS  mood energy
# G       1.00  0.14 0.32  0.06 -0.11 -0.17 0.01 0.02 0.02 0.01 -0.02  -0.02
# U1      0.14  1.00 0.07 -0.05  0.13  0.01 0.56 0.03 0.02 0.01  0.01   0.00
# U2      0.32  0.07 1.00  0.48  0.46  0.39 0.15 0.01 0.01 0.01  0.01   0.01
# R       0.06 -0.05 0.48  1.00  0.67  0.47 0.08 0.03 0.01 0.02  0.00  -0.01
# T      -0.11  0.13 0.46  0.67  1.00  0.51 0.29 0.06 0.07 0.04  0.04   0.03
# B      -0.17  0.01 0.39  0.47  0.51  1.00 0.17 0.01 0.01 0.05  0.04   0.04
# U3      0.01  0.56 0.15  0.08  0.29  0.17 1.00 0.02 0.02 0.00  0.03   0.01
# GSS     0.02  0.03 0.01  0.03  0.06  0.01 0.02 1.00 0.72 0.47  0.34   0.18
# GTS     0.02  0.02 0.01  0.01  0.07  0.01 0.02 0.72 1.00 0.51  0.42   0.30
# WVS     0.01  0.01 0.01  0.02  0.04  0.05 0.00 0.47 0.51 1.00  0.22   0.16
# mood   -0.02  0.01 0.01  0.00  0.04  0.04 0.03 0.34 0.42 0.22  1.00   0.70
# energy -0.02  0.00 0.01 -0.01  0.03  0.04 0.01 0.18 0.30 0.16  0.70   1.00

## Study 5
S5all.l <- merge(dat5all.l,S5traitsAll,by="id")
str(S5all.l)
names(S5all.l)

cor.plot(cor(S5all.l[,c(3:9,11,37,38,24,25)])) ## problems with this...graphics issue with the plotting. might be my RStudio
round(cor(S5all.l[,c(3:9,11,37,38,24,25)],use="pairwise.complete.obs"),2)

#             G    U1    U2     R     T     B   U3   GSS GTStotal WVStotal  mood energy
# G        1.00  0.40  0.42  0.29  0.09  0.06 0.23  0.03     0.04     0.02  0.04   0.02
# U1       0.40  1.00  0.23  0.16  0.14 -0.04 0.40  0.05     0.05     0.05  0.06   0.07
# U2       0.42  0.23  1.00  0.58  0.49 -0.25 0.32  0.06     0.06     0.07  0.05   0.06
# R        0.29  0.16  0.58  1.00  0.69 -0.35 0.37  0.06     0.06     0.06  0.06   0.06
# T        0.09  0.14  0.49  0.69  1.00 -0.39 0.37  0.09     0.07     0.09  0.08   0.09
# B        0.06 -0.04 -0.25 -0.35 -0.39  1.00 0.00 -0.03    -0.02    -0.01 -0.01  -0.02
# U3       0.23  0.40  0.32  0.37  0.37  0.00 1.00  0.03     0.04     0.04  0.05   0.05
# GSS      0.03  0.05  0.06  0.06  0.09 -0.03 0.03  1.00     0.60     0.62  0.34   0.23
# GTStotal 0.04  0.05  0.06  0.06  0.07 -0.02 0.04  0.60     1.00     0.57  0.45   0.31
# WVStotal 0.02  0.05  0.07  0.06  0.09 -0.01 0.04  0.62     0.57     1.00  0.44   0.31
# mood     0.04  0.06  0.05  0.06  0.08 -0.01 0.05  0.34     0.45     0.44  1.00   0.61
# energy   0.02  0.07  0.06  0.06  0.09 -0.02 0.05  0.23     0.31     0.31  0.61   1.00

## Overall
## None of the trait measures correlate well with T or B (awww yeah!)
## The trait measures correlate well with each other though (great!)
## Mood and energy have little to no relationship with any state measure, a weak one with trait



################# MODEL TESTING ANALYSES #####################

# how about transforming the trust components
library(MASS)

newdat3 <- dat3all.l[,c("G","U1","R","U2","T","B","U3")]
newdat3 <- newdat3[complete.cases(newdat3),]

# getlambda <- function(var,dv){
#   if(min(var)==0){
#     var <- var+1
#   }
#   lam <- as.data.frame(boxcox(lm(var~dv),plotit=F))
#   lam <- lam[lam$y == max(lam$y),1]
#   var <- (var^lam - 1)/lam
#   return(var)
# }
# 
# newdat3$G.t <- getlambda(var=newdat3$G,dv=newdat3$T)
# 

outG <- boxcox(lm((G+1)~T,data=dat3all.l),plotit=F)
outG <- as.data.frame(outG)
Glambda <- outG[outG$y == max(outG$y),1]
newdat3$Gt <- (newdat3$G^Glambda - 1)/Glambda

outU1 <- boxcox(lm((U1+1)~T,data=dat3all.l),plotit=F)
outU1 <- as.data.frame(outU1)
U1lambda <- outU1[outU1$y == max(outU1$y),1]
newdat3$U1t <- (newdat3$U1^U1lambda - 1)/U1lambda

outR <- boxcox(lm((R+1)~T,data=dat3all.l),plotit=F)
outR <- as.data.frame(outR)
Rlambda <- outR[outR$y == max(outR$y),1]
newdat3$Rt <- (newdat3$R^Rlambda - 1)/Rlambda

summary(lm(T~G*U1*R,data=newdat3))
# Call:
#   lm(formula = T ~ G * U1 * R, data = newdat3)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.3982 -1.0346 -0.0375  0.8366  7.6300 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.009976   0.582831   0.017  0.98635    
# G            0.111556   0.074584   1.496  0.13500    
# U1           0.486538   0.079617   6.111 1.35e-09 ***
# R            0.997756   0.099982   9.979  < 2e-16 ***
# G:U1        -0.043794   0.009996  -4.381 1.29e-05 ***
# G:R         -0.035834   0.012978  -2.761  0.00585 ** 
# U1:R        -0.056928   0.014335  -3.971 7.58e-05 ***
# G:U1:R       0.007555   0.001793   4.214 2.70e-05 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.83 on 1164 degrees of freedom
# Multiple R-squared:  0.5758,	Adjusted R-squared:  0.5732 
# F-statistic: 225.7 on 7 and 1164 DF,  p-value: < 2.2e-16

summary(lm(T~Gt*U1t*Rt,data=newdat3))
# Call:
#   lm(formula = T ~ Gt * U1t * Rt, data = newdat3)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.3699 -1.0237 -0.0403  0.8247  7.6159 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.4413583  0.3706311   3.889 0.000106 ***
#   Gt           0.0216712  0.0236889   0.915 0.360473    
# U1t          0.1860418  0.0274638   6.774 1.98e-11 ***
#   Rt           0.9154265  0.0724050  12.643  < 2e-16 ***
#   Gt:U1t      -0.0076990  0.0017055  -4.514 7.00e-06 ***
#   Gt:Rt       -0.0119962  0.0047100  -2.547 0.010994 *  
#   U1t:Rt      -0.0233382  0.0056914  -4.101 4.41e-05 ***
#   Gt:U1t:Rt    0.0015339  0.0003518   4.360 1.42e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.829 on 1164 degrees of freedom
# Multiple R-squared:  0.5764,	Adjusted R-squared:  0.5739 
# F-statistic: 226.3 on 7 and 1164 DF,  p-value: < 2.2e-16


summary(lm(T~G:U1:R,data=newdat3))
# 
# Call:
#   lm(formula = T ~ G:U1:R, data = newdat3)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.8116 -1.7826  0.0728  1.7411  5.6103 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.3896769  0.1069845   41.03   <2e-16 ***
#   G:U1:R      0.0055274  0.0003069   18.01   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.479 on 1170 degrees of freedom
# Multiple R-squared:  0.2171,	Adjusted R-squared:  0.2164 
# F-statistic: 324.5 on 1 and 1170 DF,  p-value: < 2.2e-16

summary(lm(T~Gt:U1t:Rt,data=newdat3))
# Call:
#   lm(formula = T ~ Gt:U1t:Rt, data = newdat3)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.7630 -1.8349  0.0139  1.9017  5.3840 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.799e+00  9.344e-02   51.36   <2e-16 ***
#   Gt:U1t:Rt   1.496e-03  8.645e-05   17.31   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.5 on 1170 degrees of freedom
# Multiple R-squared:  0.2038,	Adjusted R-squared:  0.2032 
# F-statistic: 299.6 on 1 and 1170 DF,  p-value: < 2.2e-16



### see:  https://www.r-bloggers.com/on-box-cox-transform-in-regression-models/
##        https://www.r-bloggers.com/tukey-and-mostellers-bulging-rule-and-ladder-of-powers/

## cycle through exponents for X to determine the best fit

tmp <- dat3all.l
tmp$GUR <- dat3all.l$G*dat3all.l$U1*dat3all.l$R

outRsq <- data.frame(Exp=NA,AdjR2=NA)
for (i in seq(.1,5,by=.1)){
  outRsq <- rbind(outRsq,c(i,summary(lm(T~I(GUR^i),data=tmp))$adj.r.squared))
}
outRsq <- outRsq[-1,]
ggplot(outRsq,aes(y=AdjR2,x=Exp)) + geom_smooth()
outRsq[outRsq$AdjR2==max(outRsq$AdjR2),]

#    Exp     AdjR2
# 10 0.9 0.2166358

## G exp:  3
## U exp:  .4
## R exp: 1
## NO BENEFIT TO TRANSFORMING GUR as a composite

dat3tmp <- dat3all.l
dat3tmp$G3 <- dat3tmp$G^3
dat3tmp$U1.4 <- dat3tmp$U1^.4
ggplot(dat3tmp,aes(x=G3,y=T)) + geom_smooth(col="green") + ylim(0,10)
ggplot(dat3tmp,aes(x=U1,y=T)) + geom_smooth() + ylim(0,10)
ggplot(dat3tmp,aes(x=R,y=T)) + geom_smooth() + ylim(0,10)

dat3tmp$G3z <- scale(dat3tmp$G3)
dat3tmp$U1.4z <- scale(dat3tmp$U1.4)
dat3tmp$Rz <- scale(dat3tmp$R)


lm3fin <- lm(T~G3z:U1.4z:Rz,data=dat3tmp)
summary(lm3fin)
# Call:
#   lm(formula = T ~ G3z:U1.4z:Rz, data = dat3tmp)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.9574 -1.8041  0.1906  2.2070  5.3845 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   5.79011    0.08074  71.717  < 2e-16 ***
#   G3z:U1.4z:Rz  0.39057    0.06657   5.867 5.77e-09 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.762 on 1170 degrees of freedom
# (20 observations deleted due to missingness)
# Multiple R-squared:  0.02858,	Adjusted R-squared:  0.02775 
# F-statistic: 34.42 on 1 and 1170 DF,  p-value: 5.765e-09

dat3tmp$GUR <- dat3tmp$G*dat3tmp$U1*dat3tmp$R
lm3GUR <- lm(T~GUR,data=dat3tmp)
summary(lm3GUR)
# Call:
#   lm(formula = T ~ GUR, data = dat3tmp)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.8116 -1.7826  0.0728  1.7411  5.6103 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.3896769  0.1069845   41.03   <2e-16 ***
#   GUR         0.0055274  0.0003069   18.01   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.479 on 1170 degrees of freedom
# (20 observations deleted due to missingness)
# Multiple R-squared:  0.2171,	Adjusted R-squared:  0.2164 
# F-statistic: 324.5 on 1 and 1170 DF,  p-value: < 2.2e-16

pGUR <- ggplot(dat3tmp,aes(x=GUR,y=T)) + geom_smooth()
pGUR

round(cor(x=dat3tmp[,c("G","U1","R")],y=dat3tmp$GUR,use="pairwise.complete.obs"),2)
# G  0.54
# U1 0.46
# R  0.62

lm3all <- lm(T~G:U1:R,data=dat3all.l)
summary(lm3all)
# Call:
#   lm(formula = T ~ G:U1:R, data = dat3all.l)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.8116 -1.7826  0.0728  1.7411  5.6103 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.3896769  0.1069845   41.03   <2e-16 ***
#   G:U1:R      0.0055274  0.0003069   18.01   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.479 on 1170 degrees of freedom
# (20 observations deleted due to missingness)
# Multiple R-squared:  0.2171,	Adjusted R-squared:  0.2164 
# F-statistic: 324.5 on 1 and 1170 DF,  p-value: < 2.2e-16


library(lme4)
m.0 <- lmer(T~ 1 + (1|id), data=dat3all.l)
m.1 <- lmer(T~ G + (1|id), data=dat3all.l)
m.2 <- lmer(T~ G + (G|id), data=dat3all.l)
m.3 <- lmer(T~ G + U1 + (1|id), data=dat3all.l)
m.4 <- lmer(T~ G*U1 + (1|id), data=dat3all.l)
m.5 <- lmer(T~ G*U1 + (G|id), data=dat3all.l)
m.6 <- lmer(T~ G*U1 + (G|id) + (U1|id), data=dat3all.l)
m.7 <- lmer(T~ G*U1 + R + (1|id), data=dat3all.l)
m.8 <- lmer(T~ G*U1*R + (1|id), data=dat3all.l)
m.9 <- lmer(T~ G*U1*R + (G|id) + (U1|id) + (R|id), data=dat3all.l)
anova(m.0,m.1,m.2,m.3,m.4,m.5,m.6,m.7,m.8,m.9) ## m.7 winner winner chicken dinner

m.10 <- lmer(T~ G:U1:R + (1|id), data=dat3all.l)
anova(m.7,m.10) ## m.7 defends the crown
m.11 <- lmer(T~G:U1 + R + (1|id),data=dat3all.l)
m.12 <- lmer(T~G:U1 + R + (R|id),data=dat3all.l)
anova(m.7,m.11,m.12) ## m.7 reigning champ

m.13 <- lmer(T~G:U1 + R + (1|scen),data=dat3all.l)
anova(m.7,m.13) ## m.13 new winner
m.14 <- lmer(T~G:U1:R + (1|scen),data=dat3all.l)
m.15 <- lmer(T~G:U1 + R + (R|scen),data=dat3all.l)
m.16 <- lmer(T~G:U1 + R + (G:U1|scen),data=dat3all.l)
m.17 <- lmer(T~G:U1 + R + (R|scen) + (G:U1|scen),data=dat3all.l)
anova(m.13,m.15,m.16,m.17)  ## m.15 winner so far.  Any contenders?

anova(m.15,m.17)  ## technically, m.17 outperforms m.15

## one more...
m.18 <- lmer(T~G:U1 + R + (R|scen) + (G:U1|id),data=dat3all.l)
m.19 <- lmer(T~ G*U1*R + (G|scen) + (U1|scen) + (R|scen), data=dat3all.l)
anova(m.15,m.17,m.19)

## compare m.19 to the lm3
lm3.all <- lm(T~G*U1*R,data=dat3all.l)
anova(m.19,lm3.all)
# Data: dat3all.l
# Models:
#   lm3.all: T ~ G * U1 * R
# m.19: T ~ G * U1 * R + (G | scen) + (U1 | scen) + (R | scen)
#          Df    AIC    BIC  logLik deviance  Chisq Chi-Df Pr(>Chisq)    
# lm3.all  9  4752.4 4798.0 -2367.2   4734.4                             
# m.19    18  4409.5 4500.7 -2186.7   4373.5 360.91      9  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### WE HAVE A WINNER - m.19
summary(lm(m.19))

# Call:
#   lm(formula = m.19)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.8365 -1.0317 -0.0698  0.8201  7.5881 
# 
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -0.768597   0.579806  -1.326  0.18523    
#   G            0.139332   0.073044   1.908  0.05670 .  
#   U1           0.393870   0.078884   4.993 6.85e-07 ***
#   R            1.041226   0.097965  10.629  < 2e-16 ***
#   scen         0.189625   0.025831   7.341 3.97e-13 ***
#   G:U1        -0.039172   0.009796  -3.999 6.77e-05 ***
#   G:R         -0.039416   0.012702  -3.103  0.00196 ** 
#   U1:R        -0.056935   0.014020  -4.061 5.21e-05 ***
#   G:U1:R       0.007611   0.001753   4.341 1.54e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.79 on 1163 degrees of freedom
# (20 observations deleted due to missingness)
# Multiple R-squared:  0.5945,	Adjusted R-squared:  0.5918 
# F-statistic: 213.2 on 8 and 1163 DF,  p-value: < 2.2e-16

## the best fitting model to date is m.19.  We have an overall winner.

# So, what did we learn with these linear, mixed effects models?  We learned 
# that the random coefficients fit well and it is unlikely that the fixed effect
# model outperforms the random coefficients (see nested model comparison above 
# between m.19 and lm3.all).  Next, we learned that the facet of random 
# variation is the scenario and NOT the subject.  When we conditioned on 
# scenario, the model fit was much better.  Thus, we have a nice bit of evidence
# that the subjects all perform relatively the same (by coefficient) but the
# scenarios produce the variance in the parameters.  That finding, my friends,
# rocks.

summary(m.19)
# Linear mixed model fit by REML ['lmerMod']
# Formula: T ~ G * U1 * R + (G | scen) + (U1 | scen) + (R | scen)
# Data: dat3all.l
# 
# REML criterion at convergence: 4422.7
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.7978 -0.4775 -0.0039  0.5051  4.2762 
# 
# Random effects:
#   Groups   Name         Variance Std.Dev. Corr 
#   scen     (Intercept)  0.34290  0.5856        
#   G                     0.01316  0.1147   -1.00
#   scen.1   (Intercept)  1.89855  1.3779        
#   U1                    0.01253  0.1119   -1.00
#   scen.2   (Intercept)  4.42931  2.1046        
#   R                     0.05748  0.2398   -1.00
#   Residual              2.32746  1.5256        
#   Number of obs: 1172, groups:  scen, 8
# 
# Fixed effects:
#              Estimate  Std.Error t-value
# (Intercept)  1.348246   1.150333   1.172
# G            0.004296   0.106084   0.040
# U1           0.306569   0.100291   3.057
# R            0.831049   0.133115   6.243
# G:U1        -0.022406   0.011796  -1.899
# G:R         -0.020509   0.014244  -1.440
# U1:R        -0.032274   0.014429  -2.237
# G:U1:R       0.004330   0.001796   2.411
# 
# Correlation of Fixed Effects:
#            G      U1     R      G:U1   G:R    U1:R  
# G      -0.564                                          
# U1     -0.596  0.513                                   
# R      -0.762  0.447  0.423                            
# G:U1    0.439 -0.737 -0.787 -0.385                     
# G:R     0.406 -0.717 -0.413 -0.656  0.589              
# U1:R    0.370 -0.426 -0.744 -0.635  0.656  0.632       
# G:U1:R -0.347  0.590  0.634  0.563 -0.795 -0.823 -0.845

## Replication attempt to the linear model
m.19.R <- lmer(T~ G*U1*R + (G|scen) + (U1|scen) + (R|scen), data=dat4.l)
summary(lm(m.19.R))

m19.R.2a <- lmer(T~ G*U1*R + (G|scen) + (U1|scen) + (R|scen), data=subset(dat5.l,dat5.l$scen < 9))
summary(lm(m19.R.2a))

m19.R.2b <- lmer(T~ G*U1*R + (G|scen) + (U1|scen) + (R|scen), data=subset(dat5.l,dat5.l$scen > 8))
summary(lm(m19.R.2b))

## Test again with data set 5
m.19.Conf <- lmer(T~ G*U1*R + (G|scen) + (U1|scen) + (R|scen), data=dat5.l)
anova(m.19.Conf)
# Warning messages:
#   1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                     unable to evaluate scaled gradient
#   2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                     Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
#   > anova(m.19.Conf)
#        Analysis of Variance Table
#         Df Sum Sq Mean Sq  F value
#  G       1  22.58   22.58   9.4550
#  U1      1  75.46   75.46  31.5915
#  R       1 463.13  463.13 193.9000
#  G:U1    1  17.37   17.37   7.2742
#  G:R     1  58.77   58.77  24.6055
#  U1:R    1   5.52    5.52   2.3128
#  G:U1:R  1  26.66   26.66  11.1613

## compare m.19 to the lm3
lm5.all <- lm(T~G*U1*R,data=dat5.l)
anova(m.19.Conf,lm5.all)
# Data: dat5.l
# Models:
#   lm5.all: T ~ G * U1 * R
# m.19.Conf: T ~ G * U1 * R + (G | scen) + (U1 | scen) + (R | scen)
#           Df   AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# lm5.all    9 13157 13212 -6569.4    13139                             
# m.19.Conf 18 12107 12216 -6035.5    12071 1067.8      9  < 2.2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(lm(m.19.Conf))
summary(m.19.Conf)
## Model failed to converge
##
# Call:
#   lm(formula = m.19.Conf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.8250 -0.9719  0.0294  1.0027  8.2318 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.632673   0.450440   1.405  0.16025    
# G            0.141941   0.055516   2.557  0.01061 *  
# U1           0.401921   0.059141   6.796 1.28e-11 ***
# R            0.797777   0.069852  11.421  < 2e-16 ***
# scen         0.009772   0.007529   1.298  0.19438    
# G:U1        -0.042962   0.006968  -6.166 7.90e-10 ***
# G:R         -0.013739   0.008488  -1.619  0.10561    
# U1:R        -0.023318   0.009548  -2.442  0.01465 *  
# G:U1:R       0.003027   0.001071   2.827  0.00473 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.863 on 3211 degrees of freedom
# (92 observations deleted due to missingness)
# Multiple R-squared:  0.5293,	Adjusted R-squared:  0.5281 
# F-statistic: 451.3 on 8 and 3211 DF,  p-value: < 2.2e-16
# 
# > summary(m.19.Conf)
# Linear mixed model fit by REML ['lmerMod']
# Formula: T ~ G * U1 * R + (G | scen) + (U1 | scen) + (R | scen)
# Data: dat5.l
# 
# REML criterion at convergence: 12127.2
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.8463 -0.5136 -0.0060  0.4818  4.6495 
# 
# Random effects:
# Groups         Name  Variance Std.Dev. Corr 
# scen     (Intercept) 0.038403 0.19597       
# G                    0.008660 0.09306  0.72 
# scen.1   (Intercept) 0.668605 0.81768       
# U1                   0.006361 0.07976  -0.99
# scen.2   (Intercept) 1.539657 1.24083       
# R                    0.038087 0.19516  -1.00
# Residual             2.388520 1.54548       
# Number of obs: 3220, groups:  scen, 16
# 
# Fixed effects:
#              Estimate Std. Error t value
# (Intercept)  1.648340   0.591790   2.785
# G            0.024267   0.066737   0.364
# U1           0.350244   0.066586   5.260
# R            0.654825   0.084554   7.744
# G:U1        -0.036284   0.007603  -4.772
# G:R         -0.004442   0.008899  -0.499
# U1:R        -0.019881   0.009464  -2.101
# G:U1:R       0.003567   0.001068   3.341
# 
# Correlation of Fixed Effects:
#   (Intr) G      U1     R      G:U1   G:R    U1:R  
# G      -0.577                                          
# U1     -0.699  0.536                                   
# R      -0.814  0.493  0.497                            
# G:U1    0.571 -0.787 -0.803 -0.452                     
# G:R     0.530 -0.770 -0.456 -0.683  0.659              
# U1:R    0.512 -0.442 -0.798 -0.661  0.676  0.609       
# G:U1:R -0.478  0.648  0.689  0.609 -0.829 -0.829 -0.861
# convergence code: 0
# unable to evaluate scaled gradient
# Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

> summary(lm5.all)
# 
# Call:
#   lm(formula = T ~ G * U1 * R, data = dat5.l)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.8218 -0.9802  0.0218  1.0051  8.2315 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.786997   0.434512   1.811  0.07020 .  
# G            0.132094   0.055001   2.402  0.01638 *  
# U1           0.396857   0.059018   6.724 2.08e-11 ***
# R            0.788269   0.069474  11.346  < 2e-16 ***
# G:U1        -0.042277   0.006949  -6.084 1.31e-09 ***
# G:R         -0.013094   0.008474  -1.545  0.12241    
# U1:R        -0.022498   0.009528  -2.361  0.01827 *  
# G:U1:R       0.002981   0.001070   2.785  0.00539 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.864 on 3212 degrees of freedom
# (92 observations deleted due to missingness)
# Multiple R-squared:  0.5291,	Adjusted R-squared:  0.528 
# F-statistic: 515.5 on 7 and 3212 DF,  p-value: < 2.2e-16

> summary(lm(T~G:U1:R,data=dat5.l))
# 
# Call:
#   lm(formula = T ~ G:U1:R, data = dat5.l)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.9541 -1.5397  0.2396  1.5443  5.8064 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 5.1539690  0.0707546   72.84   <2e-16 ***
#   G:U1:R      0.0036064  0.0001202   30.01   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.398 on 3218 degrees of freedom
# (92 observations deleted due to missingness)
# Multiple R-squared:  0.2187,	Adjusted R-squared:  0.2185 
# F-statistic: 900.8 on 1 and 3218 DF,  p-value: < 2.2e-16


############### A Bayesian Model Perhaps? #################

## Consider the situation of trust to be a set of conditions that function as
## thresholds.  We each have our own threshold for the three components.  

## consider ablearn package because it uses the same bits that the abn package uses
## NOTE:  the abn package does not run on Mac - Cairo refuses to load

### see http://www.r-bayesian-networks.org
install.packages("abn")
library(abn)

mydat <- dat5.l[,3:9]
for (i in c(1:5,7)){
  mydat[,i] <- mydat[,i]/10
}
mydists <- list(G="gaussian",U1="gaussian",U2="gaussian",R="gaussian",T="gaussian",B="binomial",U3="gaussian")
                     # G,U1,U2,R,T,B,U3
mydag <- matrix(data=c( 0,0,0,0,1,1,0, # G - row is predictor, the "1" goes in the DV col
                        0,0,0,0,1,1,0, # U1
                        0,0,0,0,1,0,0, # U2
                        0,0,0,0,1,1,0, # R
                        0,0,0,0,0,1,0, # T
                        0,0,0,0,0,0,1, # B
                        0,0,0,0,0,0,0), # U3
                        byrow=T,ncol=7)
colnames(mydag) <- rownames(mydag) <- names(mydat)
mydag
mydat$B <- as.factor(mydat$B)

myres.c <- fitabn(dag.m=mydag, data.df=mydat[complete.cases(mydat),], data.dists=mydists)
str(myres.c)
summary(myres.c)
plot(myres.c$graph)
tographviz(dag.m=mydag,data.df=mydat,data.dists=mydists,outfile="graph.dot",directed=TRUE);
system("dot -Tpdf -o graph.pdf graph.dot")




devtools::install_github('paulgovan/BayesianNetwork')
library(BayesianNetwork)
if (interactive()) {
  BayesianNetwork::BayesianNetwork()
}

library(rstan)

################### CREATE ONE MASSIVE DATASET ########################

## select only those datasets with U was broken apart
d3 <- dat3all.l
d3$study <- 3
d4 <- dat4.l
d4$study <- 4
d5 <- dat5all.l
d5$study <- 5

ATD <- rbind(d3,d4,d5)
ATD$study <- as.factor(ATD$study)
ATD <- ATD[complete.cases(ATD),]

#write.csv(ATD,file="Data345.csv",row.names = F) ## do this once to just store it for future use
#ATD <- read.csv("./Data345.csv",T)

library(lme4)
A0 <- lmer(T~1 + (1|id),data=ATD)
A1 <- lmer(T~1 + (1|scen),data=ATD)
anova(A0,A1)
A01 <- lmer(T~1 + (1|id) + (1|scen),data=ATD)
anova(A0,A1,A01)
summary(A01)
A01a <- lmer(T~1 + (1|id:scen),data=ATD)
summary(A01a)
A012 <- lmer(T~1 + (1|id) + (1|scen) + (1|study),data=ATD)
anova(A012,A01a)
anova(A0,A1,A01,A012)
summary(A01)
A012G <- lmer(T~G + (1|id) + (1|scen) + (1|study),data=ATD)
anova(A012,A012G)
A012GU <- lmer(T~G + U1 + (1|id) + (1|scen) + (1|study),data=ATD)
anova(A012G,A012GU)
A012GUi <- lmer(T~G:U1 + (1|id) + (1|scen) + (1|study),data=ATD)
anova(A012GU,A012GUi)
A012GUr <- lmer(T~G + U1 + (U1|id) + (1|scen) + (1|study),data=ATD)
anova(A012GU,A012GUr)
A012GUR <- lmer(T~G + U1 + R + (1|id) + (1|scen) + (1|study),data=ATD)
anova(A012GU,A012GUR)
A012GURr <- lmer(T~G + U1 + R + (1|id) + (R|scen) + (1|study),data=ATD)
anova(A012GUR,A012GURr)
A012GURri <- lmer(T~G * U1 * R + (1|id) + (R|scen) + (1|study),data=ATD)
anova(A012GURr,A012GURri)

summary(A012GURri)

## use on null models only
### cheat sheet:
## x:  an lmer object
## facet:  the level of generalization you wish to assess (ordered in the lmer object)

lmerICCest <- function(x,facet=NULL){
  tmp <- as.data.frame(VarCorr(x))[,c("grp","vcov")]
  out <- round(tmp$vcov[!is.na(match(tmp$grp,facet))]/sum(tmp$vcov),2)
  return(out)
}

lmerICCest(A012,"id")
lmerICCest(A012,"scen")
lmerICCest(A012,"study")
as.data.frame(VarCorr(A012))

# > lmerICCest(A012,"id")
# [1] 0.04
# > lmerICCest(A012,"scen")
# [1] 0.27
# > lmerICCest(A012,"study")
# [1] 0.05

########### ALL Traits ###################

ATDvigSums <- data.frame(id=unique(ATD$id))
for (i in 3:9){
  tmp <- merge(aggregate(ATD[,i],by=list(ATD$id),mean),aggregate(ATD[,i],by=list(ATD$id),sd),by="Group.1")
  names(tmp) <- c("id",paste(names(ATD)[i],"X",sep="."),paste(names(ATD)[i],"SD",sep="."))
  ATDvigSums <- merge(ATDvigSums,tmp,by="id")
}
str(ATDvigSums)
ATDall <- merge(ATD,ATDvigSums,by="id")
str(ATDall)
names(ATDall)

S3traitsAlltrunc <- S3traitsAll[,c(1:14,35,36,78:87)]
S5traitsAlltrunc <- S5traitsAll[,c(1:26)]
Alltraits <- rbind(S3traitsAlltrunc,S5traitsAlltrunc)


################################ General Trust Survey ##################################
## see:  http://fetzer.org/sites/default/files/images/stories/pdf/selfmeasures/Self_Measures_for_Love_and_Compassion_Research_TRUST.pdf
round(cor(Alltraits[,3:8],use="pairwise.complete.obs"),2)
factanal(Alltraits[complete.cases(Alltraits[,3:8]),3:8],2) ## two factors seem to fit best
Alltraits$GTS <- rowMeans(Alltraits[,3:8]) ## scoring routine - average across all 6 items
library(psych)
psych::alpha(Alltraits[,3:8])

##################### World Values Survey - Trust Questions ############################
round(cor(Alltraits[,9:14],use="pairwise.complete.obs"),2)
factanal(Alltraits[complete.cases(Alltraits[,9:14]),9:14],2) ## two factors seem to fit best
Alltraits$WVS <- rowMeans(Alltraits[,9:14]) ## due to high alpha, kept same scoring as GTS by average across all 6 items
psych::alpha(Alltraits[,9:14])

####################### Trait Measure Analyses ################################

lmCS <- lm(T.X~GTS+WVS+as.factor(female)+followup,data=S5all)
summary(lmCS)
summary(lmCS)[complete.cases(S5all[,9:14]),9:14],2)  ## Why does this not work?  The parentheses aren't matched. Where should it go?

lmCS2 <- lm(T.SD^2~GTS+WVS+as.factor(female)+followup,data=S5all) ## SD NOT variance


lmCS3 <- lm(T.SD^2~GTS+WVS+mood,data=S5all) ## SD NOT variance
summary(lmCS3)


################################ Scale Validity/Reliability Analyses #########################################
##### General trust survey (Trait) #####
ctt.GTS <- psych::alpha(GTS[,4:9])
ctt.GTS

lm.gts <- lm(GTStotal~T*G*R*U1,data=S5all)
summary(lm.gts)

##### General social survey (Trait) #####
lm.gss <- lm(GSS~GTStotal,data=S5all)
summary(lm.gss)
lm.gss1 <- lm(GSS~GTStotal+T+G*R*U1,data=S5all)
summary(lm.gss1)

##### World Values survey (Trait) #####
ctt.WVS <- psych::alpha(WVS[,4:9])
ctt.WVS

lm.wvs <- lm(WVStotal~GSS,dat=S5all)
summary(lm.wvs)
lm.wvs1 <- lm(WVStotal~GTStotal,dat=S5all)
summary(lm.wvs1)
lm.wvs2 <- lm(WVStotal~GTStotal+GSS,dat=S5all)
summary(lm.wvs2)
lm.wvs3 <- lm(WVStotal~GTStotal+GSS+T+G*R*U1,dat=S5all)
summary(lm.wvs3)

#### Visual Analog Mood & Energy Scales #####
dat2$MOOD <- MOOD$MOOD
dat2$ENERGY <- ENERGY$ENERGY

## Correlations of our variables and the trait totals

## Study 3
S3all.l <- merge(dat3all.l,S3traitsAll,by="id")
str(S3all.l)
names(S3all.l)

cor.plot(cor(S3all.l[,c(3:9,11,98,97,44,45)])) ## problems with this...graphics issue with the plotting. might be my RStudio
round(cor(S3all.l[,c(3:9,11,98,97,44,45)],use="pairwise.complete.obs"),2)









##################################################################################################
######################################## OLD CODE ################################################
##################################################################################################

# ####################### Visual Analog Mood & Energy Scales #############################
# MOOD <- data.frame(S5all[,c(1:2,10,24)])
# ENERGY <- data.frame(S5all[,c(1:2,10,25)])
# 
# ################################ Demographics #########################################
# DEM <- data.frame(S5all[,c(1:2,10,26:36)])