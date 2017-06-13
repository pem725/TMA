###############################################################################
##  Title:  Trust Measure Development Analysis
##  Author:  Patrick E. McKnight (pmcknigh@gmu.edu)
##           Simone Erchov (sfranz1@gmu.edu)
##           Lisa Alexander (lalexan8@gmu.edu)
##  Created:  11/20/2016
##  Last Edited By:  Patrick
##  Lasted Edited Date:  3/31/2017
##  IMPORTANT CHANGE:  ON 3/27/2017, I moved to github to track changes
##  Last Edit NOTES:  added emergent and latent prediction models via PCA and EFA
##  Previous Edit NOTES: created lavaan models and wide data
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


## x <- "./Data/S5 Vignettes_Round_5__SONA.csv" ## for testing purposes only - UNCOMMENT if needed
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


#### ICC estimation function for lmer ####
## cheat sheet:
## x:  lmer object for null model only
## facet:  the text (in quotes) name of the variable you wish to assess
lmerICCest <- function(x,facet=NULL){
  tmp <- as.data.frame(VarCorr(x))[,c("grp","vcov")]
  out <- round(tmp$vcov[!is.na(match(tmp$grp,facet))]/sum(tmp$vcov),2)
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

########### Data Recodeing for Study 1 --------------
dat1.l <- subONE(dat1.l,2:5)
dat1.lUr <- Ufold(dat1.l,5)

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

########### Data Recoding for Study 2 --------------
dat2.lUr <- Ufold(dat2.l,5)

### Study 3 Data:  9 of 27 Vignettes Presented with U broke into 3 - data from 3 sources #####

############ :SONA data ########
dat3s <- read.csv("./Data/S3 Vignettes_Round_3_SONA_10302016.csv",header=T) ## SONA
dat3sTIME <- as.numeric((strptime(as.character(dat3s$V9),"%m/%d/%Y %H:%M") - strptime(as.character(dat3s$V8),"%m/%d/%Y %H:%M"))/60)
#table(dat3sTIME > 10)
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
dat3.l <- rbind(dat3s.l,dat3m.l,dat3r.l,dat3f.l)
dat3.l$source <- as.factor(dat3.l$source) # refactor the source for later

########## Data Recoding for Study 3 ---------------
dat3.l <- subONE(dat3.l,c(3:7,9)) # subtract one from qualtrics data
dat3.l <- QualBeh(dat3.l,8) # create binary behavior scale
dat3.lUr <- Ufold(dat3.l,c(4,5,9)) # fold uncertainty scale

### Study 4 Data:  8 Vignettes Presented with U broke into 3 - data from SONA #####
dat4.l <- TrustDataFcn("./Data/S4 Vignettes_Round_4__SONA__NewVigOnly.csv",5,"SONA")

########## Data Recoding for Study 4 ---------------
dat4.l <- subONE(dat4.l,c(3:7,9)) # subtract one from qualtrics data
dat4.l <- QualBeh(dat4.l,8) # create binary behavior scale
dat4.lUr <- Ufold(dat4.l,c(4,5,9)) # fold uncertainty scale

### Study 5 Data:  Simone's dissertation data

######### :SONA data ############## 
dat5s.l <- TrustDataFcn2("./Data/S5 Vignettes_Round_5__SONA.csv",10,"SONA")

######### :mTurk data ###############
dat5m.l <- TrustDataFcn4("./Data/S5 Vignettes_Round_5__MTurk.csv",10,"mTurk")

############ :Reddit data ##################
#dat5r.l <- TrustDataFcn2("./Data/S5 Vignettes_Round_5__Reddit.csv",10,"Reddit")

############# :Flyer data ###################
dat5f.l <- TrustDataFcn3("./Data/S5 Vignettes_Round_5__Flyers.csv",20,"Flyers")

############# :Combine all Study 5 data files
dat5.l <- rbind(dat5s.l,dat5m.l,dat5f.l)
dat5.l$source <- as.factor(dat5.l$source) # refactor the source for later
#table(dat5.l$scen)

########## Data Recoding for Study 5 ---------------
dat5.l <- subONE(dat5all.l,c(3:7,9)) # subtract one from qualtrics data
dat5.l <- QualBeh(dat5.l,8) # create binary behavior scale
dat5.lUr <- Ufold(dat5.l,c(4,5,9)) # fold uncertainty scale

########################### LINEAR MODELS AND PLOTS ###################################

############# Study 1 ----------

## note - the change in columns
S1 <- ETM.Fcn(dat1.l,"S1out.pdf")
S1T <- ETM.Fcn(dat1.lUr,"S1outT.pdf")

############# Study 2 ----------

S2 <- ETM.Fcn(dat2.l,"S2out.pdf")
S2T <- ETM.Fcn(dat2.lUr,"S2outT.pdf")

############# Study 3 ----------
S3 <- ETM.Fcn(dat3.l,"S3out.pdf")
S3T <- ETM.Fcn(dat3.lUr,"S3outT.pdf")

############# Study 4 ----------
S4 <- ETM.Fcn(dat4.l,"S4out.pdf")
S4 <- ETM.Fcn(dat4.lUr,"S4outT.pdf")

############# Study 5 ----------
S5a <- ETM.Fcn(subset(dat5.l,dat5.l$scen < 9),"S5outA.pdf")
S5Ua <- ETM.Fcn(subset(dat5.lUr,dat5.lUr$scen < 9),"S5outTA.pdf")

S5b <- ETM.Fcn(subset(dat5.l,dat5.l$scen > 8),"S5outB.pdf")
S5Ub <- ETM.Fcn(subset(dat5.lUr,dat5.lUr$scen > 8),"S5outTB.pdf")

S5 <- ETM.Fcn(dat5.l,"S5out.pdf")
S5U <- ETM.Fcn(dat5.lUr,"S5outT.pdf")

####################### Latent Variable Models ################

library(lavaan)

### for the long data

LVmodel <- '
F =~ G + U1 + R
T ~ F
'
fit <- cfa(LVmodel,data=dat5.l,auto.fix.first=T)
summary(fit, fit.measures=TRUE)
parameterEstimates(fit)
standardizedSolution(fit)
fitMeasures(fit)
modificationIndices(fit,T,T)
## based upon the modification indices, we ought to run...
LVmodelMOD <- '
F =~ G + U1 + R + T
'
fit2 <- cfa(LVmodelMOD,data=dat5.l,auto.fix.first=T)
summary(fit2, fit.measures=TRUE)
parameterEstimates(fit2)
standardizedSolution(fit2)
fitMeasures(fit2)
modificationIndices(fit2,T,T)

### for the wide data
## Not a good fitting model.  

## (see below where I created the dat5.w for the trait analysis)


####################### Mediation Models ######################
MedModel <- '
T ~ c1*U1 + c2*G
R ~ a1*U1 + a2*G
T ~ b*R
inE := a1*b + a2*b
tot := c1 + c2 + (a1*b + a2*b)
'
fit3 <- sem(MedModel,data=dat5.l,se="bootstrap")
summary(fit3)

####################### MISC ######################################

### Study 1
## correlation matrix and plot
round(cor(dat1.l[,2:5]),2)
cor.plot(cor(dat1.l[,2:5]))

### Study 2
## correlation matrix and plot
round(cor(dat2.l[,4:8],use="pairwise.complete.obs"),2)
cor.plot(cor(dat2.l[,4:8],use="pairwise.complete.obs"))

### Study 3
round(cor(dat3.l[,3:9],use="pairwise.complete.obs"),2)
cor.plot(cor(dat3.l[,3:9],use="pairwise.complete.obs"))

### Study 4
round(cor(dat4.l[,3:9],use="pairwise.complete.obs"),2)
cor.plot(cor(dat4.l[,3:9],use="pairwise.complete.obs"))

### Study 5
round(cor(dat5.l[,3:9],use="pairwise.complete.obs"),2)
cor.plot(cor(dat5.l[,3:9],use="pairwise.complete.obs"))


################## TRAIT MEASURE #######################

S5traits <- TraitMeasFcn("./Data/S5 Vignettes_Round_5__SONA.csv",10,"SONA")
## 1. rescore the measures for traits
## 2. try to reduce the responses by id for G, U1, R, T, U2, U3, and B to averages and SD's (use aggregate)
## 3. merge the data frames
names(S5traits)

### GTS
## see:  http://fetzer.org/sites/default/files/images/stories/pdf/selfmeasures/Self_Measures_for_Love_and_Compassion_Research_TRUST.pdf
round(cor(S5traits[,3:8],use="pairwise.complete.obs"),2)
factanal(S5traits[complete.cases(S5traits[,3:8]),3:8],2) ## two factors seem to fit best
S5traits$GTS <- rowMeans(S5traits[,3:8]) ## scoring routine - average across all 6 items
library(psych)
psych::alpha(S5traits[,3:8]) ## note, need to use the psych package reference here


### WVS
round(cor(S5traits[,9:14],use="pairwise.complete.obs"),2)
factanal(S5traits[complete.cases(S5traits[,9:14]),9:14],2) ## two factors seem to fit best
S5traits$WVS <- rowMeans(S5traits[,9:14]) ## due to high alpha, kept same scoring as GTS by average across all 6 items
psych::alpha(S5traits[,9:14])
dat5.w <- reshape(dat5.l,v.names=names(dat5.l)[3:9],timevar="scen",idvar="id",direction="wide")
#str(dat5.w)

S5vigSums <- data.frame(id=unique(dat5.l$id))
for (i in 3:9){
  tmp <- merge(aggregate(dat5.l[,i],by=list(dat5.l$id),mean,na.rm=T),aggregate(dat5.l[,i],by=list(dat5.l$id),sd,na.rm=T),by="Group.1")
  names(tmp) <- c("id",paste(names(dat5.l)[i],"X",sep="."),paste(names(dat5.l)[i],"SD",sep="."))
  S5vigSums <- merge(S5vigSums,tmp,by="id")
}
str(S5vigSums)
S5all <- merge(dat5.w,S5traits,by="id")
S5all <- merge(S5all,S5vigSums,by="id")
str(S5all)
names(S5all)

lmCS <- lm(T.X~GTS+WVS+GSS+as.factor(female)+followup,data=S5all)
summary(lmCS)

# Call:
#   lm(formula = T.X ~ GTS + WVS + GSS + as.factor(female) + followup, 
#      data = S5all)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3724 -0.4859 -0.0156  0.5332  3.5674 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)          3.2338     0.6120   5.284 5.13e-07 ***
#   GTS                  0.3042     0.1725   1.763   0.0802 .  
#   WVS                  0.6106     0.2477   2.465   0.0150 *  
#   GSS                  0.1891     0.0954   1.982   0.0496 *  
#   as.factor(female)1  -0.4823     0.2148  -2.246   0.0264 *  
#   followup            -0.3146     0.1901  -1.655   0.1004    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9763 on 131 degrees of freedom
# (3 observations deleted due to missingness)
# Multiple R-squared:  0.2629,	Adjusted R-squared:  0.2348 
# F-statistic: 9.347 on 5 and 131 DF,  p-value: 1.234e-07

## Really fucking interesting.  So, if we predict the average Trust rating (T.X)
## across all 16 vignettes, we find that the different trust measures explain 
## some of the variance.  Neither sex (female) or willingness to be contacted 
## for follow-up significantly predicted average trust.  Again, very
## interesting.  So we have some more evidence for the trait influence on trust.

vif(lmCS)

## Center the predictors

lmCS.c <- lm(T.X~scale(GTS)+scale(WVS)+scale(GSS)+female+followup,data=S5all)
summary(lmCS.c)

# Call:
#   lm(formula = T.X ~ scale(GTS) + scale(WVS) + scale(GSS) + female + 
#        followup, data = S5all)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3724 -0.4859 -0.0156  0.5332  3.5674 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   6.4819     0.2388  27.149   <2e-16 ***
#   scale(GTS)    0.1797     0.1019   1.763   0.0802 .  
#   scale(WVS)    0.2766     0.1122   2.465   0.0150 *  
#   scale(GSS)    0.2100     0.1059   1.982   0.0496 *  
#   female       -0.4823     0.2148  -2.246   0.0264 *  
#   followup     -0.3146     0.1901  -1.655   0.1004    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9763 on 131 degrees of freedom
# (3 observations deleted due to missingness)
# Multiple R-squared:  0.2629,	Adjusted R-squared:  0.2348 
# F-statistic: 9.347 on 5 and 131 DF,  p-value: 1.234e-07


## create a common factor for the trait measures

efaT <- factanal(S5all[complete.cases(S5all[,c("GTS","WVS","GSS")]),c("GTS","WVS","GSS")],1,scores="regression")$scores
tmp3 <- S5all[complete.cases(S5all[,c("GTS","WVS","GSS")]),c("T.X","female","followup")]
tmp3 <- cbind(tmp3,efaT)
str(tmp3)
lmCS.f <- lm(T.X~Factor1+female+followup,data=tmp3)
summary(lmCS.f)

# Call:
#   lm(formula = T.X ~ Factor1 + female + followup, data = tmp3)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3335 -0.5015  0.0111  0.5027  3.6454 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   6.4913     0.2354  27.580  < 2e-16 ***
#   Factor1       0.6159     0.0941   6.546 1.18e-09 ***
#   female       -0.4864     0.2106  -2.310   0.0224 *  
#   followup     -0.3230     0.1888  -1.710   0.0895 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9711 on 133 degrees of freedom
# Multiple R-squared:  0.2597,	Adjusted R-squared:  0.243 
# F-statistic: 15.55 on 3 and 133 DF,  p-value: 9.971e-09


## Now, suppose we want to predict the variability of trust across vignettes...we
## use T.SD as our DV:

lmCS2 <- lm(T.SD^2~GTS+WVS+as.factor(female)+followup,data=S5all) ## SD NOT variance
summary(lmCS2)

# Call:
#   lm(formula = T.SD^2 ~ GTS + WVS + as.factor(female) + followup, 
#      data = S5all)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.9099 -2.2393 -0.4926  1.8326 13.4599 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)        10.68075    1.94323   5.496 1.93e-07 ***
#   GTS                 0.09427    0.55350   0.170  0.86502    
#   WVS                -2.15751    0.73045  -2.954  0.00372 ** 
#   as.factor(female)1  0.75850    0.69622   1.089  0.27794    
#   followup            0.22181    0.62293   0.356  0.72236    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.199 on 132 degrees of freedom
# (3 observations deleted due to missingness)
# Multiple R-squared:  0.08373,	Adjusted R-squared:  0.05596 
# F-statistic: 3.016 on 4 and 132 DF,  p-value: 0.02033

## the WVS predicts in a negative direction the variability of trust ratings
## across all vignetts.  Thus, as the scores increase on the WVS, the
## variability or sensitivity between vignette trust ratings decrease.  Before
## we go hog wild trying to interpret these results, note that the linear model
## accounts for only about 6% of the total variance in the trust rating
## variability.

round(cor(S5all[,1:16*7],S5all[,c(141,142,152,153)],use="pairwise.complete.obs"),2)


### How about the ETI item performance across vignettes?

## CTT
#for (i in c("G","U1","U2","R","T","B","U3")){
#  alpha(S5all[,paste(i,1:16,sep="."))
#}

#i <- 3
#alpha(S5all[,c(i,i + 1:15 * 7)])


S5all.l <- reshape(S5all,varying=names(S5all)[3:114],idvar="id",times=1:16,direction="long")
str(S5all.l)

## now include the trait predictors in our model

lmTrait.0 <- lm(T~G:U1:R,data=S5all.l)
lmTrait.1 <- lm(T~G:U1:R + GTS + WVS + followup,data=S5all.l)
summary(lmTrait.0)
summary(lmTrait.1)

## center G, U1, and R
S5all.l$G.c <- S5all.l$G - 5
S5all.l$U1.c <- S5all.l$U1 - 5
S5all.l$R.c <- S5all.l$R - 5
S5all.l$T.c <- S5all.l$T - 5

lmTrait.0c <- lm(T.c~G.c:U1.c:R.c,data=S5all.l)
lmTrait.1c <- lm(T.c~G.c:U1.c:R.c + GTS + WVS + followup,data=S5all.l)
summary(lmTrait.0c)
summary(lmTrait.1c)

## now standardize them
S5all.l$G.z <- scale(S5all.l$G)
S5all.l$U1.z <- scale(S5all.l$U1)
S5all.l$R.z <- scale(S5all.l$R)
S5all.l$T.z <- scale(S5all.l$T)

lmTrait.0z <- lm(T.z~G.z:U1.z:R.z,data=S5all.l)
lmTrait.1z <- lm(T.z~G.z:U1.z:R.z + GTS + WVS + followup,data=S5all.l)
summary(lmTrait.0z)
summary(lmTrait.1z)

#### NOTES:  note how the standardization process eliminates the differential
#### influence of R?  That is predictable because the rescaling eliminates the
#### important variance differences we observed among the trust components.

lmTrait.2z <- lm(T.z~G.z:U1.z:R.z + scale(GTS) + scale(WVS) + scale(followup) -1,data=S5all.l)
summary(lmTrait.2z)


####### Latent variable structures now tested with the wide data format

library(lavaan)

LVmodel2 <- '
G =~ G.1 + G.2 + G.3 + G.4 + G.5 + G.6 + G.7 + G.8 + G.9 + G.10 + G.11 + G.12 + G.13 + G.14 + G.15 + G.16
U =~ U1.1 + U1.2 + U1.3 + U1.4 + U1.5 + U1.6 + U1.7 + U1.8 + U1.9 + U1.10 + U1.11 + U1.12 + U1.13 + U1.14 + U1.15 + U1.16
R =~ R.1 + R.2 + R.3 + R.4 + R.5 + R.6 + R.7 + R.8 + R.9 + R.10 + R.11 + R.12 + R.13 + R.14 + R.15 + R.16
T =~ G + U + R
'
fitw <- cfa(LVmodel2,data=dat5.w)
summary(fitw,fit.measures=TRUE)

########### EMERGENT MODEL TESTED VIA PCA ##############
pca1 <- princomp(dat5.l[complete.cases(dat5.l[,c("G","U1","R")]),c("G","U1","R")])
round(summary(lm(dat5.l[complete.cases(dat5.l[,c("G","U1","R")]),"T"]~pca1$scores[,1]))$adj.r.squared,2)
### 11%

########### LANTENT MODEL TESTED VIA EFA ##############
efa1 <- factanal(dat5.l[complete.cases(dat5.l[,c("G","U1","R")]),c("G","U1","R")],1,scores="regression")
round(summary(lm(dat5.l[complete.cases(dat5.l[,c("G","U1","R")]),"T"]~efa1$scores[,1]))$adj.r.squared,2)
## 5% 
## EAT IT!

################# MODEL TESTING ANALYSES #####################

# how about transforming the trust components
library(MASS)
newdat3 <- dat3.l[,c("G","U1","R","U2","T","B","U3")]
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

outG <- boxcox(lm((G+1)~T,data=dat3.l),plotit=F)
outG <- as.data.frame(outG)
Glambda <- outG[outG$y == max(outG$y),1]
newdat3$Gt <- (newdat3$G^Glambda - 1)/Glambda

outU1 <- boxcox(lm((U1+1)~T,data=dat3.l),plotit=F)
outU1 <- as.data.frame(outU1)
U1lambda <- outU1[outU1$y == max(outU1$y),1]
newdat3$U1t <- (newdat3$U1^U1lambda - 1)/U1lambda

outR <- boxcox(lm((R+1)~T,data=dat3.l),plotit=F)
outR <- as.data.frame(outR)
Rlambda <- outR[outR$y == max(outR$y),1]
newdat3$Rt <- (newdat3$R^Rlambda - 1)/Rlambda

summary(lm(T~G*U1*R,data=newdat3))
summary(lm(T~Gt*U1t*Rt,data=newdat3))

summary(lm(T~G:U1:R,data=newdat3))
summary(lm(T~Gt:U1t:Rt,data=newdat3))

### see:  https://www.r-bloggers.com/on-box-cox-transform-in-regression-models/
##        https://www.r-bloggers.com/tukey-and-mostellers-bulging-rule-and-ladder-of-powers/

## cycle through exponents for X to determine the best fit

tmp <- dat3.l
tmp$GUR <- dat3.l$G*dat3.l$U1*dat3.l$R

outRsq <- data.frame(Exp=NA,AdjR2=NA)
for (i in seq(.1,5,by=.1)){
  outRsq <- rbind(outRsq,c(i,summary(lm(T~I(GUR^i),data=tmp))$adj.r.squared))
}
outRsq <- outRsq[-1,]
library(ggplot2)
ggplot(outRsq,aes(y=AdjR2,x=Exp)) + geom_smooth()
outRsq[outRsq$AdjR2==max(outRsq$AdjR2),]

## G exp:  3
## U exp:  .4
## R exp: 1
## NO BENEFIT TO TRANSFORMING GUR as a composite

dat3tmp <- dat3.l
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

dat3tmp$GUR <- dat3tmp$G*dat3tmp$U1*dat3tmp$R
lm3GUR <- lm(T~GUR,data=dat3tmp)
summary(lm3GUR)
pGUR <- ggplot(dat3tmp,aes(x=GUR,y=T)) + geom_smooth()
pGUR

round(cor(x=dat3tmp[,c("G","U1","R")],y=dat3tmp$GUR,use="pairwise.complete.obs"),2)

lm3all <- lm(T~G:U1:R,data=dat3.l)
summary(lm3all)

##### Linear mixed-effects models ########

library(lme4)
m.0 <- lmer(T~ 1 + (1|id), data=dat3.l)
m.1 <- lmer(T~ G + (1|id), data=dat3.l)
m.2 <- lmer(T~ G + (G|id), data=dat3.l)
m.3 <- lmer(T~ G + U1 + (1|id), data=dat3.l)
m.4 <- lmer(T~ G*U1 + (1|id), data=dat3.l)
m.5 <- lmer(T~ G*U1 + (G|id), data=dat3.l)
m.6 <- lmer(T~ G*U1 + (G|id) + (U1|id), data=dat3.l)
m.7 <- lmer(T~ G*U1 + R + (1|id), data=dat3.l)
m.8 <- lmer(T~ G*U1*R + (1|id), data=dat3.l)
m.9 <- lmer(T~ G*U1*R + (G|id) + (U1|id) + (R|id), data=dat3.l)
anova(m.0,m.1,m.2,m.3,m.4,m.5,m.6,m.7,m.8,m.9) ## m.7 winner winner chicken dinner

m.10 <- lmer(T~ G:U1:R + (1|id), data=dat3.l)
anova(m.7,m.10) ## m.7 defends the crown
m.11 <- lmer(T~G:U1 + R + (1|id),data=dat3.l)
m.12 <- lmer(T~G:U1 + R + (R|id),data=dat3.l)
anova(m.7,m.11,m.12) ## m.7 reigning champ

m.13 <- lmer(T~G:U1 + R + (1|scen),data=dat3.l)
anova(m.7,m.13) ## m.13 new winner
m.14 <- lmer(T~G:U1:R + (1|scen),data=dat3.l)
m.15 <- lmer(T~G:U1 + R + (R|scen),data=dat3.l)
m.16 <- lmer(T~G:U1 + R + (G:U1|scen),data=dat3.l)
m.17 <- lmer(T~G:U1 + R + (R|scen) + (G:U1|scen),data=dat3.l)
anova(m.13,m.15,m.16,m.17)  ## m.15 winner so far.  Any contenders?

anova(m.15,m.17)  ## technically, m.17 outperforms m.15

## one more...
m.18 <- lmer(T~G:U1 + R + (R|scen) + (G:U1|id),data=dat3.l)
m.19 <- lmer(T~ G*U1*R + (G|scen) + (U1|scen) + (R|scen), data=dat3.l)
anova(m.15,m.17,m.19)

## compare m.19 to the lm3
lm3.all <- lm(T~G*U1*R,data=dat3.l)
anova(m.19,lm3.all)

### WE HAVE A WINNER - m.19
summary(lm(m.19))
summrary(m.19)
m.19.out <- round(summary(m.19)$coefficients,3)
coefficients(m.19)

## are we sure - for Simone's dissertation:
Sdat1 <- rbind(dat3.l,dat4.l)
m.19.34 <- lmer(T~ G*U1*R + (G|scen) + (U1|scen) + (R|scen), data=Sdat1)
summary(m.19.34)  ## Simone - the combined data results

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)  7.768e-01  9.266e-01  1.240e+01   0.838 0.417729    
#   G            1.317e-01  7.234e-02  2.250e+01   1.820 0.082090 .  
#   U1           3.649e-01  7.126e-02  1.861e+02   5.121 7.56e-07 ***
#   R            8.743e-01  1.097e-01  2.340e+01   7.973 4.06e-08 ***
#   G:U1        -3.708e-02  8.297e-03  2.646e+02  -4.469 1.16e-05 ***
#   G:R         -2.840e-02  1.008e-02  1.082e+03  -2.816 0.004949 ** 
#   U1:R        -3.731e-02  1.076e-02  1.892e+03  -3.468 0.000536 ***
#   G:U1:R       5.099e-03  1.298e-03  1.360e+03   3.930 8.93e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

coefficients(m.19.34)
# $scen
# (Intercept)          G        U1         R        G:U1         G:R        U1:R      G:U1:R
# 1   1.7455990 0.05110406 0.4950382 0.6980390 -0.03708066 -0.02839474 -0.03730836 0.005099007
# 2   0.4639960 0.15767138 0.3712248 1.0476755 -0.03708066 -0.02839474 -0.03730836 0.005099007
# 3   1.7295486 0.05243867 0.3493499 0.9210308 -0.03708066 -0.02839474 -0.03730836 0.005099007
# 4   1.0314259 0.11048868 0.3305327 1.0166875 -0.03708066 -0.02839474 -0.03730836 0.005099007
# 5  -1.1731920 0.29380617 0.3403643 0.4295692 -0.03708066 -0.02839474 -0.03730836 0.005099007
# 6   0.5658734 0.14920011 0.3432174 1.0540858 -0.03708066 -0.02839474 -0.03730836 0.005099007
# 7   1.0903201 0.10559154 0.3429476 1.0162971 -0.03708066 -0.02839474 -0.03730836 0.005099007
# 8   0.7604526 0.13302054 0.3465621 0.8112053 -0.03708066 -0.02839474 -0.03730836 0.005099007

## S2 lmer summary
m.19.5 <- lmer(T~ G*U1*R + (G|scen) + (U1|scen) + (R|scen), data=dat5.l)
summary(m.19.5)  ## Simone - the combined data results

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)  2.609e+00  6.661e-01  2.730e+01   3.917 0.000543 ***
#   G           -3.494e-02  6.721e-02  1.703e+02  -0.520 0.603791    
#   U1           2.148e-01  6.150e-02  1.368e+02   3.493 0.000644 ***
#   R            5.753e-01  8.638e-02  8.230e+01   6.660 2.87e-09 ***
#   G:U1        -2.093e-02  7.179e-03  1.234e+03  -2.916 0.003614 ** 
#   G:R         -3.214e-03  9.011e-03  2.651e+03  -0.357 0.721346    
#   U1:R        -1.254e-02  8.928e-03  2.662e+03  -1.404 0.160416    
#   G:U1:R       2.925e-03  1.041e-03  2.913e+03   2.809 0.005001 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

coefficients(m.19.5)
# $scen
# (Intercept)            G          U1         R        G:U1         G:R        U1:R      G:U1:R
# 1   -0.1765720 -0.068753231 0.298013571 0.3880168 -0.02093161 -0.00321407 -0.01253571 0.002924955
# 2    1.5478118 -0.004390015 0.196111372 0.8379465 -0.02093161 -0.00321407 -0.01253571 0.002924955
# 3    2.3409806 -0.028728464 0.108006847 0.7178988 -0.02093161 -0.00321407 -0.01253571 0.002924955
# 4    3.3861436 -0.035241662 0.229106142 0.7105715 -0.02093161 -0.00321407 -0.01253571 0.002924955
# 5    1.7129846  0.219617957 0.153686692 0.1356922 -0.02093161 -0.00321407 -0.01253571 0.002924955
# 6    2.5303451 -0.022185351 0.174983011 0.7662779 -0.02093161 -0.00321407 -0.01253571 0.002924955
# 7    4.7183889 -0.099722825 0.182915560 0.6490968 -0.02093161 -0.00321407 -0.01253571 0.002924955
# 8    4.5953697 -0.036641119 0.254479760 0.6650427 -0.02093161 -0.00321407 -0.01253571 0.002924955
# 9   -1.7157746 -0.117274749 0.241591105 0.3349340 -0.02093161 -0.00321407 -0.01253571 0.002924955
# 10   3.4502794 -0.068429007 0.149177783 0.6988675 -0.02093161 -0.00321407 -0.01253571 0.002924955
# 11   2.6205951 -0.043757594 0.303564886 0.5454445 -0.02093161 -0.00321407 -0.01253571 0.002924955
# 12   3.4730347 -0.110260453 0.338852046 0.3256550 -0.02093161 -0.00321407 -0.01253571 0.002924955
# 13   2.0519377 -0.025205892 0.221637247 0.8773964 -0.02093161 -0.00321407 -0.01253571 0.002924955
# 14   1.3135966  0.034932308 0.253374198 0.5997535 -0.02093161 -0.00321407 -0.01253571 0.002924955
# 15   9.4815471 -0.215276427 0.324172107 0.3592040 -0.02093161 -0.00321407 -0.01253571 0.002924955
# 16   0.4165199  0.062234972 0.007420797 0.5933464 -0.02093161 -0.00321407 -0.01253571 0.002924955

library(xtable)
library(R2HTML)
HTML(print(xtable(m.19.out),type="html"),"Model19dat3.doc",sep="")
## NOW - import that file into Google Docs (or MS Word) and you are all set

# Call:
#   lm(formula = m.19)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.8207 -1.0360 -0.0657  0.8230  7.5747 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.753198   0.583409  -1.291 0.196957    
# G            0.135424   0.073610   1.840 0.066066 .  
# U1           0.390147   0.079378   4.915 1.02e-06 ***
# R            1.036380   0.098882  10.481  < 2e-16 ***
# scen         0.190486   0.026287   7.246 7.89e-13 ***
# G:U1        -0.038384   0.009873  -3.888 0.000107 ***
# G:R         -0.037597   0.012841  -2.928 0.003480 ** 
# U1:R        -0.056185   0.014145  -3.972 7.57e-05 ***
# G:U1:R       0.007337   0.001774   4.136 3.80e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.799 on 1134 degrees of freedom
# (17 observations deleted due to missingness)
# Multiple R-squared:  0.594,	Adjusted R-squared:  0.5911 
# F-statistic: 207.4 on 8 and 1134 DF,  p-value: < 2.2e-16

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

## Replication attempt to the linear model
m.19.R <- lmer(T~ G*U1*R + (G|scen) + (U1|scen) + (R|scen), data=dat4.l)
summary(lm(m.19.R))

library(lmerTest)

## convergence problems for the replication of m19 lead me to these steps:
d5tmp <- dat5.l
d5tmp$G <- scale(d5tmp$G,T,F)
d5tmp$U1 <- scale(d5tmp$U1,T,F)
d5tmp$R <- scale(d5tmp$R,T,F)

m19.R.2 <- lmer(T~ G*U1*R + (G|scen) + (U1|scen) + (R|scen), data=d5tmp)
summary(m19.R.2) ## use these results rather than the results from the lm() summary below
summary(lm(m19.R.2))



m19.R.2a <- lmer(T~ G*U1*R + (G|scen) + (U1|scen) + (R|scen), data=subset(dat5.l,dat5.l$scen < 9))  ## failed to converge
summary(lm(m19.R.2a))

m19.R.2b <- lmer(T~ G*U1*R + (G|scen) + (U1|scen) + (R|scen), data=subset(dat5.l,dat5.l$scen > 8))
summary(lm(m19.R.2b))

#### ICC estimates by study #####
## be sure to load the lmerICCest (see below)
ic3 <- lmer(T~1 + (1|id) + (1|scen), data=dat3.l)
lmerICCest(ic3,"id") # 0.08
lmerICCest(ic3,"scen") # 0.36
ic4 <- lmer(T~1 + (1|id) + (1|scen), data=dat4.l)
lmerICCest(ic4,"id") # 0.12
lmerICCest(ic4,"scen") # 0.33
ic5 <- lmer(T~1 + (1|id) + (1|scen), data=dat5.l)
lmerICCest(ic5,"id") # 0.07
lmerICCest(ic5,"scen") # 0.27

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
d3 <- dat3.l
d3$study <- 3
d4 <- dat4.l
d4$study <- 4
d5 <- dat5.l
d5$study <- 5

ATD <- rbind(d3,d4,d5)
ATD$study <- as.factor(ATD$study)
summary(ATD)
library(Amelia)
missmap(ATD) ## yo yo, check it out.  Pretty cool, eh?
ATD <- ATD[complete.cases(ATD),]

## some EDA on ATD

library(ggplot2)
ggplot(ATD,aes(x=as.factor(scen),y=G)) + geom_boxplot(aes(col=as.factor(source)))
ggplot(ATD,aes(x=as.factor(scen),y=U1)) + geom_boxplot(aes(col=as.factor(source)))
ggplot(ATD,aes(x=as.factor(scen),y=R)) + geom_boxplot(aes(col=as.factor(source)))
ggplot(ATD,aes(x=as.factor(scen),y=T)) + geom_boxplot(aes(col=as.factor(source)))
ggplot(ATD,aes(x=as.factor(scen),y=B)) + geom_boxplot(aes(col=as.factor(source)))
summary(ATD)
library(psych)
describe(ATD)
pairs.panels(ATD)

## don't do this again unless necessary (i.e., fixed something in the data frames)
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

lmerICCest(A012,"id")
lmerICCest(A012,"scen")
lmerICCest(A012,"study")
as.data.frame(VarCorr(A012))

predict(A012GURri)

##### Emergent-latent paper analysis CFA ####

library(lavaan)
library(semTools)
ETI.ms.model <- 'trust =~ G + U1 + R'
fit.ETI.ms <- cfa(ETI.ms.model,data=ATD,group="scen",group.equal=c("loadings"))
##measurementInvariance(ETI.ms.model,data=ATD,group="scen")
summary(fit.ETI.ms)
fitmeasures(fit.ETI.ms)
## certainly not invariant across vignetts fo sho.


ETI.cfa.all <- cfa(ETI.ms.model,data=ATD)
summary(ETI.cfa.all)
fitmeasures(ETI.cfa.all)
## whoa!  perfect fit for the cfa model.  Something fishy.


## hmmmm, no convergence here.  Bummer
ETI.full.model <- 'trust =~ G + U1 + R
                T ~ trust'
ETI.full <- sem(ETI.full.model,data=ATD)
summary(ETI.full)

## create table of correlations between G, U1, and R by scen
#GU1Rcorr <- data.frame(scen=NA,GU1=NA,GR=NA,U1R=NA,GT=NA,U1T=NA,RT=NA)
outTMP1 <- data.frame(scen=NA,rel=NA,r=NA)
for (i in 1:16){
  tmp <- round(cor(ATD[ATD$scen==i,3:9]),2)
  tmp <- as.data.frame(as.table(tmp[lower.tri(tmp)]))
  names(tmp) <- c("rel","r")
  tmp$rel <- c("GU1","GU2","GR","GT","GB","GU3","U1U2","U1R","U1T","U1B","U1U3","U2R","U2T","U2B","U2U3","RT","RB","RU3","TB","TU3","BU3")
  tmp$scen <- i
  tmp <- tmp[,c("scen","rel","r")]
  outTMP1 <- rbind(outTMP1,tmp)
}
outTMP1 <- outTMP1[-1,]
outTMP1$scen <- as.factor(outTMP1$scen)
outTMP1$rel <- as.factor(outTMP1$rel)
outTMP1

library(ggplot2)
ggplot(outTMP1,aes(x=r,fill=rel)) + geom_density(alpha=.3)

## just for G, U1, and R

outTMP2 <- data.frame(scen=NA,rel=NA,r=NA)
for (i in 1:16){
  tmp <- round(cor(ATD[ATD$scen==i,c(3,4,6)]),2)
  tmp <- as.data.frame(as.table(tmp[lower.tri(tmp)]))
  names(tmp) <- c("rel","r")
  tmp$rel <- c("GU1","GR","U1R")
  tmp$scen <- i
  tmp <- tmp[,c("scen","rel","r")]
  outTMP2 <- rbind(outTMP2,tmp)
}
outTMP2 <- outTMP2[-1,]
outTMP2$scen <- as.factor(outTMP2$scen)
outTMP2$rel <- as.factor(outTMP2$rel)
outTMP2

ggplot(outTMP2,aes(x=r,fill=rel)) + geom_density(alpha=.3)

