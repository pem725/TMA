###############################################################################
##  Title:  Trust Measure Development Analysis
##  Author:  Patrick E. McKnight (pmcknigh@gmu.edu)
##           Simone Erchov (sfranz1@gmu.edu)
##           Lisa Alexander (lalexan8@gmu.edu)
##  Created:  11/20/2016
##  Last Edited By:  Patrick
##  Lasted Edited Date:  2/15/2017
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
  datTIME <- as.numeric((strptime(as.character(dat$V9),"%Y-%m-%d %H:%M:%S") - strptime(as.character(dat$V8),"%Y-%m-%d %H:%M:%S"))/60)
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
                #SONA
                c("Q194","Q296","Q198","Q200","Q201","Q204","Q205"),
                c("Q358","Q359","Q362","Q365","Q366","Q368","Q369"),
                c("Q373","Q374","Q377","Q380","Q381","Q383","Q384"),
                c("Q388","Q389","Q392","Q395","Q396","Q398","Q399"),
                c("Q403","Q404","Q407","Q410","Q411","Q413","Q414"),
                c("Q418","Q419","Q422","Q425","Q426","Q428","Q429"),
                c("Q433","Q434","Q437","Q440","Q441","Q443","Q444"),
                c("Q448","Q449","Q452","Q455","Q456","Q458","Q459"))
                
                # Flyers
                # c("Q299","Q300","Q303","Q306","Q307","Q309","Q310"),
                # c("Q314","Q315","Q318","Q321","Q322","Q324","Q325"),
                # c("Q329","Q330","Q333","Q336","Q337","Q339","Q340"),
                # c("Q344","Q345","Q348","Q351","Q352","Q354","Q355"),
                # c("Q359","Q360","Q363","Q366","Q367","Q369","Q370"),
                # c("Q374","Q375","Q378","Q381","Q382","Q384","Q385"),
                # c("Q389","Q390","Q393","Q396","Q397","Q399","Q400"),
                # c("Q404","Q405","Q408","Q411","Q412","Q414","Q415"))
                
                # MTurk
                # c("Q300","Q301","Q304","Q307","Q308","Q310","Q311"),
                # c("Q315","Q316","Q319","Q322","Q323","Q325","Q326"),
                # c("Q330","Q331","Q334","Q337","Q338","Q340","Q341"),
                # c("Q345","Q346","Q349","Q351","Q352","Q354","Q355"),
                # c("Q360","Q361","Q364","Q367","Q368","Q370","Q371"),
                # c("Q375","Q376","Q379","Q381","Q382","Q384","Q385"),
                # c("Q389","Q390","Q393","Q396","Q397","Q399","Q400"),
                # c("Q404","Q405","Q408","Q411","Q412","Q414","Q415"))
                
                # Reddit
                # Survey cut off so only completed original 8 - phooey
                
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
  datTIME <- as.numeric((strptime(as.character(dat$V9),"%Y-%m-%d %H:%M:%S") - strptime(as.character(dat$V8),"%Y-%m-%d %H:%M:%S"))/60)
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
  datTIME <- as.numeric((strptime(as.character(dat$V9),"%Y-%m-%d %H:%M:%S") - strptime(as.character(dat$V8),"%Y-%m-%d %H:%M:%S"))/60)
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
dat5m.l <- TrustDataFcn4("./Data/S5 Vignettes_Round_5__MTurk.csv",15,"mTurk")

############ :Reddit data ##################
#dat5r.l <- TrustDataFcn2("./Data/S5 Vignettes_Round_5__Reddit.csv",15,"Reddit")

############# :Flyer data ###################
dat5f.l <- TrustDataFcn3("./Data/S5 Vignettes_Round_5__Flyers.csv",15,"Flyers")

############# :Combine all Study 5 data files
#dat5all.l <- rbind(dat5s.l,dat5m.l,dat5r.l,dat5f.l)
#dat5all.l$source <- as.factor(dat5all.l$source) # refactor the source for later


############################# CLEAN UP ENVIRONMENT #################################
rm(list=ls()[!(ls() %in% c('dat1.l','dat2.l','dat3s.l','dat3m.l','dat3r.l','dat3f.l','dat4.l','dat3all.l','TrustDataFcn','TrustDataFcn2','subONE','QualBeh','Ufold','ETM.Fcn','dat5s.l'))])

## reload packages
library(car)
library(psych)
library(ggplot2)

######################### DATA RECODING AND MANIPULATION ###########################

########### Study 1 --------------
dat1.l <- subONE(dat1.l,2:5)
dat1.lUr <- Ufold(dat1.l,5)

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
dat5.l <- subONE(dat5s.l,c(3:7,9)) # subtract one from qualtrics data
dat5.l <- QualBeh(dat5.l,8) # create binary behavior scale
dat5.lUr <- Ufold(dat5.l,c(4,5,9)) # fold uncertainty scale

########################### LINEAR MODELS AND PLOTS ###################################

############# Study 1 ----------

S1 <- ETM.Fcn(dat1.l,"S1out.pdf")
S1T <- ETM.Fcn(dat1.lUr,"S1outT.pdf")

############# Study 2 ----------

S2 <- ETM.Fcn(dat2.l,"S2out.pdf")
S2T <- ETM.Fcn(dat2.lUr,"S2outT.pdf")

############# Study 3 ----------
S3 <- ETM.Fcn(dat3all.l,"S3out.pdf")
S3T <- ETM.Fcn(dat3all.l,"S3outT.pdf")

############# Study 4 ----------
S4 <- ETM.Fcn(dat4.l,"S4out.pdf")
S4 <- ETM.Fcn(dat4.lUr,"S4outT.pdf")

############# Study 5 ----------
S5a <- ETM.Fcn(subset(dat5.l,dat5.l$scen < 9),"S5outA.pdf")
S5Ua <- ETM.Fcn(subset(dat5.lUr,dat5.lUr$scen < 9),"S5outTA.pdf")

S5b <- ETM.Fcn(subset(dat5.l,dat5.l$scen > 8),"S5outB.pdf")
S5Ub <- ETM.Fcn(subset(dat5.lUr,dat5.lUr$scen > 8),"S5outTB.pdf")

S5 <- ETM.Fcn(dat5.l,"S5out.pdf")
S5 <- ETM.Fcn(dat5.lUr,"S5outT.pdf")

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
round(cor(dat3all.l[,3:9],use="pairwise.complete.obs"),2)
cor.plot(cor(dat3all.l[,3:9],use="pairwise.complete.obs"))

### Study 4
round(cor(dat4.l[,3:9],use="pairwise.complete.obs"),2)
cor.plot(cor(dat4.l[,3:9],use="pairwise.complete.obs"))

### Study 5
round(cor(dat5.l[,3:9],use="pairwise.complete.obs"),2)
cor.plot(cor(dat5.l[,3:9],use="pairwise.complete.obs"))


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
summary(lm(T~Gt*U1t*Rt,data=newdat3))

summary(lm(T~G:U1:R,data=newdat3))
summary(lm(T~Gt:U1t:Rt,data=newdat3))

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

dat3tmp$GUR <- dat3tmp$G*dat3tmp$U1*dat3tmp$R
lm3GUR <- lm(T~GUR,data=dat3tmp)
summary(lm3GUR)
pGUR <- ggplot(dat3tmp,aes(x=GUR,y=T)) + geom_smooth()
pGUR

round(cor(x=dat3tmp[,c("G","U1","R")],y=dat3tmp$GUR,use="pairwise.complete.obs"),2)

lm3all <- lm(T~G:U1:R,data=dat3all.l)
summary(lm3all)

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

### WE HAVE A WINNER - m.19
summary(lm(m.19))

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

m19.R.2a <- lmer(T~ G*U1*R + (G|scen) + (U1|scen) + (R|scen), data=subset(dat5.l,dat5.l$scen < 9))
summary(lm(m19.R.2a))

m19.R.2b <- lmer(T~ G*U1*R + (G|scen) + (U1|scen) + (R|scen), data=subset(dat5.l,dat5.l$scen > 8))
summary(lm(m19.R.2b))



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
mydists <- list(G="binomial",U1="binomial",U2="binomial",R="binomial",T="binomial",B="binomial",U3="binomial")
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

myres.c <- fitabn(dag.m=mydag, data.df=mydat[complete.cases(mydat),], data.dists=mydists)


devtools::install_github('paulgovan/BayesianNetwork')
library(BayesianNetwork)
if (interactive()) {
  BayesianNetwork::BayesianNetwork()
}

library(rstan)