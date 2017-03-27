###############################################################################
##  Title:  Trust Measure Development Analysis
##  Author:  Patrick E. McKnight (pmcknig@gmu.edu)
##           Simone Erchov (sfranz1@gmu.edu)
##           Lisa Alexander (lalexan8@gmu.edu)
##  Created:  11/20/2016
##  Last Edited By:  Patrick
##  Lasted Edited Date:  11/22/2016
##  Description:  Data analysis for the Trust Measurement development paper 
##                where we documented the ETI item performance.
##  Publication:  TBA
###############################################################################

############################ LOAD ENVIRONMENT #################################
### NOTE:  include all relevant packages here
library(car)
library(psych)
library(ggplot2)

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


####################### TRUST MODEL DATA SOURCES ##############################
## NOTE:  load data source here and conclude each section with a final object
## RESULT:  Each study produced slightly different data.  The data structure
##          looks like this:
##          id, scen, G, R, U, T, B and sometimes something else
##          also the order may change
##          The files are always LONG after this section

### Study 1 Data:  Retrospectively Recalled Trust Scenarios ####
setwd("~/Trust/Measure Development/Final Analyses")
dat1 <- read.csv("./S1 Trust Data - cleaned_Jan2016.csv", header=T)
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
tmp2 <- read.csv("./S2 TrustVig2.csv",header=T)
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

### Study 3 Data:  8 of 24 Vignettes Presented with U broke into 3 - data from 3 sources #####

############ :SONA data ########
dat3s <- read.csv("./S3 Vignettes_Round_3_SONA_10302016.csv",header=T) ## SONA
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
dat3m <- read.csv("./S3 Vignettes_Round_3__MTurk.csv",header=T)
dat3mTIME <- as.numeric((strptime(as.character(dat3m$V9),"%m/%d/%Y %H:%M") - strptime(as.character(dat3m$V8),"%m/%d/%Y %H:%M"))/60)
table(dat3mTIME > 10)
dat3m <- dat3m[dat3mTIME > 10,]
dat3m$ID <- c(1:nrow(dat3m))
d1 <- data.frame(id=dat3m$ID,scen=1,dat3m[,c("Q102","Q103","Q104","Q105","Q106","Q85","Q86")])
d2 <- data.frame(id=dat3m$ID,scen=2,dat3m[,c("Q170","Q171","Q173","Q175","Q176","Q89","Q90")])
d3 <- data.frame(id=dat3m$ID,scen=3,dat3m[,c("Q64","Q161","Q65","Q165","Q166","Q87","Q88")])
d4 <- data.frame(id=dat3m$ID,scen=4,dat3m[,c("Q179","Q180","Q182","Q184","Q185","Q91","Q92")])
d5 <- data.frame(id=dat3m$ID,scen=5,dat3m[,c("Q188","Q189","Q191","Q193","Q194","Q93","Q94")])
d6 <- data.frame(id=dat3m$ID,scen=6,dat3m[,c("Q197","Q198","Q200","Q201","Q203","Q95","Q96")])
d7 <- data.frame(id=dat3m$ID,scen=7,dat3m[,c("Q206","Q207","Q209","Q212","Q213","Q97","Q98")])
d8 <- data.frame(id=dat3m$ID,scen=8,dat3m[,c("Q215","Q216","Q217","Q221","Q222","Q99","Q100")])
names(d1) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d2) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d3) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d4) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d5) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d6) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d7) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d8) <- c("id","scen","G","U1","U2","R","T","B","U3")
dat3m.l <- rbind(d1,d2,d3,d4,d5,d6,d7,d8)
dat3m.l$source <- "mTurk"

############ :Reddit data ##################
dat3r <- read.csv("./S3 Vignettes_Round_3__Reddit.csv",header=T)
dat3rTIME <- as.numeric((strptime(as.character(dat3r$V9),"%m/%d/%Y %H:%M") - strptime(as.character(dat3r$V8),"%m/%d/%Y %H:%M"))/60)
table(dat3rTIME > 10)
dat3r <- dat3r[dat3rTIME > 10,]
dat3r$ID <- c(1:nrow(dat3r))
d1 <- data.frame(id=dat3r$ID,scen=1,dat3r[,c("Q102","Q103","Q104","Q105","Q106","Q85","Q86")])
d2 <- data.frame(id=dat3r$ID,scen=2,dat3r[,c("Q170","Q171","Q173","Q175","Q176","Q89","Q90")])
d3 <- data.frame(id=dat3r$ID,scen=3,dat3r[,c("Q64","Q161","Q65","Q165","Q166","Q87","Q88")])
d4 <- data.frame(id=dat3r$ID,scen=4,dat3r[,c("Q179","Q180","Q182","Q184","Q185","Q91","Q92")])
d5 <- data.frame(id=dat3r$ID,scen=5,dat3r[,c("Q188","Q189","Q191","Q193","Q194","Q93","Q94")])
d6 <- data.frame(id=dat3r$ID,scen=6,dat3r[,c("Q197","Q198","Q200","Q201","Q203","Q95","Q96")])
d7 <- data.frame(id=dat3r$ID,scen=7,dat3r[,c("Q206","Q207","Q209","Q212","Q213","Q97","Q98")])
d8 <- data.frame(id=dat3r$ID,scen=8,dat3r[,c("Q215","Q216","Q217","Q221","Q222","Q99","Q100")])
names(d1) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d2) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d3) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d4) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d5) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d6) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d7) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d8) <- c("id","scen","G","U1","U2","R","T","B","U3")
dat3r.l <- rbind(d1,d2,d3,d4,d5,d6,d7,d8)
dat3r.l$source <- "Reddit"

############# :Flyer data ###################
dat3f <- read.csv("./S3 Vignettes_Round_3__Flyers.csv",header=T)
dat3fTIME <- as.numeric((strptime(as.character(dat3f$V9),"%m/%d/%Y %H:%M") - strptime(as.character(dat3f$V8),"%m/%d/%Y %H:%M"))/60)
table(dat3fTIME > 10)
dat3f <- dat3f[dat3fTIME > 10,]
dat3f$ID <- c(1:nrow(dat3f))
d1 <- data.frame(id=dat3f$ID,scen=1,dat3f[,c("Q102","Q103","Q104","Q105","Q106","Q85","Q86")])
d2 <- data.frame(id=dat3f$ID,scen=2,dat3f[,c("Q170","Q171","Q173","Q175","Q176","Q89","Q90")])
d3 <- data.frame(id=dat3f$ID,scen=3,dat3f[,c("Q64","Q161","Q65","Q165","Q166","Q87","Q88")])
d4 <- data.frame(id=dat3f$ID,scen=4,dat3f[,c("Q179","Q180","Q182","Q184","Q185","Q91","Q92")])
d5 <- data.frame(id=dat3f$ID,scen=5,dat3f[,c("Q188","Q189","Q191","Q193","Q194","Q93","Q94")])
d6 <- data.frame(id=dat3f$ID,scen=6,dat3f[,c("Q197","Q198","Q200","Q201","Q203","Q95","Q96")])
d7 <- data.frame(id=dat3f$ID,scen=7,dat3f[,c("Q206","Q207","Q209","Q212","Q213","Q97","Q98")])
d8 <- data.frame(id=dat3f$ID,scen=8,dat3f[,c("Q215","Q216","Q217","Q221","Q222","Q99","Q100")])
names(d1) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d2) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d3) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d4) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d5) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d6) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d7) <- c("id","scen","G","U1","U2","R","T","B","U3")
names(d8) <- c("id","scen","G","U1","U2","R","T","B","U3")
dat3f.l <- rbind(d1,d2,d3,d4,d5,d6,d7,d8)
dat3f.l$source <- "Flyers"

dat3all.l <- rbind(dat3s.l,dat3m.l,dat3r.l,dat3f.l)
dat3all.l$source <- as.factor(dat3all.l$source)


############################# CLEAN UP ENVIRONMENT #################################
rm(list=ls()[!(ls() %in% c('dat1.l','dat2.l','dat3s.l','dat3m.l','dat3r.l','dat3f.l','dat3all.l','subONE','QualBeh','Ufold'))])

## reload packages
library(car)
library(psych)
library(ggplot2)

######################### DATA RECODING AND MANIPULATION ###########################

########### Study 1 --------------
dat1.l <- subONE(dat1.l,2:5)
dat1.lUr <- Ufold(dat1.l,5)
# double long for plots
dat1.l2 <- reshape(dat1.l,varying=list(names(dat1.l)[3:5]),new.row.names=1:2670,v.names="value",times=names(dat1.l)[3:5],timevar="measure",idvar="id",direction="long")
dat1.lUr2 <- reshape(dat1.lUr,varying=list(names(dat1.lUr)[3:5]),new.row.names=1:2670,v.names="value",times=names(dat1.lUr)[3:5],timevar="measure",idvar="id",direction="long")

########### Study 2 --------------
dat2.lUr <- Ufold(dat2.l,5)
# double long for plots
dat2.l2 <- reshape(dat2.l,varying=list(names(dat2.l)[4:6]),new.row.names=1:648,v.names="value",times=names(dat2.l)[4:6],timevar="measure",idvar='id',direction="long")
dat2.lUr2 <- reshape(dat2.lUr,varying=list(names(dat2.lUr)[4:6]),new.row.names=1:648,v.names="value",times=names(dat2.lUr)[4:6],timevar="measure",idvar='id',direction="long")


########## Study 3 ---------------
dat3all.l <- subONE(dat3all.l,c(3:7,9)) # subtract one from qualtrics data
dat3all.l <- QualBeh(dat3all.l,8) # create binary behavior scale
dat3all.lUr <- Ufold(dat3all.l,c(4,5,9)) # fold uncertainty scale
# double long for plots
dat3all.l2 <- reshape(dat3all.l,varying=names(dat3all.l[c(3:6,9)]),new.row.names=1:5800,v.names="value",timevar="Measure",times=c("G","U1","U2","R","U3"),idvar=c("id","scen"),direction="long")
dat3all.lUr2 <- reshape(dat3all.lUr,varying=names(dat3all.lUr[c(3:6,9)]),new.row.names=1:5800,v.names="value",timevar="Measure",times=c("G","U1","U2","R","U3"),idvar=c("id","scen"),direction="long")

########################### EFAs ###########################

########## Study 1 --------------
dat1.EFA <- na.omit(dat1.l[,c(3:5)])
FA1 <- factanal(dat1.EFA, factors=1, scores="regression")
print(FA1)

########## Study 2 --------------
dat2.EFA <- na.omit(dat2.l[,c(4:6)])
FA2 <- factanal(dat2.EFA, factors=1, scores="regression")
print(FA2)

########## Study 3 --------------
dat3.EFA <- na.omit(dat3all.l[,c(3,4,6)])
FA3 <- factanal(dat3.EFA, factors=1, scores="regression")
print(FA3)

########################### LINEAR MODELS AND PLOTS ###################################

############# Study 1 ----------
lm1linear <- lm(T~G+U+R,data=dat1.l)
summary(lm1linear)
lm1 <- lm(T~G:U:R,data=dat1.l)
summary(lm1)
lm1r <- lm(T~G:U:R,data=dat1.lUr)
summary(lm1r)
#### T vs G U R - plot
p1normal <- ggplot(dat1.l2,aes(y=value,x=T,by=measure))+geom_smooth(aes(colour=measure))
p1normal
p1Ufold <- ggplot(dat1.lUr2,aes(y=value,x=T,by=measure))+geom_smooth(aes(colour=measure))
p1Ufold
### Interaction Plot
dat.hat <- data.frame(G=c(9,9,2,2,9,9,2,2),R=c(9,2,9,2,9,2,9,2),U=c(9,9,9,9,2,2,2,2))
lm1.hat <- predict(lm1r,newdata = dat.hat,se.fit=T)
dat.hat <- cbind(dat.hat,lm1.hat$fit)
dat.hat$G <- factor(dat.hat$G,labels=c("L","H"))
dat.hat$R <- factor(dat.hat$R,labels=c("L","H"))
dat.hat$U <- factor(dat.hat$U,labels=c("L","H"))
names(dat.hat) <- c("G","R","U","T")
p1int <- ggplot(dat.hat,aes(x=factor(G),y=T,colour=factor(R))) + geom_point(size=5) + facet_wrap(~U) + guides(col=guide_legend(title="Reliance")) + xlab("Goal Importance") + ylab("Overall Trust Rating (0-10)") + ylim(c(0,10))
p1int

pdf("Study1plots.pdf")
p1normal
p1Ufold
p1int
dev.off()

############# Study 2 ----------
lm2rlinearUr <- lm(T~G+U+R,data=dat2.lUr) 
summary(lm2rlinearUr)
lm2linearU <- lm(T~G+U+R,data=dat2.l) 
summary(lm2linearU)
lm2 <- lm(T~G:U:R,data=dat2.l) ## not sure if this is the correct model
summary(lm2)
lm2r <- lm(T~G:U:R,data=dat2.lUr) ## or whether we need to fold U
summary(lm2r)
lm2mi <- lm(T~G*U*R,data=dat2.l) 
summary(lm3)
lm2rmi <- lm(T~G*U*R,data=dat2.lUr) 
summary(lm3r)

### T vs G U R - plot
p2normal <- ggplot(dat2.l2,aes(x=T,y=value,colour=measure)) + geom_smooth() ## seems this one more appropriate
p2Ufolded <- ggplot(dat2.lUr2,aes(x=T,y=value,colour=measure)) + geom_smooth()
### Interaction Plot
dat.hat <- data.frame(G=c(9,9,2,2,9,9,2,2),R=c(9,2,9,2,9,2,9,2),U=c(9,9,9,9,2,2,2,2))
lm2.hat <- predict(lm2,newdata = dat.hat,se.fit=T)
dat.hat <- cbind(dat.hat,lm2.hat$fit)
dat.hat$G <- factor(dat.hat$G,labels=c("L","H"))
dat.hat$R <- factor(dat.hat$R,labels=c("L","H"))
dat.hat$U <- factor(dat.hat$U,labels=c("L","H"))
names(dat.hat) <- c("G","R","U","T")
p2int <- ggplot(dat.hat,aes(x=factor(G),y=T,colour=factor(R))) + geom_point(size=5) + facet_wrap(~U) + guides(col=guide_legend(title="Reliance")) + xlab("Goal Importance") + ylab("Overall Trust Rating (0-10)") + ylim(c(0,10))

pdf("Study2plots.pdf")
p2normal
p2Ufolded
p2int
dev.off()

############# Study 3 ----------
lm3rlinear <- lm(T~G+U1+R,data=dat3all.lUr)
summary(lm3rlinear)
## wrong model
lm3 <- lm(T~G:U1:R,data=dat3all.l)
summary(lm3)
## correct model with U folded
lm3r <- lm(T~G:U1:R,data=dat3all.lUr)
summary(lm3r)
lm3rmi <- lm(T~G*U1*R,data=dat3all.lUr)
summary(lm3rmi)
### T vs G U R - plot
p3normal <- ggplot(dat3all.l2,aes(x=T,y=value,colour=Measure)) + geom_smooth()
p3Ufold <- ggplot(dat3all.lUr2,aes(x=T,y=value,colour=Measure)) + geom_smooth()
### Interaction Plot
dat.hat <- data.frame(G=c(9,9,2,2,9,9,2,2),R=c(9,2,9,2,9,2,9,2),U1=c(9,9,9,9,2,2,2,2))
lm3.hat <- predict(lm3r,newdata = dat.hat,se.fit=T)
dat.hat <- cbind(dat.hat,lm3.hat$fit)
dat.hat$G <- factor(dat.hat$G,labels=c("L","H"))
dat.hat$R <- factor(dat.hat$R,labels=c("L","H"))
dat.hat$U1 <- factor(dat.hat$U1,labels=c("L","H"))
names(dat.hat) <- c("G","R","U","T")
p3int <- ggplot(dat.hat,aes(x=factor(G),y=T,colour=factor(R))) + geom_point(size=5) + facet_wrap(~U) + guides(col=guide_legend(title="Reliance")) + xlab("Goal Importance") + ylab("Overall Trust Rating (0-10)") + ylim(c(0,10))

pdf("Study3plots.pdf")
p3normal
p3Ufold
p3int
dev.off()
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

