## A clean look at the ATD data.frame

### The focus is on the vignettes and then the person levels.  We want to be
### able to separate the respondents from the vignettes here.

####### Data Handling ----------------
Rdat <- read.csv("./Data345.csv",header=T)
summary(Rdat)

Rdat.G <- Rdat[,c(1,2,3,10,11)]
Rdat.U1 <- Rdat[,c(1,2,4,10,11)]
Rdat.U2 <- Rdat[,c(1,2,5,10,11)]
Rdat.R <- Rdat[,c(1,2,6,10,11)]
Rdat.T <- Rdat[,c(1,2,7,10,11)]
Rdat.B <- Rdat[,c(1,2,8,10,11)]
Rdat.U3 <- Rdat[,c(1,2,9,10,11)]

Rdat.Gw <- reshape(Rdat.G,v.names="G",timevar="scen",idvar=c("id","study","source"),direction="wide")
Rdat.U1w <- reshape(Rdat.U1,v.names="U1",timevar="scen",idvar=c("id","study","source"),direction="wide")
Rdat.U2w <- reshape(Rdat.U2,v.names="U2",timevar="scen",idvar=c("id","study","source"),direction="wide")
Rdat.Rw <- reshape(Rdat.R,v.names="R",timevar="scen",idvar=c("id","study","source"),direction="wide")
Rdat.Tw <- reshape(Rdat.T,v.names="T",timevar="scen",idvar=c("id","study","source"),direction="wide")
Rdat.Bw <- reshape(Rdat.B,v.names="B",timevar="scen",idvar=c("id","study","source"),direction="wide")
Rdat.U3w <- reshape(Rdat.U3,v.names="U3",timevar="scen",idvar=c("id","study","source"),direction="wide")

######### Useful Functions ----------------
clnRasch <- function(x){
  out <- x
  for (i in 1:ncol(x)){
    x[,i] <- is.na(x[,i])
  }
  
  out <- out[rowSums(x) > 3,]
  return(out)
}

GI.clean <- clnRasch(Rdat.Gw[,4:19])

########## Rasch models #########
library(eRm) ## doesn't work because the vignettes do not all use the full rating scale
library(CTT)
library(ggplot2)

#####  Goal Importance -----
GI.rasch <- PCM(Rdat.Gw[complete.cases(Rdat.Gw),4:19])
GI.ctt <- reliability(Rdat.Gw[,4:19])
VigRes <- data.frame(Vignette=1:16,G=round(t(t(GI.ctt$itemMean)),2))
row.names(VigRes) <- 1:16
VigRes

ggplot(Rdat.G,aes(x=G,group=as.factor(scen),fill=as.factor(scen))) + geom_density(alpha=.3)

#####  Prior Uncertainty -----
U1.ctt <- reliability(Rdat.U1w[,4:19])
VigRes <- data.frame(VigRes,U1=round(t(t(U1.ctt$itemMean)),2))
VigRes

ggplot(Rdat.U1,aes(x=U1,group=as.factor(scen),fill=as.factor(scen))) + geom_density(alpha=.3)

#####  Uncertainty of Agent -----
U2.ctt <- reliability(Rdat.U2w[,4:19])
VigRes <- data.frame(VigRes,U2=round(t(t(U2.ctt$itemMean)),2))
VigRes

ggplot(Rdat.U2,aes(x=U2,group=as.factor(scen),fill=as.factor(scen))) + geom_density(alpha=.3)

#####  Reliance on Agent -----
R.ctt <- reliability(Rdat.Rw[,4:19])
VigRes <- data.frame(VigRes,R=round(t(t(R.ctt$itemMean)),2))
VigRes

ggplot(Rdat.R,aes(x=R,group=as.factor(scen),fill=as.factor(scen))) + geom_density(alpha=.3)

#####  Trust -----
T.ctt <- reliability(Rdat.Tw[,4:19])
VigRes <- data.frame(VigRes,T=round(t(t(T.ctt$itemMean)),2))
VigRes

ggplot(Rdat.T,aes(x=T,group=as.factor(scen),fill=as.factor(scen))) + geom_density(alpha=.3)

#####  Behavioral Intent -----
B.ctt <- reliability(Rdat.Bw[,4:19])
VigRes <- data.frame(VigRes,B=round(t(t(B.ctt$itemMean)),2))
VigRes

ggplot(Rdat.B,aes(x=B,group=as.factor(scen),fill=as.factor(scen))) + geom_density(alpha=.3)

#####  Uncertainty after Trust -----
U3.ctt <- reliability(Rdat.U3w[,4:19])
VigRes <- data.frame(VigRes,U3=round(t(t(U3.ctt$itemMean)),2))
VigRes

ggplot(Rdat.U3,aes(x=U3,group=as.factor(scen),fill=as.factor(scen))) + geom_density(alpha=.3)

##### CREATE LONG FILE FOR PLOTTING ------
VigRes.w <- reshape(VigRes,varying=names(VigRes)[2:8],idvar="Vignette",v.names="Value",timevar="Measure",times=names(VigRes)[2:8],direction="long")
VigRes.w


##### Plot it now -----------
library(ggplot2)
ggplot(VigRes.w,aes(x=as.factor(Vignette),y=Value,col=Measure))+geom_boxplot()

##### Plot the full data --------
tmp <- Rdat[,2:9]
row.names(tmp) <- 1:nrow(tmp)
Rdat.w <- reshape(tmp,varying=names(tmp)[2:8],idvar="scen",v.names="Value",timevar="Measure",times=names(tmp)[2:8],direction="long",new.row.names=1:40971)
str(Rdat.w)
ggplot(Rdat.w,aes(x=as.factor(scen),y=Value,col=Measure))+geom_boxplot()
ggplot(Rdat.w,aes(x=scen,y=Value,col=Measure))+geom_smooth()

ggplot(Rdat.w,aes(x=Value,y))

##### Look at the response tendencies by subject and vignette

Rdat$Sid <- Rdat$study*1000 + Rdat$id
summary(Rdat$Sid)

lmerICCest <- function(x,facet=NULL){
  tmp <- as.data.frame(VarCorr(x))[,c("grp","vcov")]
  out <- round(tmp$vcov[!is.na(match(tmp$grp,facet))]/sum(tmp$vcov),2)
  return(out)
}

library(lme4)
l.G <- lmer(G~1+(1|Sid)+(1|scen)+(1|source)+(1|study),data=Rdat)
l.U1 <- lmer(U1~1+(1|Sid)+(1|scen)+(1|source)+(1|study),data=Rdat)
l.U2 <- lmer(U2~1+(1|Sid)+(1|scen)+(1|source)+(1|study),data=Rdat)
l.R <- lmer(R~1+(1|Sid)+(1|scen)+(1|source)+(1|study),data=Rdat)
l.T <- lmer(T~1+(1|Sid)+(1|scen)+(1|source)+(1|study),data=Rdat)
l.B <- lmer(B~1+(1|Sid)+(1|scen)+(1|source)+(1|study),data=Rdat)
l.U3 <- lmer(U3~1+(1|Sid)+(1|scen)+(1|source)+(1|study),data=Rdat)

ICCoutput <- data.frame(Outcome=NA,Subject=NA,Vignette=NA,DataSource=NA,Study=NA)
### Create dataframe of the results from the ICC estimates above
ICCoutput <- rbind(ICCoutput,c("Goal Importance",lmerICCest(l.G,"Sid"),lmerICCest(l.G,"scen"),lmerICCest(l.G,"source"),lmerICCest(l.G,"study")))
ICCoutput <- rbind(ICCoutput,c("Initial Uncertainty",lmerICCest(l.U1,"Sid"),lmerICCest(l.U1,"scen"),lmerICCest(l.U1,"source"),lmerICCest(l.U1,"study")))
ICCoutput <- rbind(ICCoutput,c("Uncertainty about Agent",lmerICCest(l.U2,"Sid"),lmerICCest(l.U2,"scen"),lmerICCest(l.U2,"source"),lmerICCest(l.U2,"study")))
ICCoutput <- rbind(ICCoutput,c("Reliance",lmerICCest(l.R,"Sid"),lmerICCest(l.R,"scen"),lmerICCest(l.R,"source"),lmerICCest(l.R,"study")))
ICCoutput <- rbind(ICCoutput,c("SR Trust",lmerICCest(l.T,"Sid"),lmerICCest(l.T,"scen"),lmerICCest(l.T,"source"),lmerICCest(l.T,"study")))
ICCoutput <- rbind(ICCoutput,c("Behavioral Intent",lmerICCest(l.B,"Sid"),lmerICCest(l.B,"scen"),lmerICCest(l.B,"source"),lmerICCest(l.B,"study")))
ICCoutput <- rbind(ICCoutput,c("Post Decision Uncertainty",lmerICCest(l.U3,"Sid"),lmerICCest(l.U3,"scen"),lmerICCest(l.U3,"source"),lmerICCest(l.U3,"study")))

ICCoutput <- ICCoutput[-1,]
ICCoutput

### these results indicate that subjects don't really contribute much compared to the vignettes
### The VIGNETTES are doing what we want them to do - create variance in the different aspects of trust

### what happens if we test a full model with all our trust bits as predictors:

lm.full <- lm(T~G*U1*U2*R*U3,data=Rdat)
summary(lm.full)
