setwd("XXXXXX/Final Replication")

setwd("XXXXX/Final Replication")

#install.packages("tidyverse")
#install.packages("haven")
#install.packages("estimatr")
#install.packages("texreg")
#install.packages("sjPlot")
#install.packages("margins")
#install.packages("mgcv")
#install.packages("statar")
#install.packages("lfe")
#install.packages("plm")
#install.packages("lmtest")
#install.packages("ggpubr")
#install.packages("foreign")




library(tidyverse)
library(haven)
library(estimatr)
library(texreg)
library(sjPlot)
library(margins)
library(mgcv)
library(statar)
library(lfe)
library(plm)
library(lmtest)
library(ggpubr)
library(foreign)
library(xtable)

#--------------------------------------------

# Issues

# Text edits:

############################################################################
############################################################################
# Setup
############################################################################
############################################################################


#### Austria

austria <- read_dta("./Austria_final.dta")

control2 <- "+ educ_tertiary + avg_income + lab_pct_manufact_01 + lab_pct_unemp"
control3 <- "+ educ_tertiary + avg_income + lab_pct_manufact_01 + lab_pct_unemp + welfare_cap_06 +  health_cap_06 + education_cap_06"
control4 <- "+ educ_tertiary + avg_income + lab_pct_manufact_01 + lab_pct_unemp + welfare_cap_06 +  health_cap_06 + education_cap_06 + foreignborn_delta"

control_placebo <- "+ educ_tertiary + lab_pct_manufact_01"

spec <- "dv_pop_01*pct_noneu_06"
spec2 <- "factor(treat)*pct_noneu_06"

austria$treat <- as.factor(austria$manual_ph3_10_20)

austria_p <- read_dta("./Austria_panel.dta")

##### Vienna

vienna <- read_dta("./vienna_final.dta")
vienna_p <- read_dta("./vienna_panel.dta")

vcovar_string1 <- ""
vcovar_string2 <- "+ lab_pct_pensioners + pctforeign + log_voters + educ_tertiary  + pctforeign_delta"
vcovar_string3 <- "+ lab_pct_pensioners + pctforeign + log_voters + educ_tertiary  + pctforeign_delta + private_price_w_s"


# GAM coloration
newDef <- newDef2 <- deparse(vis.gam)
newDef[grep("gray\\(seq\\(",newDef)] <- " colfunc <- colorRampPalette(c(\"white\",\"cyan\", \"dodgerblue\")); pal <- colfunc(nCol)"
vis.gam2 <- eval(parse(text=newDef))


############################################################################
############################################################################
# Main Text
############################################################################
############################################################################

#######################
### Figure 1
#######################

par(mfrow=c(1,2))
data <- austria
rugz <- data$dv_pop_01

topmax <- 50
uspan <- 1
x <- data$dv_pop_01*100
y <- data$d_rr_06- mean(data$d_rr_06,na.rm=T)
y <- y *100
plot(x,y,cex=.3,xlab="% Adults in Public Housing",ylab="Change Vote Share (Demeaned)",xlim=c(0,50),ylim=c(-5,6),col="gray",type="n")
lo <- loess(y~x,span=uspan)
xl <- seq(min(x,na.rm=T),topmax, (topmax - min(x,na.rm=T))/1000)
pred.c <- predict(lo,xl,se=T)
lines(xl, pred.c$fit, col='blue', lwd=1)
min <- pred.c$fit -  pred.c$s*1.96
max <- pred.c$fit +  pred.c$s*1.96
polygon(c(xl,rev(xl)),c(max,rev(min)),col=adjustcolor("lightblue",alpha.f=0.4),border=NA)

y <- data$d_rr_02 - mean(data$d_rr_02,na.rm=T)
y <- y *100
lo <- loess(y~x,span=uspan)
xl <- seq(min(x,na.rm=T),topmax, (topmax - min(x,na.rm=T))/1000)
pred.c <- predict(lo,xl,se=T)
lines(xl, pred.c$fit, col='black', lty=1)
min <- pred.c$fit -  pred.c$s*1.96
max <- pred.c$fit +  pred.c$s*1.96
polygon(c(xl,rev(xl)),c(max,rev(min)),col=adjustcolor("lightgray",alpha.f=0.4),border=NA)

y <- data$d_eu_rr_04 - mean(data$d_eu_rr_04,na.rm=T)
y <- y *100
lo <- loess(y~x,span=uspan)
xl <- seq(min(x,na.rm=T),topmax, (topmax - min(x,na.rm=T))/1000)
pred.c <- predict(lo,xl,se=T)
lines(xl, pred.c$fit, col='gray50', lty=2)

y <- data$d_rr_94_99 - mean(data$d_rr_94_99,na.rm=T)
y <- y *100
lo <- loess(y~x,span=uspan)
xl <- seq(min(x,na.rm=T),topmax, (topmax - min(x,na.rm=T))/1000)
pred.c <- predict(lo,xl,se=T)
lines(xl, pred.c$fit, col='black', lty=3)
min <- pred.c$fit -  pred.c$s*1.96
max <- pred.c$fit +  pred.c$s*1.96
polygon(c(xl,rev(xl)),c(max,rev(min)),col=adjustcolor("darkgray",alpha.f=0.4),border=NA)
rug(rugz*100)
legend("topleft", c("Δ 2002 - 2006 Legislative Election","Δ 1999 - 2002 Legislative Election","Δ 1994 - 1999 Legislative Election","Δ 1999 - 2004 EU Election"), col=c("blue","black","black","gray50"),lty=c(1,1,3,2),cex=.6,bg="white")


data <- austria
data <- subset(data, data$pct_noneu_06 < .18 & data$dv_pop_01 < .55)  # Trim outliers
data$y <- data$d_rr_06 - mean(data$d_rr_06 ,na.rm=T)

newDef <- deparse(vis.gam)
newDef[grep("gray\\(seq\\(",newDef)] <- " colfunc <- colorRampPalette(c(\"white\", \"dodgerblue\")); pal <- colfunc(nCol)"
vis.gam2 <- eval(parse(text=newDef))
data$dv_pop_01 <- data$dv_pop_01 *100
data$pct_noneu_06 <- data$pct_noneu_06 *100
mod.gam <- gam(y ~ s(dv_pop_01,pct_noneu_06,sp=1.5),data=data)
vis.gam2(mod.gam,plot.type="contour",color="gray",main="",xlab="% Adults in Public Housing",ylab="% Non-EU",nCol=5)
rug(rugz*100)
rug(data$pct_noneu_06,side=2)


#######################
### Table 1
#######################

fit1<- lm_robust(formula(paste0("d_rr_06 ~",spec)),data=austria)
fit2<- lm_robust(formula(paste0("d_rr_06 ~",spec,control2)),data=austria)
fit3<- lm_robust(formula(paste0("d_rr_06 ~",spec,control4)),data=austria)
fit4<- lm_robust(formula(paste0("d_rr_06 ~",spec2)),data=austria)
fit5<- lm_robust(formula(paste0("d_rr_06 ~",spec2,control4)),data=austria)

fit6<- lm_robust(formula(paste0("d_rr_94_99 ~",spec,control_placebo)),data=austria)
fit7 <- lm_robust(formula(paste0("d_rr_02 ~",spec,control_placebo)),data=austria)
fit8<- lm_robust(formula(paste0("d_eu_rr_04 ~",spec,control_placebo)),data=austria)

# All as regression table
texreg(list(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8), include.ci = FALSE,
       caption.above=TRUE,
       include.rsquared = FALSE,
       include.adjrs = FALSE,
       include.groups = FALSE,
       stars=.05,
       digits=2, scalebox=0.75, booktabs=TRUE,  use.packages = FALSE,
       caption='X',float.pos="!h")

# Table SM6 in Appendix
texreg(list(fit2,fit3,fit5), include.ci = FALSE,
       caption.above=TRUE,
       include.rsquared = FALSE,
       include.adjrs = FALSE,
       include.groups = FALSE,
       stars=.05,
       digits=2, scalebox=0.75, booktabs=TRUE,  use.packages = FALSE,
       caption='X',float.pos="!h")


#######################
### Figure 2 - Vacancy
#######################

data2 <- subset(austria,dv_pop_01 > 0)

data_nogrow <- subset(data2,citizen_eu_growth_pct < 0)

  fit2 <- lm_robust(formula(paste0("d_rr_06 ~ pct_noneu_06*vacancy_01_public+ dv_pop_01",control2)), data=data_nogrow)
  a <- plot_model(fit2,type="pred",legend.title="Vacancy Rate",color="bw",terms=c("pct_noneu_06 [0, .18]","vacancy_01_public [0, .2]")) + 
      ggtitle("Shrinking Eligible Population") + xlab("% Non-EU Residents") + ylab("Change Vote Share (Demeaned)")+
    geom_rug(data=subset(data_nogrow,pct_noneu_06<.2),aes(x = pct_noneu_06), inherit.aes = F) + ylim(0,.08)+ 
    theme_minimal() + xlim(0,.18) + 
    theme(plot.title = element_text(size = 11))

data_grow <- subset(data2,citizen_eu_growth_pct > 0)

  fit1 <- lm_robust(formula(paste0("d_rr_06 ~ pct_noneu_06*vacancy_01_public + dv_pop_01",control2)), data=data_grow)
  b <- plot_model(fit1,type="pred",legend.title="Vacancy Rate",color="bw",terms=c("pct_noneu_06 [0, .18]","vacancy_01_public [0, .2]")) + 
    ggtitle("Growing Eligible Population") + xlab("% Non-EU Residents") + ylab("")+
    geom_rug(data=subset(data_grow,pct_noneu_06<.2),aes(x = pct_noneu_06), inherit.aes = F) + ylim(0,.08) + 
    theme_minimal() + xlim(0,.18) +
    theme(plot.title = element_text(size = 11))

ggpubr::ggarrange(a,b,ncol=2,common.legend=TRUE,legend="bottom")

# Table SM 7
texreg(list(fit1,fit2), include.ci = FALSE,
       caption.above=TRUE,
       include.rsquared = FALSE,
       include.adjrs = FALSE,
       include.groups = FALSE,
       stars=.05,
       digits=2, scalebox=0.75, booktabs=TRUE,  use.packages = FALSE,
       caption='X',float.pos="!h")




#######################
### Figure 3 
#######################

span <- 1
addlines <- function(data,yvar,xvar,lty){
  data <- as.data.frame(data)
  x <-   data[,xvar]*100
  y <-   data[,yvar]- mean( data[,yvar],na.rm=T)
  lo <- loess(y~x,span=span)
  xl <- seq(min(x,na.rm=T),max(x,na.rm=T), (max(x,na.rm=T) - min(x,na.rm=T))/1000)
  pred.c <- predict(lo,xl,se=T)
  lines(xl, pred.c$fit, col='black', lwd=1,lty=lty)
  min <- pred.c$fit -  pred.c$s*1.96
  max <- pred.c$fit +  pred.c$s*1.96
  polygon(c(xl,rev(xl)),c(max,rev(min)),col=adjustcolor("gray",alpha.f=0.4),border=NA)
  
}

par(mfrow=c(1,2))
data <- vienna

x <-  data$pctpublic_w_zsp*100
y <-  data$dv - mean(data$dv,na.rm=T)

plot(x,y,cex=.3,xlab="% Adults in Public Housing",ylab="Change in Vote Share (Demeaned)",col="gray",ylim=c(-.08,.08),type="n",main="All Wards",cex.main=.85,cex.lab=.9)
lo <- loess(y~x,span=1)
xl <- seq(min(x,na.rm=T),max(x,na.rm=T), (max(x,na.rm=T) - min(x,na.rm=T))/1000)
pred.c <- predict(lo,xl,se=T)
lines(xl, pred.c$fit, col='blue', lwd=1)
min <- pred.c$fit -  pred.c$s*1.96
max <- pred.c$fit +  pred.c$s*1.96
polygon(c(xl,rev(xl)),c(max,rev(min)),col=adjustcolor("lightblue",alpha.f=0.4),border=NA)
rug(x)

addlines(vienna,"d_02","pctpublic_w_zsp",3)
addlines(vienna,"eu_rr_delta","pctpublic_w_zsp",4)
legend("topleft", c("2002-2006 Legislative Election","1999-2002 Legislative Election","1999-2004 EU Election"), col=c("blue","black","black"),lty=c(1,1,2),cex=.5)

data$pctrental <-data$pctrental*100
mod.gam <- gam(dv2 ~ s(private_price_w,pctrental,sp=3) + log_voters + pctforeign ,data=subset(data,pctpublic_w_zsp==0 & private_price_w < 51))
vis.gam2(mod.gam,plot.type="contour",view=c("pctrental","private_price_w"),cex.main=.85,cex.lab=.9,main="Wards with No Public Housing",xlab="% Rentals",ylab="Rental Price Index",color="gray",nCol=5)
rug(data$pctrental)
rug(data$private_price_w,side=2)

#######################
### Table 2 
#######################

vcovar_string1 <- ""
vcovar_string2 <- "+ lab_pct_pensioners + pctforeign + log_voters + educ_tertiary  + pctforeign_delta"
vcovar_string3 <- "+ lab_pct_pensioners + pctforeign + log_voters + educ_tertiary  + pctforeign_delta + private_price_w_s"


fit1a<- lm_robust(formula(paste0("dv ~  (pctrental + pctpublic_w_zsp)",vcovar_string1)),data=vienna,clusters=tract_key)
fit1b<- lm_robust(formula(paste0("dv ~  (pctrental + pctpublic_w_zsp)",vcovar_string3)),data=vienna,clusters=tract_key)

fit2a<- lm_robust(formula(paste0("dv ~  private_price_w_s*pctrental + pctpublic_w_zsp",vcovar_string1)),data=vienna,clusters=tract_key)
fit2b<- lm_robust(formula(paste0("dv ~  private_price_w_s*pctrental + pctpublic_w_zsp",vcovar_string2)),data=vienna,clusters=tract_key)

fit3<- lm_robust(formula(paste0("d_99 ~  private_price_w_s*(pctrental) + pctpublic_w_zsp",vcovar_string1)),data=vienna,clusters=tract_key)
fit4<- lm_robust(formula(paste0("d_02 ~  private_price_w_s*(pctrental) + pctpublic_w_zsp",vcovar_string1)),data=vienna,clusters=tract_key)
fit5<- lm_robust(formula(paste0("eu_rr_delta ~  private_price_w_s*(pctrental) + pctpublic_w_zsp",vcovar_string1)),data=vienna,clusters=tract_key)

texreg(list(
  fit1a,fit1b,fit2a,fit2b,fit3,fit4,fit5),
  include.ci=FALSE,
  caption.above=TRUE,
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  include.groups = FALSE,
  stars = c(.05),
  digits=2, scalebox=0.9, booktabs=TRUE,  use.packages = FALSE,
  caption='X',float.pos="!h")




#######################
### Figure 4 
#######################

vcovar_string4 <- "+ lab_pct_pensioners + pctforeign + log_voters + educ_tertiary  + pctforeign_delta"

# Uses weighting measure at the zahlbezirk level due to to match weighting of price variable
data <- vienna
fit1<- lm_robust(formula(paste0("dv2 ~ private_price_w*yearrenovated_w",vcovar_string4)),weights=pctpublic_w,data=data)
summary(fit1)

a<- plot_model(fit1,type="pred",terms=c("private_price_w [15:50]" ,"yearrenovated_w [1990, 2005]"),legend.title = "Renovation Year",colors="bw")+ 
  theme_minimal() + theme(legend.position="bottom") + geom_rug(data=data,aes(x = private_price_w), inherit.aes = F) +
  ggtitle("") + xlab("Rental Price Index") +ylab("Change in Vote Share, 2002-2006 (Demeaned)")  + xlim(15,45) + ylim(-.1,.1)
a

fit2<- lm_robust(formula(paste0("dv2 ~ private_price_w*sizeunder60_w",vcovar_string4)),weights=pctpublic_w,data=data)
summary(fit2)
b<- plot_model(fit2,type="pred",terms=c("private_price_w [15:50]","sizeunder60_w [.3, .6]"),legend.title = "% Apartments < 60 sq. m",colors="bw")+ 
  theme_minimal() + theme(legend.position="bottom")  + geom_rug(data=data,aes(x = private_price_w), inherit.aes = F) +ggtitle("") + 
  ylim(-.06,.06) + xlab("Rental Price Index") +ylab("")   + xlim(15,45) + ylim(-.1,.1)

ggarrange(a,b,ncol=2)



############################################################################
############################################################################
# Appendix
############################################################################
############################################################################

#######################
### Table SM1
#######################

mikro <- read_dta("./mikrozensus_extract.dta")

fit1 <- lm_robust(cost ~ public,data=mikro)
fit2 <- lm_robust(cost ~ public + A7,data=mikro)
fit3 <- lm_robust(cost ~ public + A7 + factor(BAUP1),data=mikro)
fit4 <- lm_robust(cost ~ public + A7 + factor(BAUP1) + factor(AUSS1),data=mikro)
fit5 <- felm(cost ~ public + A7 + factor(BAUP1) + factor(AUSS1) | WPOL| 0 | WPOL,data=mikro)
fit6 <- felm(cost ~ public + A7 + factor(BAUP1) + factor(AUSS1) + factor(GTYPN) + factor(GRKL) | WPOL| 0 | WPOL,data=mikro)


texreg(list(
  fit1,fit2,fit3,fit4,fit5,fit6),
  include.ci=FALSE,
  caption.above=TRUE,
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  stars = c(.05),
  include.groups = FALSE,
  digits=2, scalebox=0.9, booktabs=TRUE,  use.packages = FALSE,
  caption='',float.pos="!h")


#######################
### Table SM2 
#######################

mikro <- subset(mikro, housing_type=="Private")

fit1 <- lm_robust(cost ~ foreign,data=mikro)
fit2 <- lm_robust(cost ~ foreign + A7,data=mikro)
fit3 <- lm_robust(cost ~ foreign + A7 + factor(BAUP1),data=mikro)
fit4 <- lm_robust(cost ~ foreign + A7 + factor(BAUP1) + factor(AUSS1),data=mikro)
fit5 <- felm(cost ~ foreign + A7 + factor(BAUP1) + factor(AUSS1) | WPOL| 0 | WPOL,data=mikro)
fit6 <- felm(cost ~ foreign + A7 + factor(BAUP1) + factor(AUSS1) + factor(GTYPN) + factor(GRKL) | WPOL| 0 | WPOL,data=mikro)



texreg(list(
  fit1,fit2,fit3,fit4,fit5,fit6),
  include.ci=FALSE,
  caption.above=TRUE,
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  stars = c(.05),
  include.groups = FALSE,
  digits=2, scalebox=0.9, booktabs=TRUE,  use.packages = FALSE,
  caption='',float.pos="!h")

#######################
### Table SM3 - Public Opinion
#######################

survey2006 <- read_dta("./2006survey.dta")

fit1 <- lm_robust(vote_right_all ~ v2h3,data=survey2006)
fit2 <- lm_robust(vote_right_all ~ v2h3 + female + factor(age) + unemployed + factor(schooling),data=survey2006)
fit3 <- lm_robust(vote_right_all ~ v2h3 + contribute_culture + female + factor(age) + unemployed + factor(schooling),data=survey2006)
fit4 <- lm_robust(vote_right_all ~ v2h3 + neighbor_contact + female + factor(age) + unemployed + factor(schooling),data=survey2006)

texreg(list(
  fit1,fit2,fit3,fit4),
  include.ci=FALSE,
  caption.above=TRUE,
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  stars = c(.05),
  include.groups = FALSE,
  digits=2, scalebox=0.9, booktabs=TRUE,  use.packages = FALSE,
  caption='',float.pos="!h")


#######################
### Figure SM1 - 2011 Housing Census
#######################

par(mfrow=c(1,3))
uspan <- 1
data <- read_dta("./census2011.dta")
data <- subset(data, data$dv_pop_01 < .55) # Trim Outliers
x <- data$dv_pop_01*100
topmax <- 50
y <- data$noneu_ph_rate*100
plot(x,y,cex=.3,xlab="% of All Residents in Public Housing (2006)",ylab="% of all Non-EU Residents Living in PH (2011)",xlim=c(0,55),ylim=c(0,70),col="gray",type="n")
lo <- loess(y~x,span=.9)
xl <- seq(min(x,na.rm=T),topmax, (topmax - min(x,na.rm=T))/1000)
pred.c <- predict(lo,xl,se=T)
lines(xl, pred.c$fit, col='blue', lwd=1)
min <- pred.c$fit -  pred.c$s*1.96
max <- pred.c$fit +  pred.c$s*1.96
polygon(c(xl,rev(xl)),c(max,rev(min)),col=adjustcolor("lightblue",alpha.f=0.4),border=NA)
rug(x)


topmax <- 50
y <- data$noneu_ph_share*100
plot(x,y,cex=.3,xlab="% of All Residents in Public Housing (2006)",ylab="Share of PH Residents who are Non-EU (2011)",xlim=c(0,55),ylim=c(0,22),col="gray",type="n")
lo <- loess(y~x,span=1)
xl <- seq(min(x,na.rm=T),topmax, (topmax - min(x,na.rm=T))/1000)
pred.c <- predict(lo,xl,se=T)
lines(xl, pred.c$fit, col='blue', lwd=1)
min <- pred.c$fit -  pred.c$s*1.96
max <- pred.c$fit +  pred.c$s*1.96
polygon(c(xl,rev(xl)),c(max,rev(min)),col=adjustcolor("lightblue",alpha.f=0.4),border=NA)
rug(x)


data <- read.dta("./census2011.dta")
data <- subset(data, data$pct_noneu_06 < .17) # Trim Outliers
x <- data$pct_noneu_06*100
topmax <- 15
y <- data$noneu_ph_share*100
plot(x,y,cex=.3,xlab="% Non-EU Residents (2006)",ylab="Share of PH Residents who are Non-EU (2011)",xlim=c(0,17),ylim=c(0,22),col="gray",type="n")
lo <- loess(y~x,span=1)
xl <- seq(min(x,na.rm=T),topmax, (topmax - min(x,na.rm=T))/1000)
pred.c <- predict(lo,xl,se=T)
lines(xl, pred.c$fit, col='blue', lwd=1)
min <- pred.c$fit -  pred.c$s*1.96
max <- pred.c$fit +  pred.c$s*1.96
polygon(c(xl,rev(xl)),c(max,rev(min)),col=adjustcolor("lightblue",alpha.f=0.4),border=NA)
rug(x)



#######################
### Table SM4 - Descriptive Stats
#######################

describe <- austria %>% select(rr_share_06,rr_share_02,rr_share_99,rr_share_94,
                               euro_rr_share_99,euro_rr_share_04,dv_pop_01,pct_noneu_06,pctforeign,
                               educ_tertiary,avg_income,lab_pct_unemp, lab_pct_manufact_01,
                               welfare_cap_06, health_cap_06, education_cap_06,citizen_eu_growth_pct,vacancy_01_public)  %>% 
                                ungroup()  %>% gather(key = variable, value = value)

# External
out <- describe %>% group_by(variable) %>%  summarise(
  mean = mean(value, na.rm = T),
  sd = sd(value, na.rm = T),
  q10 = quantile(value, 0.1, na.rm = T),
  q90 = quantile(value, 0.9, na.rm = T),
)
xtable(out,digits=3)


#######################
### Figure SM2 - Density
#######################

data <- austria

data <- data %>% 
  mutate(phbin=ifelse(dv_pop_01 < .1,1,ifelse(dv_pop_01 >= .1 & dv_pop_01 < .2,2,ifelse(dv_pop_01 >= .2 & dv_pop_01 < .3,3,ifelse(dv_pop_01 >= .3,4,NA))))) %>% 
  mutate(noneubin=ifelse(pct_noneu_06 < .05,1,ifelse(pct_noneu_06 >=.05 & pct_noneu_06 < .1,2,ifelse(pct_noneu_06 >=.1,3,NA))))
out <- data %>% filter(!is.na(phbin) & !is.na(noneubin))  %>% 
  group_by(phbin,noneubin)  %>% summarize(total=100*sum(registered_06)/sum(austria$registered_06,na.rm=T))

ggplot(out, aes(phbin, noneubin, fill = total)) + geom_raster() +
  scale_fill_gradient2(low="white", high="blue", na.value="black", name="") + 
  xlab("Public Housing Bin") + ylab("Non-EU Bin") + 
  geom_text(data=out,hjust = "middle",aes(label=round(total,2))) + 
  scale_x_discrete(limits=1:4,labels= c("0-10%","10-20%","20-30%","30%+"),expand=c(0.01,.01))  + 
  scale_y_discrete(limits=1:3,labels= c("0-5%","5-10%","10%+"),expand=c(0.01,.01)) + theme_classic() + theme( axis.line = element_blank())


#######################
### Table SM5 
#######################

es <- plm(rr_share ~  pct_noneu_06*dv_pop_01*(y1999 + y2002 + y2006), 
          data=austria_p, model="within",effect="twoways",
          index=c("muni_code","year"))

coeftest(es, function(x) vcovHC(x, type = 'sss'))

#######################
### Figure SM3 
#######################

austria2 <- subset(austria,pct_noneu_06 < .14)  %>% select(d_rr_02,d_rr_99,d_eu_rr_04,dv_pop_01,pct_noneu_06)

data <-  subset(austria2,dv_pop_01<.1) %>% mutate(d_rr_02_demeaned = d_rr_02 - mean(d_rr_02,na.rm=T),
                                                  d_rr_99_demeaned = d_rr_99 - mean(d_rr_99,na.rm=T),
                                                  d_eu_rr_04_demeaned = d_eu_rr_04 - mean(d_eu_rr_04,na.rm=T))
data2 <- subset(austria2,dv_pop_01>=.1 & dv_pop_01<=.15) %>% mutate(d_rr_02_demeaned = d_rr_02 - mean(d_rr_02,na.rm=T),
                                                                    d_rr_99_demeaned = d_rr_99 - mean(d_rr_99,na.rm=T),
                                                                    d_eu_rr_04_demeaned = d_eu_rr_04 - mean(d_eu_rr_04,na.rm=T))
data3 <- subset(austria2,dv_pop_01>=.15)%>% mutate(d_rr_02_demeaned = d_rr_02 - mean(d_rr_02,na.rm=T),
                                                   d_rr_99_demeaned = d_rr_99 - mean(d_rr_99,na.rm=T),
                                                   d_eu_rr_04_demeaned = d_eu_rr_04 - mean(d_eu_rr_04,na.rm=T))
data4 <-austria2 %>% mutate(d_rr_02_demeaned = d_rr_02 - mean(d_rr_02,na.rm=T),
                            d_rr_99_demeaned = d_rr_99 - mean(d_rr_99,na.rm=T),
                            d_eu_rr_04_demeaned = d_eu_rr_04 - mean(d_eu_rr_04,na.rm=T))


th <- theme_minimal() + theme(plot.title = element_text(size = rel(1)),axis.text = element_text(size = rel(.6)),legend.title=element_text(size = rel(.65)),legend.text=element_text(size = rel(.65)),axis.title = element_text(size = rel(.8))) 

x <- data4 %>% pivot_longer(
  cols= ends_with("_demeaned"),
  names_to = "Election"
) %>% mutate(Election= str_replace(Election, "d_rr_99_demeaned", "Legislative 1995-1999")) %>%
  mutate(Election= str_replace(Election, "d_eu_rr_04_demeaned", "EU 1999-2004")) %>%
  mutate(Election= str_replace(Election, "d_rr_02_demeaned", "Legislative 1999-2002"))

a1<- ggplot(data=x,aes(x=pct_noneu_06,y=value,linetype=Election)) + geom_smooth(color="black",size=.5,method="gam") + th+ ggtitle("All Municipalities") + xlim(0,.13)  + xlab("% Non-EU 2006")+ ylab("") 

x <- data %>% pivot_longer(
  cols= ends_with("_demeaned"),
  names_to = "Election"
) %>% mutate(Election= str_replace(Election, "d_rr_99_demeaned", "Legislative 1995-1999")) %>%
  mutate(Election= str_replace(Election, "d_eu_rr_04_demeaned", "EU 1999-2004")) %>%
  mutate(Election= str_replace(Election, "d_rr_02_demeaned", "Legislative 1999-2002"))


a<- ggplot(data=x,aes(x=pct_noneu_06,y=value,linetype=Election)) + geom_smooth(color="black",size=.5,method="gam")  + th + ylab("") + xlab("% Non-EU 2006") + ggtitle("< 10% PH") + xlim(0,.13)

x <- data2 %>% pivot_longer(
  cols= ends_with("_demeaned"),
  names_to = "Election"
) %>% mutate(Election= str_replace(Election, "d_rr_99_demeaned", "Legislative 1995-1999")) %>%
  mutate(Election= str_replace(Election, "d_eu_rr_04_demeaned", "EU 1999-2004")) %>%
  mutate(Election= str_replace(Election, "d_rr_02_demeaned", "Legislative 1999-2002"))


b<- ggplot(data=x,aes(x=pct_noneu_06,y=value,linetype=Election)) + geom_smooth(color="black",size=.5,method="gam")  + th + ylab("") + xlab("% Non-EU 2006") + ggtitle("10-15% PH") + xlim(0,.13)

x <- data3 %>% pivot_longer(
  cols= ends_with("_demeaned"),
  names_to = "Election"
) %>% mutate(Election= str_replace(Election, "d_rr_99_demeaned", "Legislative 1995-1999")) %>%
  mutate(Election= str_replace(Election, "d_eu_rr_04_demeaned", "EU 1999-2004")) %>%
  mutate(Election= str_replace(Election, "d_rr_02_demeaned", "Legislative 1999-2002"))


c<- ggplot(data=x,aes(x=pct_noneu_06,y=value,linetype=Election)) + geom_smooth(color="black",size=.5,method="gam")  + th + ylab("") + xlab("% Non-EU 2006") + ggtitle("> 15% PH") + xlim(0,.13)

ggarrange(a1,a,b,c,ncol=2,nrow=2,common.legend=TRUE,legend="right")


#######################
### Table SM6 
#######################

# See Table 1 above

#######################
### Table SM7 
#######################

# See Figure 2

#######################
### Table SM8
#######################


es <- plm(rr_share ~  pct_noneu_06*dv_pop_01*after_shock , 
          data=austria_p, model="within",effect="twoways",
          index=c("muni_code","year"))

coeftest(es, function(x) vcovHC(x, type = 'sss'))


es <- plm(rr_share ~  pct_noneu_06*dv_pop_01*after_shock + state*as.integer(year), 
          data=austria_p, model="within",effect="twoways",
          index=c("muni_code","year"))

coeftest(es, function(x) vcovHC(x, type = 'sss'))


#######################
### Table SM9
#######################


for (treat in c("manual_ph3_10_20","manual_ph3_05_15")){

austria$treat2 <-  as.factor(unlist(austria[,treat]))

fit1<- lm_robust(formula(paste0("d_rr_06 ~","treat2*pct_noneu_06")),data=austria)
margins1<- summary(margins(fit1, at = list(treat2 = levels(austria$treat2)))) %>% filter(factor=="pct_noneu_06")

fit2<- lm_robust(formula(paste0("d_rr_06 ~","treat2*pct_noneu_06",control2)),data=austria)
margins2<- summary(margins(fit2, at = list(treat2 = levels(austria$treat2))))  %>% filter(factor=="pct_noneu_06")

fit3<- lm_robust(formula(paste0("d_rr_06 ~","treat2*pct_noneu_06",control3)),data=austria)
margins3<- summary(margins(fit3, at = list(treat2 = levels(austria$treat2)))) %>% filter(factor=="pct_noneu_06")

fit4<- lm_robust(formula(paste0("d_rr_06 ~","treat2*pct_noneu_06",control4)),data=austria)
margins4<- summary(margins(fit4, at = list(treat2 = levels(austria$treat2))))  %>% filter(factor=="pct_noneu_06")

print(margins1)
print(margins2)
print(margins3)
print(margins4)

}

#######################
### Table SM10
#######################

# Replace with model changing pctnoneu

austria$treat <- as.factor(austria$manual_ph3_10_20)

# FD
fit1<- lm_robust(formula(paste0("d_rr_06 ~", "treat*manual_noneu2_05")),data=austria)
summary(margins(fit1, at = list(treat = levels(austria$treat))))%>% filter(factor=="manual_noneu2_05")

fit2<- lm_robust(formula(paste0("d_rr_06 ~", "treat*manual_noneu2_05",control2)),data=austria)
summary(margins(fit2, at = list(treat = levels(austria$treat))))%>% filter(factor=="manual_noneu2_05")

fit4<- lm_robust(formula(paste0("d_rr_06 ~", "treat*manual_noneu2_05",control4)),data=austria)
summary(margins(fit4, at = list(treat = levels(austria$treat))))%>% filter(factor=="manual_noneu2_05")




#######################
### Table SM11 + marginal effects plot
#######################

fitb <- lm_robust(formula(paste0("d_rr_06 ~",spec,control4)),data=austria) 
fit1a<- lm_robust(formula(paste0("d_ovp_06 ~",spec,control2)),data=austria) 
fit1b<- lm_robust(formula(paste0("d_ovp_06 ~",spec,control4)),data=austria) 

fit2a<- lm_robust(formula(paste0("d_spo_06 ~",spec,control2)),data=austria) 
fit2b<- lm_robust(formula(paste0("d_spo_06 ~",spec,control4)),data=austria) 

fit3a<- lm_robust(formula(paste0("d_grune_06 ~",spec,control2)),data=austria)
fit3b<- lm_robust(formula(paste0("d_grune_06 ~",spec,control4)),data=austria)


texreg(list(
  fit1a,fit1b,fit2a,fit2b,fit3a,fit3b),
  include.ci=FALSE,
  caption.above=TRUE,
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  stars = c(.05),
  include.groups = FALSE,
  digits=2, scalebox=0.9, booktabs=TRUE,  use.packages = FALSE,
  caption='',float.pos="!h")


a<- plot_model(fitb,type="pred",color="bw",legend.title = "% Public Housing",terms=c("pct_noneu_06 [0, .16]","dv_pop_01 [0, .3]")) + ggtitle("Far Right") + ylab("") + xlab("") 
b<-plot_model(fit1b,type="pred",color="bw",terms=c("pct_noneu_06 [0, .16]","dv_pop_01 [0, .3]")) + ggtitle("SPOE")  + ylab("") + xlab("") 
c<-plot_model(fit2b,type="pred",color="bw",terms=c("pct_noneu_06 [0, .16]","dv_pop_01 [0, .3]")) + ggtitle("OEVP")  + ylab("") + xlab("% Non-EU") 
d<-plot_model(fit3b,type="pred",color="bw",terms=c("pct_noneu_06 [0, .16]","dv_pop_01 [0, .3]")) + ggtitle("Greens")  + ylab("")+ xlab("% Non-EU") 

ggarrange(a,b,c,d,ncol=2,nrow=2,common.legend=TRUE,legend="right")  

#######################
### Figure SM4
#######################

data <- austria
rugz <- data$dv_pop_01
par(mfrow=c(1,2))
topmax <- 50
uspan <- 1
x <- data$dv_pop_01*100
y <- data$matin_share- mean(data$matin_share,na.rm=T)
y <- y *100
plot(x,y,cex=.3,xlab="% Adults in Public Housing",ylab="Vote Share (Demeaned)",xlim=c(0,50),ylim=c(-3,3),col="gray",type="n")
lo <- loess(y~x,span=uspan)
xl <- seq(min(x,na.rm=T),topmax, (topmax - min(x,na.rm=T))/1000)
pred.c <- predict(lo,xl,se=T)
lines(xl, pred.c$fit, col='blue', lwd=1)
min <- pred.c$fit -  pred.c$s*1.96
max <- pred.c$fit +  pred.c$s*1.96
polygon(c(xl,rev(xl)),c(max,rev(min)),col=adjustcolor("lightblue",alpha.f=0.4),border=NA)
rug(rugz*100)

data <- subset(data, data$pct_noneu_06 < .18 & data$dv_pop_01 < .55) # trim outliers
data$y <- data$matin_share - mean(data$matin_share ,na.rm=T)

data$dv_pop_01 <- data$dv_pop_01 *100
data$pct_noneu_06 <- data$pct_noneu_06 *100
mod.gam <- gam(y ~ s(dv_pop_01,pct_noneu_06,sp=1.5),data=data)
vis.gam2(mod.gam,plot.type="contour",color="gray",main="",xlab="% Adults in Public Housing",ylab="% Non-EU",nCol=5)
rug(rugz*100)
rug(data$pct_noneu_06,side=2)


#######################
### Table SM12
#######################

fit1<- lm_robust(formula(paste0("d_g1_right_preb ~",spec)),data=austria)
fit2<- lm_robust(formula(paste0("d_g1_right_preb ~",spec,control4)),data=austria)
fit3<- lm_robust(formula(paste0("d_prov_rr_impl_10  ~",spec)),data=austria)
fit4<- lm_robust(formula(paste0("d_prov_rr_impl_10  ~",spec,control4)),data=austria)

texreg(list(
  fit1,fit2,fit3,fit4),
  include.ci=FALSE,
  caption.above=TRUE,
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  stars = c(.05),
  include.groups = FALSE,
  digits=2, scalebox=0.9, booktabs=TRUE,  use.packages = FALSE,
  caption='',float.pos="!h")

#######################
### Figure SM5-SM7
#######################

data2 <- subset(austria,dv_pop_01 > 0)
data2$muni_price_gap_q2 <- as.factor(xtile(data2$muni_price_gap_noassoc,2))

# Muni Price Gap
fit1 <- lm_robust(formula(paste0("d_rr_06 ~",spec,control4)), data=subset(data2,muni_price_gap_q2==1))
a <- plot_model(fit1, color="bw",type = "pred",terms = c("pct_noneu_06 [0 ,.15]","dv_pop_01 [0, .2]"),legend.title="% in PH")+ ylim(0,.1) + ggtitle("Below Median Price Gap") + xlab("% Non-EU") + ylab("Change in Vote Share 2002-2006") + theme_minimal()
fit2 <- lm_robust(formula(paste0("d_rr_06 ~",spec,control4)), data=subset(data2,muni_price_gap_q2==2))
b <- plot_model(fit2, color="bw", type = "pred",terms = c("pct_noneu_06 [0 ,.15]","dv_pop_01 [0, .2]"))+ ylim(0,.1) + ggtitle("Above Median Price Gap") + xlab("% Non-EU") + ylab("") + theme_minimal()

ggarrange(a,b,ncol=2,common.legend=TRUE,legend="bottom")


data2$price_growth_q2 <- as.factor(ntile(data2$yhat_priv_03_01,2))

# Muni Price Gap
fit1 <- lm_robust(formula(paste0("d_rr_06 ~",spec,control4)), data=subset(data2,price_growth_q2==1))
a <- plot_model(fit1, color="bw",type = "pred",terms = c("pct_noneu_06 [0 ,.15]","dv_pop_01 [0, .2]"),legend.title="% in PH")+ ylim(0,.1) + ggtitle("Below Median Price Growth") + xlab("% Non-EU") + ylab("Change in Vote Share 2002-2006") + theme_minimal()
fit2 <- lm_robust(formula(paste0("d_rr_06 ~",spec,control4)), data=subset(data2,price_growth_q2==2))
b <- plot_model(fit2, color="bw", type = "pred",terms = c("pct_noneu_06 [0 ,.15]","dv_pop_01 [0, .2]"))+ ylim(0,.1) + ggtitle("Above Median Price Growth") + xlab("% Non-EU") + ylab("") + theme_minimal()

ggarrange(a,b,ncol=2,common.legend=TRUE,legend="bottom")

data2$elig_growth_q2 <- as.factor(xtile(data2$citizen_eu_growth_pct,2))
# Muni Price Gap
fit1 <- lm_robust(formula(paste0("d_rr_06 ~",spec,control4)), data=subset(data2,elig_growth_q2==1))
a <- plot_model(fit1, color="bw",type = "pred",terms = c("pct_noneu_06 [0 ,.15]","dv_pop_01 [0, .2]"),legend.title="% in PH")+ ylim(0,.1) + ggtitle("Below Median Eligible Growth") + xlab("% Non-EU") + ylab("Change in Vote Share 2002-2006") + theme_minimal()
fit2 <- lm_robust(formula(paste0("d_rr_06 ~",spec,control4)), data=subset(data2,elig_growth_q2==2))
b <- plot_model(fit2, color="bw", type = "pred",terms = c("pct_noneu_06 [0 ,.15]","dv_pop_01 [0, .2]"))+ ylim(0,.1) + ggtitle("Above Median Eligible Growth") + xlab("% Non-EU") + ylab("") + theme_minimal()

ggarrange(a,b,ncol=2,common.legend=TRUE,legend="bottom")

fit2 <- lm_robust(formula(paste0("d_rr_06 ~dv_pop_01*pct_noneu_06*construction_ratio",control4)), data=data2)
a<- plot_model(fit2, color="bw",conf.lvl=.9, type = "pred",terms = c("pct_noneu_06 [0 ,.15]","dv_pop_01 [0, .25]","construction_ratio [.05]"))+ ylim(-.01,.1) + ggtitle("Below Median Construction Level") + xlab("% Non-EU") + ylab("") + theme_minimal()
b<- plot_model(fit2, color="bw",conf.lvl=.9, type = "pred",terms = c("pct_noneu_06 [0 ,.15]","dv_pop_01 [0, .25]","construction_ratio [.19]"))+ ylim(-.01,.1) + ggtitle("Above Median Construction Level") + xlab("% Non-EU") + ylab("") + theme_minimal()
ggarrange(a,b,ncol=2,common.legend=TRUE,legend="bottom")

#######################
### B10 - SM13
#######################

austria$f99_3 <- as.factor(xtile(austria$rr_share_99,3))

fit1<- lm_robust(formula(paste0("d_rr_06 ~ ",spec,control4)),data=subset(austria,f99_3==1))
fit2<- lm_robust(formula(paste0("d_rr_06 ~ ",spec,control4)),data=subset(austria,f99_3==2))
fit3<- lm_robust(formula(paste0("d_rr_06 ~ ",spec,control4)),data=subset(austria,f99_3==3))

fit4<- lm_robust(formula(paste0("d_rr_06 ~ ",spec2,control4)),data=subset(austria,f99_3==1))
fit5<- lm_robust(formula(paste0("d_rr_06 ~ ",spec2,control4)),data=subset(austria,f99_3==2))
fit6<- lm_robust(formula(paste0("d_rr_06 ~ ",spec2,control4)),data=subset(austria,f99_3==3))

texreg(list(fit1,fit2,fit3,fit4,fit5,fit6), include.ci = FALSE,
       caption.above=TRUE,
       include.rsquared = FALSE,
       include.adjrs = FALSE,
       include.groups = FALSE,
       digits=2, scalebox=0.75, booktabs=TRUE,  use.packages = FALSE,
       caption='X',float.pos="!h")


#######################
### TABLE SM14
#######################


cor(austria$pct_noneu_06,austria$pct_eu2004,use='complete.obs')

austria$combined <- austria$pct_eu2004 + austria$pct_noneu_06

fit1<- lm_robust(formula(paste0("d_rr_06 ~ pct_eu2004 + ",spec)),data=austria)
fit2<- lm_robust(formula(paste0("d_rr_06 ~ pct_eu2004 +",spec,control2)),data=austria)
fit3<- lm_robust(formula(paste0("d_rr_06 ~ pct_eu2004 +",spec,control4)),data=austria)

spec3 <- "combined*dv_pop_01"
fit4<- lm_robust(formula(paste0("d_rr_06 ~",spec3)),data=austria)
fit5<- lm_robust(formula(paste0("d_rr_06 ~ ",spec3,control2)),data=austria)
fit6<- lm_robust(formula(paste0("d_rr_06 ~ ",spec3,control4)),data=austria)


# All as regression table
texreg(list(fit1,fit2,fit3,fit4,fit5,fit6), include.ci = FALSE,
       caption.above=TRUE,
       include.rsquared = FALSE,
       include.adjrs = FALSE,
       include.groups = FALSE,
       stars=.05,
       digits=2, scalebox=0.75, booktabs=TRUE,  use.packages = FALSE,
       caption='X',float.pos="!h")




#######################
### Table SM15
#######################

describe <- vienna %>% 
  select(yearrenovated_w,pctrental,pctrental,sizeunder60_w,
         private_price_w,pctforeign_public,pctforeign_private,
         avgincome_06,lab_pct_active,farright_share2006,farright_share2002,
         farright_share1999,eu_rr_share1999, eu_rr_share2004,lab_pct_pensioners,
         pctpublic_w_zsp,pctforeign,educ_tertiary,pctforeign_delta,pct_noneu_06)  %>% 
  ungroup()  %>% gather(key = variable, value = value)


# External
out <- describe %>% group_by(variable) %>%  summarise(
  mean = mean(value, na.rm = T),
  sd = sd(value, na.rm = T),
  q10 = quantile(value, 0.1, na.rm = T),
  q90 = quantile(value, 0.9, na.rm = T),
)
xtable(out,digits=3)


#######################
### Table SM16
#######################

es1 <- felm(farright_share ~ (pctpublic_w+ pctrental)*(y1999 + y2002 + y2006)  | sprengel2 | 0 | tract_key,data=vienna_p)
es2 <- felm(farright_share ~ (pctpublic_w + pctrental)*(y1999 + y2002 + y2006) | sprengel2 | 0 | tract_key,data=subset(vienna_p, private_price_w_q3 ==1))
es3 <- felm(farright_share ~ (pctpublic_w + pctrental)*(y1999 + y2002 + y2006) | sprengel2 | 0 | tract_key,data=subset(vienna_p, private_price_w_q3 ==2))
es4 <- felm(farright_share ~ (pctpublic_w + pctrental)*(y1999 + y2002 + y2006) | sprengel2 | 0 | tract_key,data=subset(vienna_p, private_price_w_q3 ==3))

texreg(list(
  es1,es2,es3,es4),
  include.ci=FALSE,
  caption.above=TRUE,
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  include.groups = FALSE,
  stars = c(.05),
  digits=2, scalebox=0.9, booktabs=TRUE,  use.packages = FALSE,
  caption='X',float.pos="!h")

#######################
### Table SM17
#######################

es0 <- felm(farright_share ~ pctpublic_w_zsp*aftershock +(pctrental)*aftershock | sprengel2 | 0 | tract_key,data=vienna_p)
es1 <- felm(farright_share ~ pctpublic_w_zsp*aftershock +(pctrental)*aftershock | sprengel2 | 0 | tract_key,data=subset(vienna_p, private_price_w_q3==1))
es2 <- felm(farright_share ~ pctpublic_w_zsp*aftershock +(pctrental)*aftershock | sprengel2 | 0 | tract_key,data=subset(vienna_p, private_price_w_q3==2))
es3 <- felm(farright_share ~ pctpublic_w_zsp*aftershock +(pctrental)*aftershock | sprengel2 | 0 | tract_key,data=subset(vienna_p,  private_price_w_q3==3))

texreg(list(
  es0,es1,es2,es3),
  include.ci=FALSE,
  caption.above=TRUE,
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  include.groups = FALSE,
  stars = c(.05),
  digits=2, scalebox=0.9, booktabs=TRUE,  use.packages = FALSE,
  caption='X',float.pos="!h")


#######################
### Table SM18
#######################

# See results computed using Table 2 code 

#######################
### Table SM19
#######################

fit0b<- lm_robust(formula(paste0("dv ~  as.factor(private_price_w_q3)*pctrental + pctpublic_w_zsp",vcovar_string2)),data=vienna,clusters=tract_key)
fit1b<- lm_robust(formula(paste0("dv ~  (rental_scaled + pctpublic_w_zsp)",vcovar_string3)),data=vienna,clusters=tract_key)
fit2b<- lm_robust(formula(paste0("dv ~  as.factor(private_price_w_q3)*rental_scaled + pctpublic_w_zsp",vcovar_string2)),data=vienna,clusters=tract_key)

texreg(list(
  fit0b,fit1b,fit2b),
  include.ci=FALSE,
  caption.above=TRUE,
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  include.groups = FALSE,
  stars = c(.05),
  digits=2, scalebox=0.9, booktabs=TRUE,  use.packages = FALSE,
  caption='X',float.pos="!h")


#######################
### Table SM20
#######################

data <-vienna

fit1<- lm_robust(formula(paste0("dv2 ~ private_price_w*yearrenovated_w",vcovar_string4)),weights=pctpublic_w,data=data)
fit2<- lm_robust(formula(paste0("dv2 ~ private_price_w*sizeunder60_w",vcovar_string4)),weights=pctpublic_w,data=data)
fit3<- lm_robust(formula(paste0("dv2 ~  private_price_w*factor(yearrenovated_w_q3)",vcovar_string4)),weights=pctpublic_w,data=data)


texreg(list(
  fit1,fit2,fit3),
  include.ci=FALSE,
  caption.above=TRUE,
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  include.groups = FALSE,
  stars = c(.05),
  digits=3, scalebox=0.9, booktabs=TRUE,  use.packages = FALSE,
  caption='X',float.pos="!h")

#######################
### Figure SM9 
#######################

# Change to median

data <-subset(vienna,pctpublic_w >= quantile(vienna$pctpublic_w,.5,na.rm=T))

fit1<- lm_robust(formula(paste0("dv2 ~ private_price_w*yearrenovated_w",vcovar_string4)),data=data)
summary(fit1)

a<- plot_model(fit1,type="pred",terms=c("private_price_w [15:50]" ,"yearrenovated_w [1990, 2005]"),legend.title = "Renovation Year",colors="bw")+ 
  theme_minimal() + theme(legend.position="bottom") + geom_rug(data=data,aes(x = private_price_w), inherit.aes = F) +
  ggtitle("") + xlab("Rental Price Index") +ylab("Demeaned Change in Vote Share, 2002-2006")  + xlim(15,45) + ylim(-.1,.1)
a

fit2<- lm_robust(formula(paste0("dv2 ~ private_price_w*sizeunder60_w",vcovar_string4)),data=data)
summary(fit2)
b<- plot_model(fit2,type="pred",terms=c("private_price_w [15:50]","sizeunder60_w [.3, .6]"),legend.title = "% Apartments < 60 sq. m",colors="bw")+ 
  theme_minimal() + theme(legend.position="bottom")  + geom_rug(data=data,aes(x = private_price_w), inherit.aes = F) +ggtitle("") + 
  ylim(-.06,.06) + xlab("Rental Price Index") +ylab("")   + xlim(15,45) + ylim(-.1,.1)

ggarrange(a,b,ncol=2)

#######################
### Figure SM10 
#######################

par(mfrow=c(1,3))

data <- vienna
data <- subset(data,data$pctpublic_w_zsp > .25 & data$pctforeign_public < .25)

plot(data$pctforeign_public*100,data$dv,cex=.3,xlab="% of Foreign-born PH Occupants",ylab="Delta Vote Share",main="25% Threshold", col="gray")
x <- data$pctforeign_public*100
y <- data$dv
lo <- loess(y~x,span=1)
xl <- seq(min(x,na.rm=T),max(x,na.rm=T), (max(x,na.rm=T) - min(x,na.rm=T))/1000)
pred.c <- predict(lo,xl,se=T)
lines(xl, pred.c$fit, col='black', lwd=1)
min <- pred.c$fit -  pred.c$s*1.96
max <- pred.c$fit +  pred.c$s*1.96
polygon(c(xl,rev(xl)),c(max,rev(min)),col=adjustcolor("gray",alpha.f=0.4),border="white")

data <- vienna
data <- subset(data,data$pctpublic_w_zsp > .5 & data$pctforeign_public < .25)

plot(data$pctforeign_public*100,data$dv,cex=.3,xlab="% of Foreign-born PH Occupants",ylab="Delta Vote Share",main="50% Threshold", col="gray")
x <- data$pctforeign_public*100
y <- data$dv
lo <- loess(y~x,span=1)
xl <- seq(min(x,na.rm=T),max(x,na.rm=T), (max(x,na.rm=T) - min(x,na.rm=T))/1000)
pred.c <- predict(lo,xl,se=T)
lines(xl, pred.c$fit, col='black', lwd=1)
min <- pred.c$fit -  pred.c$s*1.96
max <- pred.c$fit +  pred.c$s*1.96
polygon(c(xl,rev(xl)),c(max,rev(min)),col=adjustcolor("gray",alpha.f=0.4),border="white")


data <- vienna
data <- subset(data,data$pctpublic_w_zsp > .75 & data$pctforeign_public < .25)

plot(data$pctforeign_public*100,data$dv,cex=.3,xlab="% of Foreign-born PH Occupants",ylab="Delta Vote Share",main="75% Threshold", col="gray")
x <- data$pctforeign_public*100
y <- data$dv
lo <- loess(y~x,span=1)
xl <- seq(min(x,na.rm=T),max(x,na.rm=T), (max(x,na.rm=T) - min(x,na.rm=T))/1000)
pred.c <- predict(lo,xl,se=T)
lines(xl, pred.c$fit, col='black', lwd=1)
min <- pred.c$fit -  pred.c$s*1.96
max <- pred.c$fit +  pred.c$s*1.96
polygon(c(xl,rev(xl)),c(max,rev(min)),col=adjustcolor("gray",alpha.f=0.4),border="white")


#######################
### Table SM21
#######################


vienna_somep <- subset(vienna,pctpublic_w_zsp > 0)

covar_string3 <- " + rental_scaled+ lab_pct_pensioners + log_voters + private_price_w + educ_tertiary"

fit1<- lm_robust(formula(paste0("dv ~ pctforeign_public",covar_string3)),weights=pctpublic_w,data=vienna_somep,clusters=tract_key)
fit2<- lm_robust(formula(paste0("dv ~ pctforeign_public +pctforeign_private",covar_string3)),weights=pctpublic_w,data=vienna_somep,clusters=tract_key)
fit3<- lm_robust(formula(paste0("dv ~ pctforeign_public*pctforeign_private",covar_string3)),weights=pctpublic_w,data=vienna_somep,clusters=tract_key)

fit4<- lm_robust(formula(paste0("dv ~ pctforeign_public",covar_string3)),data=subset(vienna_somep,pctpublic_w > quantile(vienna$pctpublic_w,.66,na.rm=T)),clusters=tract_key)
fit5<- lm_robust(formula(paste0("dv ~ pctforeign_public +pctforeign_private",covar_string3)),data=subset(vienna_somep,pctpublic_w > quantile(vienna$pctpublic_w,.66,na.rm=T)),clusters=tract_key)
fit6<- lm_robust(formula(paste0("dv ~ pctforeign_public*pctforeign_private",covar_string3)),data=subset(vienna_somep,pctpublic_w > quantile(vienna$pctpublic_w,.66,na.rm=T)),clusters=tract_key)


texreg(list(
  fit1,fit2,fit3,fit4,fit5,fit6),
  include.ci=FALSE,
  caption.above=TRUE,
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  include.groups = FALSE,
  stars=c(.05),
  digits=2, scalebox=0.9, booktabs=TRUE,  use.packages = FALSE,
  caption='X',float.pos="!h")

