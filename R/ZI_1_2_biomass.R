#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Content ############################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data preparation
# Statistics
## 1 Experiment 1: Design vs. standard seedmix
## 2 Experiment 2: Clean vs. demolition bricks
# Plotten
## 1 Experiment 1: Design vs. standard seedmix
## 2 Experiment 2: Clean vs. demolition bricks
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(installr);updateR(browse_news=F, install_R=T, copy_packages = T,copy_Rprofile.site = T,keep_old_packages = T, update_packages = T)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Data preparation ############################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(tidyverse);setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/waste_bricks_for_restoration/data/processed")

edata <- read_table2("main_site.txt", col_names = T, na = "na", col_types = 
                       cols(
                         .default = col_double(),
                         plot = col_factor(),
                         block = col_factor(),
                         pump = col_factor(),
                         position = col_factor(),
                         brickType = col_factor(levels = c("clean","demolition")),
                         seedmix = col_factor(levels = c("standard","robust","intermediate","vigorous")),
                         brickRatio = col_factor(levels = c("5","30")),
                         acid = col_factor(levels = c("0","1"))
                       )        
)
###Create variables
edata$conf.low <- c(1:160);
edata$conf.high <- c(1:160)
edata <- mutate(edata, biomass = grassMass + herbMass)
edata$grassRatio <- edata$grassMass/edata$biomass
edata$f.watering <- factor(edata$watering);
edata$q.watering <- edata$watering^2
###Recode levels
#edata$brickRatio <- dplyr::recode(edata$brickRatio,
 #                                 "5" = "5% bricks", "30" = "30% bricks")
edata$acid <- dplyr::recode(edata$acid,
                            "0" = "Control", "1" = "Acid")
edata$f.watering <- dplyr::recode(edata$f.watering,
                                  "0.5" = "Dry", "1" = "Medium dry", "2" = "Medium moist", "3" = "Moist")
edata$brickType <- dplyr::recode(edata$brickType,
                                 "abort" = "Demolition", "return" = "Clean")
edata$seedmix <- dplyr::recode(edata$seedmix,
                               "RSM" = "Standard","robust" = "Robust", "intermediate" = "Intermediate", "vigorous" = "Vigorous")
###Brick type data frame
edataBricktype <- edata %>% 
  filter(acid == "Acid" & seedmix == "Robust" | seedmix == "Vigorous" & acid == "Acid") %>%
  filter(brickRatio == "5" & brickType == "Clean" | brickRatio == "30")# %>%
edataBricktype$treatment <- fct_cross(edataBricktype$brickType, edataBricktype$brickRatio)
edataBricktype$treatment <- fct_recode(edataBricktype$treatment, "5% clean" = "Clean:5", "30% clean" = "Clean:30","30% demolition" = "Demolition:30")
edataBricktype$seedmix <- factor(edataBricktype$seedmix)
###Main data frame
edata <- filter(edata, brickType == "Clean");


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(ggplot2)
library(ggbeeswarm)
library(car); #Anova(); vif(): variance inflation factors --> checking for dependence (Collinearity) (below 3 is ok)
library(nlme); #use for vif()
library(lme4)
library(lmerTest)
library(DHARMa)
#library(vcd)
library(sjPlot) #plot random effects
library(MuMIn)
library(emmeans)
library(ggeffects)


## 1 Standard seedmix #############################################################################################

####a Data exploration####
#####Homogenity of variances? Find potential effects and interactions
par(mfrow = c(2,2));plot(biomass ~ brickRatio, edata);plot(biomass ~ acid, edata);plot(biomass ~ f.watering, edata);plot(biomass ~ seedmix, edata)
par(mfrow = c(2,2));plot(biomass ~ vegCov13, edata);plot(biomass ~ grassRatio, edata);
par(mfrow = c(2,2));plot(biomass ~ position, edata);plot(biomass ~ pump, edata);plot(biomass ~ block, edata)
#2way: brickRatio:acid eher nicht
ggplot(edata,aes(brickRatio, biomass, color = acid)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#2way: brickRatio:watering nein
ggplot(edata,aes(f.watering, biomass,color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#2way: brickRatio:seedmix nein
ggplot(edata,aes(seedmix, biomass, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#2way: seedmix:watering vllt, aber nicht wie erwartet
ggplot(edata,aes(f.watering, biomass,color=seedmix)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#2way: acid:watering vllt by medium dry (ABER warum?)
ggplot(edata,aes(f.watering, biomass, color = acid)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#3way: brickRatio:acid:seedmix ja
ggplot(edata,aes(brickRatio, biomass, color = acid)) + facet_grid(~seedmix) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#3way: brickRatio:watering:seedmix vllt aber anders als erwartet
ggplot(edata,aes(f.watering, biomass, color = brickRatio)) + facet_grid(~seedmix) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#3way: brickRatio:acid:watering im mittleren Bereich ABER warum?
ggplot(edata,aes(brickRatio, biomass, color = acid)) + facet_grid(~f.watering) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#4way: focus only on standard and intermediate --> no effect under dry conditions
ggplot(edata,aes(f.watering, biomass, color = brickRatio, shape = acid)) + facet_grid(~seedmix) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
### interactions with block
ggplot(edata,aes(brickRatio, biomass,fill=acid)) + geom_boxplot() + facet_wrap(~ block) + geom_quasirandom(dodge.width=.7)
ggplot(edata,aes(block, biomass,fill=f.watering)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
ggplot(edata,aes(block, biomass,fill=seedmix)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#####Outliers and lots of zeroes and transformations?
par(mfrow = c(2,2));dotchart((edata$biomass), groups=factor(edata$brickRatio), main="Cleveland dotplot");dotchart((edata$biomass), groups=factor(edata$acid), main="Cleveland dotplot");dotchart((edata$biomass), groups=factor(edata$watering), main="Cleveland dotplot");dotchart((edata$biomass), groups=factor(edata$seedmix), main="Cleveland dotplot")
dotchart((edata$biomass), groups=factor(edata$grassRatio), main="Cleveland dotplot")
par(mfrow=c(1,1));boxplot(edata$biomass,ylim=c(0,45));#identify(rep(1,length(edata$biomass)),edata$biomass, labels = c(edata$no))
par(mfrow = c(2,2));plot(table(log(edata$biomass)),type = "h",xlab = "Observed values", ylab = "Frequency");plot(table((edata$biomass)),type = "h",xlab = "Observed values", ylab = "Frequency")
ggplot(edata,aes(biomass))+geom_density()
####b Model building####
m2a1 <- lmer((biomass) ~ f.watering*seedmix + (1|pump/block), edata, REML = F);VarCorr(m2a1)
m2a2 <- lmer((biomass) ~ f.watering*seedmix + (1|block), edata, REML = F);VarCorr(m2a2)
m2a3 <- lmer((biomass) ~ f.watering*seedmix + (1|position/block), edata, REML = F);VarCorr(m2a3)
#-> random structure: only block
m2c <- lmer(log(biomass) ~ (brickRatio + acid + f.watering + seedmix)^2 +  
              brickRatio:f.watering:seedmix + brickRatio:acid:seedmix + 
              brickRatio:acid:f.watering:seedmix + 
              (1|block), edata, REML = F);isSingular(m2c);simulationOutput <- simulateResiduals(m2c, plot=T)
#4w-model: ok but not perfect
m2d <- lmer(log(biomass) ~ (brickRatio + acid + f.watering + seedmix) +
              brickRatio:acid + brickRatio:f.watering + brickRatio:seedmix + 
              f.watering:seedmix + acid:seedmix +
              brickRatio:f.watering:seedmix + brickRatio:acid:seedmix + 
              (1|block), edata, REML = F);isSingular(m2d);simulationOutput <- simulateResiduals(m2d, plot=T)
#full 3w-model: worse
m2e <- lmer(log(biomass) ~ (brickRatio + acid + f.watering + seedmix) +
              brickRatio:acid + brickRatio:f.watering + brickRatio:seedmix + 
              f.watering:seedmix +
              brickRatio:f.watering:seedmix + 
              (1|block), edata, REML = F);isSingular(m2e);simulationOutput <- simulateResiduals(m2e, plot=T)
#3w-model brick:water:mix: worse than 4w-model
m2f <- lmer(log(biomass) ~ (brickRatio + acid + f.watering + seedmix) +  
              brickRatio:acid + brickRatio:f.watering + brickRatio:seedmix + 
              f.watering:seedmix + acid:seedmix + 
              brickRatio:acid:seedmix + 
              (1|block), edata, REML = F);isSingular(m2f);simulationOutput <- simulateResiduals(m2f, plot=T);VarCorr(m2f);Anova(m2f,type=3)
#3w-model brick:acid:mix: worse than 4w-model
m2g <- lmer(log(biomass) ~ (brickRatio + acid + f.watering + seedmix) + 
              brickRatio:acid + brickRatio:f.watering + brickRatio:seedmix + 
              f.watering:seedmix + 
              (1|block), edata, REML = F);isSingular(m2g);simulationOutput <- simulateResiduals(m2g, plot=T);
#2w-model: worse than 4w-model
AIC(m2c,m2d,m2e,m2f,m2g) # 1.m2c, 2.m2f --> m2f (auch wenn Modellkritik eigentlich die Interaktion acid:f.watering forder, ABER ökologisch Quatsch)
(re.effects <- plot_model(m2f, type = "re", show.values = TRUE))
rm(m2a1,m2a2,m2a3,m2c,m2d,m2e,m2g)
simulationOutput <- simulateResiduals(m2f, plot=F)
par(mfrow=c(2,2));
plotResiduals(main = "brickRatio", simulationOutput$scaledResiduals, edata$brickRatio);
plotResiduals(main = "acid", simulationOutput$scaledResiduals, edata$acid);
plotResiduals(main = "f.watering", simulationOutput$scaledResiduals, edata$f.watering);
plotResiduals(main = "seedmix", simulationOutput$scaledResiduals,edata$seedmix);
par(mfrow=c(2,2));
plotResiduals(main = "position", simulationOutput$scaledResiduals, edata$position);
plotResiduals(main = "pump", simulationOutput$scaledResiduals, edata$pump);
plotResiduals(main = "block", simulationOutput$scaledResiduals, edata$block);
par(mfrow=c(2,2));
plotResiduals(sub = "vegCov13", simulationOutput$scaledResiduals, edata$vegCov13, quantreg = F);
plotResiduals(sub = "grassMass", simulationOutput$scaledResiduals, edata$grassMass, quantreg = F);
plotResiduals(sub = "estRate", simulationOutput$scaledResiduals, edata$estRate, quantreg = F)
#single variable are ok
###c Model output####
VarCorr(m2f)
Anova(m2f, type = 3)
summary(m2f) #check the effect of random factor
r.squaredGLMM(m2f)
(emm <- emmeans(m2f, revpairwise ~ seedmix | f.watering, type = "response")); plot(emm, comparison = T)
contrast(emmeans(m2f, ~ seedmix * f.watering, type = "response"), "trt.vs.ctrl", ref = 1)
(emm <- emmeans(m2f, revpairwise ~ brickRatio * acid | seedmix, type="response")); plot(emm,comparison = T)
(emm <- emmeans(m2f, revpairwise ~ brickRatio | f.watering, type = "response")); plot(emm, comparison = T)


## 2 Demolition bricks #############################################################################################

####a Data exploration####
par(mfrow = c(2,2));
plot(biomass ~ brickType, edataBricktype);plot(biomass ~ brickRatio, edataBricktype);plot(biomass ~ seedmix, edataBricktype);plot(biomass ~ pump, edataBricktype);
plot(biomass ~ block, edataBricktype)
ggplot(edataBricktype,aes(brickType, biomass,color=brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#2way: brickType:brickRatio vllt
ggplot(edataBricktype,aes(f.watering, biomass,color=brickType)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#2way: brickType:f.watering nicht sinnvoll zu interpretieren ABER könnte sein
ggplot(edataBricktype,aes(seedmix, biomass,color=brickType)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#2way: brickType:seedmix nicht sinnvoll zu interpretieren ABER eher nicht
ggplot(edataBricktype,aes(brickType, biomass,color=brickRatio)) + facet_grid(~f.watering) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#3way:brickType:brickRatio:watering eher bei trockenem Teil
ggplot(edataBricktype,aes(brickType, biomass,color=brickRatio)) + facet_grid(~seedmix) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#3way:brickRatio:brickType:seedmix eher bei demolition
par(mfrow = c(2,2));dotchart((edataBricktype$biomass), groups=factor(edataBricktype$brickType), main="Cleveland dotplot");dotchart((edataBricktype$biomass), groups=factor(edataBricktype$watering), main="Cleveland dotplot");dotchart((edataBricktype$biomass), groups=factor(edataBricktype$brickRatio), main="Cleveland dotplot");dotchart((edata$biomass), groups=factor(edata$seedmix), main="Cleveland dotplot")
dotchart((edataBricktype$biomass), groups=factor(edataBricktype$grassRatio), main="Cleveland dotplot")
par(mfrow=c(1,1));boxplot(edataBricktype$biomass);#identify(rep(1,length(edata$biomass)),edata$biomass, labels = c(edata$no))
plot(table(log(edataBricktype$biomass)),type = "h",xlab = "Observed values", ylab = "Frequency")
ggplot(edataBricktype,aes(biomass))+geom_density()
####b Model building####
m3a <- lmer((biomass) ~ f.watering * brickType + (1|pump/block), edataBricktype, REML = F);VarCorr(m3a)
m3b <- lmer((biomass) ~ f.watering * brickType + (1|block), edataBricktype, REML = F);VarCorr(m3b)
#-> random structure: only block
m3c <- lmer(log(biomass) ~ (f.watering + brickRatio + brickType + seedmix)^2 +  
              seedmix:brickRatio:brickType + f.watering:brickRatio:brickType + 
              (1|block), edataBricktype, REML = F); isSingular(m3c); simulationOutput <- simulateResiduals(m3c, plot=T)
#Modell ok, vif() funktioniert nicht
m3d <- lmer(log(biomass) ~ (f.watering + brickRatio + brickType)^2 + seedmix +  
              f.watering:brickRatio:brickType + 
              (1|block), edataBricktype, REML = F); isSingular(m3d); simulationOutput <- simulateResiduals(m3d, plot=T)
m3e <- lmer(log(biomass) ~ (f.watering + brickRatio + brickType)^2 + seedmix + 
              (1|block), edataBricktype, REML = F); isSingular(m3e); simulationOutput <- simulateResiduals(m3e, plot=T)
#Modell ok
m3f <- lmer(log(biomass) ~ f.watering + brickRatio + brickType + seedmix + 
              brickType:brickRatio + 
              (1|block), edataBricktype, REML = F); isSingular(m3f); simulationOutput <- simulateResiduals(m3f, plot=T)
AIC(m3c,m3d,m3e,m3f);
rm(m3a,m3b,m3c,m3d,m3e) #-->m3f ABER m3d enthält watering
simulationOutput <- simulateResiduals(m3f, plot=F)
par(mfrow=c(2,2));
plotResiduals(main = "brickType", simulationOutput$scaledResiduals, edataBricktype$brickType);
plotResiduals(main = "f.watering", simulationOutput$scaledResiduals, edataBricktype$f.watering);
plotResiduals(main = "seedmix", simulationOutput$scaledResiduals, edataBricktype$seedmix);
plotResiduals(main = "brickRatio", simulationOutput$scaledResiduals, edataBricktype$brickRatio);
par(mfrow=c(2,2));
plotResiduals(main = "position", simulationOutput$scaledResiduals, edataBricktype$position);
plotResiduals(main = "pump", simulationOutput$scaledResiduals, edataBricktype$pump);
plotResiduals(main = "block", simulationOutput$scaledResiduals, edataBricktype$block);
par(mfrow=c(2,2));
plotResiduals(sub = "vegCov13", simulationOutput$scaledResiduals, edataBricktype$vegCov13, quantreg = F);
plotResiduals(sub = "grassMass", simulationOutput$scaledResiduals, edataBricktype$grassMass, quantreg = F);
plotResiduals(sub = "estRate", simulationOutput$scaledResiduals, edataBricktype$estRate, quantreg = F)
###c Model output####
VarCorr(m3f)
Anova(m3f, type = 3) #only  brickType sig. but not important
r.squaredGLMM(m3f)
(emm <- emmeans(m3d, pairwise ~ brickRatio * brickType | f.watering));plot(emm, comparison = T); 
(emm <- emmeans(m3f, pairwise ~ brickType|brickRatio, typ = "response"));plot(emm, comparison = T);
###d New model####
m4a <- lmer(log(biomass) ~ f.watering + treatment + seedmix + 
              treatment:f.watering +
              (1|block), edataBricktype, REML = F); isSingular(m4a); simulationOutput <- simulateResiduals(m4a, plot=T)
m4b <- lmer(log(biomass) ~ f.watering + treatment + seedmix +
              (1|block), edataBricktype, REML = F); isSingular(m4b); simulationOutput <- simulateResiduals(m4b, plot=T)
par(mfrow=c(2,2));
plotResiduals(main = "treatment", simulationOutput$scaledResiduals, edataBricktype$treatment);
plotResiduals(main = "f.watering", simulationOutput$scaledResiduals, edataBricktype$f.watering);
plotResiduals(main = "seedmix", simulationOutput$scaledResiduals, edataBricktype$seedmix);
par(mfrow=c(2,2));
plotResiduals(main = "position", simulationOutput$scaledResiduals, edataBricktype$position);
plotResiduals(main = "pump", simulationOutput$scaledResiduals, edataBricktype$pump);
plotResiduals(main = "block", simulationOutput$scaledResiduals, edataBricktype$block);
par(mfrow=c(2,2));
plotResiduals(sub = "vegCov13", simulationOutput$scaledResiduals, edataBricktype$vegCov13, quantreg = F);
plotResiduals(sub = "grassMass", simulationOutput$scaledResiduals, edataBricktype$grassMass, quantreg = F);
plotResiduals(sub = "estRate", simulationOutput$scaledResiduals, edataBricktype$estRate, quantreg = F)
VarCorr(m4b)
AIC(m4a,m4b) # --> m4b
Anova(m4b, type=2)
r.squaredGLMM(m4b)
(emm <- emmeans(m4b, revpairwise ~ treatment, typ = "response")); plot(emm, comparison = T)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Plotten #####################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(ggplot2);
library(visreg);
library(ggeffects);
library(ggbeeswarm);
themeMB <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size=10, color = "black"),
    axis.line.y = element_line(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

## 1 Standard seedmix #############################################################################################

### 3w seedmix:brickRatio:acid -----------------------------------------------------------------------------------------------------------------------
pdata <- ggemmeans(m2f, terms = c("brickRatio","acid","seedmix"), type = "fe")
pdata <- rename(pdata, biomass = predicted, brickRatio = x, acid = group, seedmix = facet);
####Extract hlines and scale
#Standard <-  pdata %>% filter(seedmix=="Standard") %>% mutate(st.biomass = biomass-17.71116) %>% mutate(st.conf.low = conf.low-17.71116) %>% mutate(st.conf.high = conf.high-17.71116)
#Robust <-  pdata %>% filter(seedmix=="Robust") %>% mutate(st.biomass = biomass-14.15029) %>% mutate(st.conf.low = conf.low-14.15029) %>% mutate(st.conf.high = conf.high-14.15029)
#Intermediate <-  pdata %>% filter(seedmix=="Intermediate") %>% mutate(st.biomass = biomass-14.80358) %>% mutate(st.conf.low = conf.low-14.80358) %>% mutate(st.conf.high = conf.high-14.80358)
#Vigorous <-  pdata %>% filter(seedmix=="Vigorous") %>% mutate(st.biomass = biomass-13.45494) %>% mutate(st.conf.low = conf.low-13.45494) %>% mutate(st.conf.high = conf.high-13.45494)
#pdata <- full_join(Standard,Robust)  
#pdata <- full_join(pdata,Intermediate)  
#pdata <- full_join(pdata,Vigorous)  
meandata <- filter(pdata, acid=="Control" & brickRatio=="5")
#Standard <-  edata %>% filter(seedmix=="Standard") %>% mutate(st.biomass = biomass-17.71116)
#Robust <-  edata %>% filter(seedmix=="Robust") %>% mutate(st.biomass = biomass-14.15029)
#Intermediate <-  edata %>% filter(seedmix=="Intermediate") %>% mutate(st.biomass = biomass-14.80358)
#Vigorous <-  edata %>% filter(seedmix=="Vigorous") %>% mutate(st.biomass = biomass-13.45494)
#edata <- full_join(Standard,Robust)  
#edata <- full_join(edata,Intermediate)  
#edata <- full_join(edata,Vigorous)  
####Plot
pd <- position_dodge(.6)
ggplot(pdata, aes(brickRatio, biomass, shape = brickRatio, color = acid, ymin = conf.low, ymax = conf.high))+
  geom_quasirandom(data = edata, aes(brickRatio, biomass, shape = brickRatio, color = acid), 
                   color = "grey70", dodge.width = .6, size = 0.7)+
  geom_hline(aes(yintercept = biomass), meandata, color = "grey70")+
  geom_errorbar(position = pd, width = 0.0, size = 0.4)+
  geom_point(position = pd, size = 2.5)+
  facet_grid(.~ seedmix)+
  scale_y_continuous(limits = c(0,33), breaks = seq(-100,100,5)) +
  scale_colour_manual(values = c("grey50","black")) +
  scale_shape_manual(values = c(1,16)) +
  #labs(x = "", y = expression(paste(Delta,"biomass [g]")), shape = "", color = "") +
  labs(x = "Brick ratio [vol%]", y = expression(paste("Biomass [g]")), shape = "", color = "") +
  guides(shape = F)+
  themeMB()
ggsave("main_biomass_acid_brickRatio_seedmix_(800dpi_16x5cm)2.tiff",
       dpi = 800, width = 16, height = 5, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/main")
### 2w brickRatio:watering -------------------------------------------------------------------------------------------
visreg(m2f, "brickRatio", by = "f.watering", data = edata,
       ylab = expression(paste(Delta,"biomass [g g"^"-1"*"]")), xlab = "",
       trans = exp, type = "contrast",
       partial = T, rug = F, gg = T, overlay = F, band = T, points = list(cex = 0.5, pch = 16), line=list(col = "black"), whitespace = .2) +
  theme_MB()
pdata <- ggemmeans(m2f, terms = c("brickRatio", "f.watering"), type = "fe")
pdata <- rename(pdata, biomass = predicted, brickRatio = x, f.watering = group)
#####Extract hlines and scale
meandata <- filter(pdata, brickRatio == "5")
#edata <- filter(edataFull,acid == "Acid treatment" & seedmix == "Robust" | seedmix == "Vigorous" & acid == "Acid treatment");edata$plot <- factor(edata$plot);edata$seedmix <- factor(edata$seedmix)
#####Plot
pd <- position_dodge(.6)
ggplot(pdata, aes(brickRatio, biomass, shape = brickRatio, ymin = conf.low, ymax = conf.high))+
  geom_quasirandom(data = edata, aes(brickRatio, biomass), 
                   color = "grey70", dodge.width = .6, size = 0.7)+
  geom_hline(aes(yintercept = biomass), meandata, color = "grey70") +
  geom_errorbar(position = pd, width = 0.0, size = 0.4) +
  geom_point(position = pd, size = 2.5) +
  facet_grid(~ f.watering) +
  scale_y_continuous(limits = c(0,33), breaks = seq(-100, 100, 5)) +
  scale_shape_manual(values = c(1,16,16,16)) +
  labs(x = "Brick ratio [vol%]", y = expression(paste("Biomass [g]")), shape = "", color = "") +
  guides(shape = F)+
  themeMB()
ggsave("main_biomass_watering_brickRatio_(800dpi_10x5cm)2.tiff",
       dpi = 800, width = 10, height = 5, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/main")
### 2w watering:seedmix -------------------------------------------------------------------------------------------
####Contrast plot
visreg(m2f, "seedmix", by = "f.watering", ylab = expression(paste(Delta,"biomass [g g"^"-1"*"]")), xlab = "", data = edata,
       type = "contrast", partial = T, rug = F, gg = T, overlay = F, band = T, points = list(cex = 0.5, pch = 16), line = list(col = "black"), whitespace = .2) +
  themeMB()
pdata <- ggemmeans(m2f, terms = c("seedmix", "f.watering"), type = "fe")
pdata <- rename(pdata, biomass = predicted, seedmix = x, f.watering = group)
#####Extract hlines and scale
meandata <- filter(pdata, seedmix == "Standard")
#edata <- filter(edataFull,acid == "Acid treatment" & seedmix == "Robust" | seedmix == "Vigorous" & acid == "Acid treatment");edata$plot <- factor(edata$plot);edata$seedmix <- factor(edata$seedmix)
#####Plot
pd <- position_dodge(.6)
ggplot(pdata, aes(seedmix, biomass, shape = seedmix, ymin = conf.low, ymax = conf.high))+
  geom_quasirandom(data = edata, aes(seedmix, biomass), 
                   color = "grey70", dodge.width = .6, size = 0.7)+
  geom_hline(aes(yintercept = biomass), meandata, color = "grey70") +
  geom_errorbar(position = pd, width = 0.0, size = 0.4) +
  geom_point(position = pd, size = 2.5) +
  facet_grid(~ f.watering) +
  scale_y_continuous(limits = c(0,33), breaks = seq(-100, 100, 5)) +
  scale_shape_manual(values = c(1,16,16,16)) +
  labs(x = "", y = expression(paste("Biomass [g]")), shape = "", color = "") +
  guides(x = guide_axis(angle = 45), shape = F)+
  themeMB()
ggsave("main_biomass_seedmix_watering_(800dpi_16x6cm).tiff",
       dpi = 800, width = 16, height = 6, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/main")


## 2 Demolition Bricks #############################################################################################

### 3w watering brickRatio brickType -----------------------------------------------------------------------------------------------------------------------
visreg(m4b,"treatment", by = "f.watering", ylab = expression(paste(Delta,"Biomass [g g"^"-1"*"]")), xlab = "",data = edataBricktype,
       trans = exp, type = "conditional", 
       partial = T, rug = F, gg = T, overlay = F, band = T, points=list(cex=0.5, pch=16), line=list(col="black"), whitespace=.2) +
  themeMB()
pd <- position_dodge(.6)
pdata <- ggemmeans(m3f, terms = c("treatment", "f.watering"), type = "fe")
pdata <- rename(pdata, biomass = predicted); pdata <- rename(pdata, treatment = x); pdata <- rename(pdata, f.watering = group)
####Extract hlines
meandata <- filter(pdata, brickType=="Clean bricks" & brickRatio=="5% bricks")
####Plot
ggplot(pdata,aes(treatment, biomass, shape = treatment, ymin = conf.low, ymax = conf.high)) +
  geom_quasirandom(data = edata,aes(treatment, biomass, shape = treatment),
                   color = "grey70", dodge.width = .6, size = 0.7)+
  geom_hline(aes(yintercept = biomass), meandata, color = "grey70")+
  geom_errorbar(position = pd, width = 0.0, size = 0.4)+
  geom_point(position = pd, size = 2.5)+
  facet_grid(.~ f.watering)+
  scale_y_continuous(limits = c(-9.9,9.3), breaks = seq(-100, 100, 5)) +
  scale_colour_manual(values = c("grey40","black")) +
  scale_shape_manual(values = c(1,16)) +
  labs(x="",y=expression(paste(Delta,"biomass [g]")), shape = "") +
  guides(shape = guide_legend(reverse = F))+
  themeMB()
#ggsave("main_brickType_watering_brickRatio_biomass_clean_allmix_2(800dpi_11x5cm).tiff",dpi=800,width=11,height=5, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/main")
### 2w brickType:brickRatio -------------------------------------------------------------------------------------------
####Contrast plot
visreg(m4b, "treatment", ylab = expression(paste(Delta,"Biomass [g g"^"-1"*"]")), xlab = "",data = edataBricktype,
       trans = exp, type = "contrast", partial = T, rug = F, gg = T, overlay = F, band = T,
       points = list(cex = 0.5, pch = 16), line = list(col="black"), whitespace=.2) +
  themeMB()
pdata <- ggemmeans(m4b, terms = "treatment", type = "fe")
pdata <- rename(pdata, biomass = predicted, treatment = x);
#####Extract hlines and scale
meandata <- filter(pdata, treatment=="5% clean")
#####Plot
pd <- position_dodge(.6)
ggplot(pdata,aes(treatment, biomass, shape = treatment, ymin = conf.low, ymax = conf.high))+
  geom_quasirandom(data = edataBricktype, aes(treatment, biomass),
                   color = "grey70", dodge.width = .6, size = 0.7)+
  geom_hline(aes(yintercept = biomass), meandata, color = "grey70")+
  geom_errorbar(position = pd, width = 0.0, size = 0.4)+
  geom_point(position = pd, size = 2.5)+
  scale_y_continuous(limits=c(0,25), breaks=seq(-100, 100, 5)) +
  scale_shape_manual(values=c(1,16,16)) +
  labs(x="",y=expression(paste("Biomass [g]")), shape = "",color="") +
  guides(shape = F)+
  themeMB()
ggsave("main_biomass_brickType_(800dpi_6.5x5cm).tiff",
       dpi = 800, width = 6.5, height = 5, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/main")
