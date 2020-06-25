#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Content #########################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data preparation
# Statistics
## 1. Frequentist
#### Data exploration
#### Model building
#### Model output
## 2. Bayesian
# C Plotten
## one-way interactions
## two-way interactions
## three-way interactions
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(installr);updateR(browse_news=F, install_R=T, copy_packages = T,copy_Rprofile.site = T,keep_old_packages = T, update_packages = T)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Data preparation ############################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(tidyverse);setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse")

edata <- read_table2("soil_site.txt", col_names = T, na="na", col_types =
                       cols(
                         plotID = col_factor(),
                         brickRatio = col_factor(levels = c("5","30")),
                         texture = col_factor(levels=c("loam","medium","sand")),
                         compaction = col_factor(levels=c("0","1")),
                         coal = col_factor(levels=c("0","1")),
                         ph = col_double(),
                         biomass = col_double(),
                         estRate = col_double()
                         )
                     )
###Create variables
edata$conf.low <- c(1:72);
edata$conf.high <- c(1:72)
###Recode levels
#edata$brickRatio <- dplyr::recode(edata$brickRatio,"5"="5% bricks","30"="30% bricks")
edata$texture <- dplyr::recode(edata$texture,
                               "loam" = "Loam", "medium" = "Medium", "sand" = "Sand")
edata$compaction <- dplyr::recode(edata$compaction,
                                  "0" = "Control", "1" = "Compaction")
edata$coal <- dplyr::recode(edata$coal,
                            "0" = "Control", "1" = "Charcoal")
#edata <- edata %>% slice(rep(1:n(),each=20));write.table(edata2,"edata2.txt",sep="\t",row.names=F)
edataPh <- edata %>% filter(coal == "Control") %>% filter(!is.na(ph))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(ggplot2);
library(ggbeeswarm);
library(car); #vif() variance inflation factors --> checking for dependence (Collinearity) (below 3 is ok)
library(nlme); #use for vif()
library(lme4);
library(lmerTest);
library(DHARMa);
library(MuMIn);
library(emmeans)


## 1. Frequentist #############################################################################################


## 2. Bayesian #############################################################################################
library(rjags);
library(BayesianTools);
library(MCMCvis)

# 1) Save a description of the model in JAGS syntax 
# to the working directory
sink("soil_biomass_bayesian.txt")
cat(
  "model{
  # Likelihood
  for(i in 1:n.dat){
    log.biomass[i] ~ dnorm(mu[i],tau)
    mu[i] <- alpha + 
    beta.brickRatio*brickRatio5[i] + 
    beta.texture*textureloam[i] + 
    beta.compaction*compaction0[i] +
    beta.coal*coal0[i] +
    beta.inter.bs*brickRatio5[i]*textureloam[i] +
    beta.inter.bc*brickRatio5[i]*compaction0[i] +
    beta.inter.cs*compaction0[i]*textureloam[i]
    # + beta.inter.bsc*brickRatio5[i]*textureloam[i]*compaction0[i]
    }
  
  # Prior distributions
  alpha ~ dnorm(0,0.001)
  beta.brickRatio ~ dnorm(0,0.001)
  beta.texture ~ dnorm(0,0.001)
  beta.compaction ~ dnorm(0,0.001)
  beta.coal ~ dnorm(0,0.001)
  beta.inter.bs ~ dnorm(0,0.001)
  beta.inter.bc ~ dnorm(0,0.001)
  beta.inter.cs ~ dnorm(0,0.001)
  # beta.inter.bsc ~ dnorm(0,0.001)
  tau <- 1/(sigma*sigma)
  sigma ~ dunif(0,100)
  }
  ",fill = TRUE)

sink()

# 2) Set up a list that contains all the necessary data
Data = list(log.biomass = edata$log.biomass, 
            brickRatio5 = ifelse(edata$brickRatio == '5', 1, 0),
            textureloam = ifelse(edata$texture == 'loam', 1, 0),
            compaction0 = edata$compaction,
            coal0 = edata$coal,
            n.dat = nrow(edata))

# 3) Specify a function to generate inital values for the parameters
inits.fn <- function() list(alpha = rnorm(1), 
                            beta.brickRatio = rnorm(1),
                            beta.texture = rnorm(1),
                            beta.compaction = rnorm(1),
                            beta.coal = rnorm(1),
                            beta.inter.bs = rnorm(1),
                            beta.inter.bc = rnorm(1),
                            beta.inter.cs = rnorm(1),
                            # beta.inter.bsc = rnorm(1),
                            log.biomass = rnorm(0.5),
                            sigma = runif(1,1,100))

# Compile the model and run the MCMC for an adaptation (burn-in) phase
jagsModel <- jags.model(file = "soil_biomass_bayesian.txt", data=Data, 
                        init = inits.fn, n.chains = 3, 
                        n.adapt= 5000)

# Specify parameters for which posterior samples are saved
para.names <- c("alpha","beta.brickRatio","beta.texture","beta.compaction","beta.coal", "beta.inter.bs","beta.inter.bc","beta.inter.cs",
                #"beta.inter.bsc",
                "sigma")

# Continue the MCMC runs with sampling
Samples <- coda.samples(jagsModel, variable.names = para.names, n.iter = 5000)

# Check convergence #1.1 is acceptable
gelman.diag(Samples)

# Correlation plot
correlationPlot(Samples)

# Statistical summaries of the (marginal) posterior distribution
# for each parameter
MCMCsummary(Samples)

# Graphical overview of the samples from the MCMC chains
marginalPlot(Samples)
tiff("soil_biomass_bayesian_reduced_(72ppi_8x8cm).tiff", width=8,height=8,res=800,units = "cm")
par(mfrow=c(1,1));MCMCplot(object = Samples,
                           #params = c("beta.brickRatio","beta.texture","beta.compaction","beta.coal","beta.inter.bs","beta.inter.bc","beta.inter.cs"
                                      #,"beta.inter.bsc"
                                      #),
                           params = c("beta.coal","beta.inter.bs","beta.inter.bc","beta.inter.cs"
                                      #,"beta.inter.bsc"
                           ),
                           ref_ovl = F,
                           rank = T,
                           guide_lines = F,
                           horiz = T,
                           xlab = "Delta log(biomass)",
                           main = "MCMCvis plot",
                           #labels = c("Brick ratio","Soil type", "compaction","Coal","B x S","B x C","C x S"
                                      #,"B x S x C"
                                      #)
                           labels = c("Coal","B x S","B x C","C x S"
                                      #,"B x S x C"
                           )
                  )
dev.off()

# DHARMa
library(DHARMa)
x = getSample(Samples)
# note - yesterday, we calcualted the predictions from the parameters
# here we observe them direct - this is the normal way to calcualte the 
# posterior predictive distribution
posteriorPredSim = x[,9:80]
posteriorPredDistr = x[,9:80]

sim = createDHARMa(simulatedResponse = t(posteriorPredSim), 
                   observedResponse = edata$log.biomass, 
                   fittedPredictedResponse = apply(posteriorPredDistr, 2, median), 
                   integerResponse = T)
plot(sim)




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

## 1 way interactions #############################################################################################

### coal-------------------------------------------------------------------------
visreg(m1b, "coal", ylab = expression("Biomass [g]"), xlab = "", data = edata,
       trans = exp, type = "conditional",
       partial = T, gg = T, overlay = F, band = T, points = list(cex = 0.5, pch = 16), line = list(col = "black"), whitespace = .2) +
  scale_y_continuous(limits = c(0,21), breaks = seq(-100, 100, 5)) +
  themeMB()
ggsave("soil_biomass_coal_(800dpi_5x4cm).tiff",dpi=800,width=5,height=4, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/soil")

## 2 way interactions #############################################################################################

### compaction:texture-------------------------------------------------------------------------
visreg(m1b, "compaction", by="texture", ylab = "Biomass [g]", xlab = "", data = edata,
       trans = exp, type = "conditional",
       partial = T, rug = F, gg = T, overlay = F, band = T, points = list(cex = 0.5, pch = 16), line = list(col = "black"), whitespace = .2) +
  scale_y_continuous(limits = c(0,21), breaks = seq(-100, 100, 5)) +
  themeMB()
ggsave("soil_biomass_texture_compaction_(800dpi_8x5cm).tiff",
       dpi = 800, width = 8, height = 5, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/soil")
### brickRatio:compaction-------------------------------------------------------------------------
visreg(m1b,"compaction",by="brickRatio",ylab="Biomass [g]", xlab="",data=edata,
       trans=exp, type="conditional", partial=T, rug=F, gg=T, overlay=F, band=T,points=list(cex=0.5, pch=16), line=list(col="black"), whitespace=.2) +
  scale_y_continuous(limits = c(0,21), breaks = seq(-100,100,5)) +
  themeMB()
ggsave("soil_biomass_brickRatio_compaction_(800dpi_8x5cm)2.tiff",
       dpi = 800, width = 8, height = 5, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/soil")
### brickRatio:texture-------------------------------------------------------------------------
visreg(m1b, "brickRatio", by = "texture", ylab = "Biomass [g]", xlab = "Brick ratio [Vol.-%]",data = edata,
       trans = exp, type = "conditional",
       partial = T, rug = F, gg = T, overlay = F, band = T, points = list(cex = 0.5, pch = 16), line = list(col = "black"), whitespace = .2) +
  scale_y_continuous(limits = c(0,21), breaks = seq(-100, 100, 5)) +
  themeMB()
pd <- position_dodge(.6)
pdata <- ggemmeans(m1b, terms = c("brickRatio","texture"), type = "fe")
pdata <- rename(pdata, biomass = predicted, brickRatio = x, texture = group);
meandata <- filter(pdata, brickRatio == "5")
####Plot
ggplot(pdata,aes(brickRatio, biomass, shape = brickRatio, ymin = conf.low, ymax = conf.high))+
  geom_quasirandom(data = edata, aes(brickRatio, biomass, shape = brickRatio),
                   color = "grey70", dodge.width = .6, size = 0.7)+
  geom_hline(aes(yintercept = biomass), meandata, color = "grey70")+
  geom_errorbar(position = pd, width = 0.0, size = 0.4)+
  geom_point(position = pd, size = 2.5)+
  facet_grid(.~ texture)+
  scale_y_continuous(limits = c(0,18), breaks = seq(-100,100,5)) +
  scale_colour_manual(values = c("grey40","black")) +
  scale_shape_manual(values = c(1,16)) +
  labs(x = "Brick ratio [Vol.%]",y = expression(paste("Biomass [g]")), shape = "",color = "") +
  guides(shape = F)+
  themeMB()
ggsave("soil_biomass_texture_brickRatio_(800dpi_8x5cm)2.tiff",
       dpi = 800, width = 8, height = 5, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/soil")

## 3 way interactions #############################################################################################

### brickRatio:texture:compaction -----------------------------------------------------------------------------------------------------------------------
pd <- position_dodge(.6)
pdata <- ggemmeans(m1b, terms = c("brickRatio","texture","compaction"), type = "fe")
pdata <- rename(pdata, biomass = predicted, brickRatio = x, texture = group, compaction = facet);
####Extract hlines and scale
#adata <-  pdata %>% filter(texture=="loam") %>% mutate(st.biomass = biomass-9.656765) %>% mutate(st.conf.low = conf.low-9.656765) %>% mutate(st.conf.high = conf.high-9.656765)
#bdata <-  pdata %>% filter(texture=="medium") %>% mutate(st.biomass = biomass-10.644185) %>% mutate(st.conf.low = conf.low-10.644185) %>% mutate(st.conf.high = conf.high-10.644185)
#cdata <-  pdata %>% filter(texture=="sand") %>% mutate(st.biomass = biomass-9.099560) %>% mutate(st.conf.low = conf.low-9.099560) %>% mutate(st.conf.high = conf.high-9.099560)
#pdata <- full_join(adata,bdata)  
#pdata <- full_join(pdata,cdata)  
#adata <-  edata %>% filter(texture=="loam") %>% mutate(st.biomass = biomass-9.656765)
#bdata <-  edata %>% filter(texture=="medium") %>% mutate(st.biomass = biomass-10.644185)
#cdata <-  edata %>% filter(texture=="sand") %>% mutate(st.biomass = biomass-9.099560)
#edata <- full_join(adata,bdata)  
#edata <- full_join(edata,cdata)
#rm(adata,bdata,cdata)
meandata <- filter(pdata, compaction == "Control" & brickRatio == "5")
####Plot
ggplot(pdata,aes(brickRatio, biomass, color = compaction, shape = brickRatio, ymin = conf.low, ymax = conf.high))+
  geom_quasirandom(data = edata, aes(brickRatio, biomass, color = compaction, shape = brickRatio),
                   color = "grey70", dodge.width = .6, size = 0.7)+
  geom_hline(aes(yintercept = biomass), meandata, color = "grey70")+
  geom_errorbar(position = pd, width = 0.0, size = 0.4)+
  geom_point(position = pd, size = 2.5)+
  facet_grid(.~ texture)+
  scale_y_continuous(limits = c(0,20), breaks = seq(-100,100,5)) +
  scale_colour_manual(values = c("grey40","black")) +
  scale_shape_manual(values = c(1,16)) +
  labs(x = "Brick ratio [Vol.%]",y = expression(paste("Biomass [g]")), shape = "",color = "") +
  guides(shape = F)+
  themeMB()
ggsave("soil_biomass_texture_compaction_brickRatio_(800dpi_16x5cm).tiff",
       dpi = 800, width = 16, height = 5, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/soil")
