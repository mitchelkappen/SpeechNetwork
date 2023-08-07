#############################
#                           #
#### SPEECH & STRESS 5.0 ####
#############################
rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window

#Load packages
library(car)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(cowplot)
library(yarrr)
library(qgraph)
library(bootnet)
library(mgm)
library(huge)
library(skimr)
library(tidyr)
library(reshape)
library(lme4)
library(lmerTest)
library(pander)
library(emmeans)
library(NetworkComparisonTest)

#Set WD & read in data (follow folder structure as shown in OSF)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # CHANGE THIS IF NOT USING RSTUDIO

FullData <- read.csv2("../../Data/completeData.csv", head = TRUE, sep=",",  stringsAsFactors=FALSE)
FullData$ID = as.integer(FullData$ID)
# FullData <- FullData[FullData$KNOWS_EXPERIMENT_GOAL == "False",] # This line excludes all participants that guess the purpose of the study

myvars <- c("NA_REACTIVITY", 
            "DELTA_F0SEMITONEFROM27.5HZ_SMA3NZ_AMEAN", 
            "DELTA_HNRDBACF_SMA3NZ_AMEAN", 
            "DELTA_JITTERLOCAL_SMA3NZ_AMEAN", 
            "DELTA_MEANVOICEDSEGMENTLENGTHSEC", 
            "DELTA_F1F2RATIO") 
myvarspre <- c("PRE_F0SEMITONEFROM27.5HZ_SMA3NZ_AMEAN", 
               "PRE_HNRDBACF_SMA3NZ_AMEAN", 
               "PRE_JITTERLOCAL_SMA3NZ_AMEAN", 
               "PRE_MEANVOICEDSEGMENTLENGTHSEC", 
               "PRE_F1F2RATIO") 
myvarspost <- c("POST_F0SEMITONEFROM27.5HZ_SMA3NZ_AMEAN", 
               "POST_HNRDBACF_SMA3NZ_AMEAN", 
               "POST_JITTERLOCAL_SMA3NZ_AMEAN", 
               "POST_MEANVOICEDSEGMENTLENGTHSEC", 
               "POST_F1F2RATIO") 

Data_SR <- na.omit(FullData[,c(myvars, "ID")])
Data_Pre<- na.omit(FullData[,c(myvarspre, "ID")])
Data_Post<- na.omit(FullData[,c(myvarspost, "ID")])

length(unique(Data_SR$ID)) #148
length(unique(Data_Pre$ID))#148
length(unique(Data_Post$ID))#148

# Rewrite data without ID:
Data_SR <- na.omit(Data_SR[,myvars])
Data_Pre<- na.omit(Data_Pre[,myvarspre])
Data_Post<- na.omit(Data_Post[,myvarspost])

n <- 148
v_SR <- 6
v_pre <- 5
v_post <- 5

plotPrefix = '../../Plots/'
###############################
#### 1. Manipulation check ####
###############################
#### 1.1 Check data distributions ####
#### 1.1.1. Neg. Affect ####
# Get Neg. Affect data pre- and post-stressor
myvarsaffect <- c("NEGAFFPRE", 
                "NEGAFFPOST") 

affectData <- na.omit(FullData[,c(myvarsaffect, "ID")])
affectData[myvarsaffect] <- sapply(affectData[myvarsaffect],as.numeric)

for (i in 1:length(affectData)) {
  affectData[i] <- unlist(affectData[i])
}

# Reshape data to long Format for plotting
data_longAffect <- reshape(
  affectData,
  direction = "long",
  varying = list(names(affectData)[1:length(affectData) -
                                   1]),
  v.names = "Value",
  idvar = c("ID")
)
data_longAffect$time <- factor(data_longAffect$time)
# Generate density plots to check for normal distribution
par(mfrow = c(2, 1))
a = ggdensity(affectData$NEGAFFPRE,
              main = "Density plot ",
              xlab = "value")
b = ggdensity(affectData$NEGAFFPOST,
              main = "Density plot ",
              xlab = "value")
plot_grid(
  a,
  b,
  labels = c("Pre", "Post"),
  ncol = 1,
  nrow = 2
)
# Save density plots as png
dev.copy(png,
         paste0(plotPrefix, 'densityPlot_Affect.png'))
dev.off()

# Run Shapiro Wilks test to test for normality
print(shapiro.test(affectData$NEGAFFPRE))
print(shapiro.test(affectData$NEGAFFPOST))

#### 1.1.2. EDA ####
# Get Neg. Affect data pre- and post-stressor
myvarseda <- c("MEAN_SCL_PRE_LEFT", 
                "MEAN_SCL_STRESS_LEFT",
                "MEAN_SCL_POST_LEFT") 

edaData <- na.omit(FullData[,c(myvarseda, "ID")])
edaData[myvarseda] <- sapply(edaData[myvarseda],as.numeric)
names(edaData)[1] = "EDA_Pre"
names(edaData)[2] = "EDA_During"
names(edaData)[3] = "EDA_Post"

for (i in 1:length(edaData)) {
  edaData[i] <- unlist(edaData[i])
}

# Reshape data to long Format for plotting
data_longEda <- reshape(
  edaData,
  direction = "long",
  varying = list(names(edaData)[1:length(edaData) -
                                   1]),
  v.names = "Value",
  idvar = c("ID")
)
data_longEda$time <- factor(data_longEda$time)
# Generate density plots to check for normal distribution
par(mfrow = c(3, 1))
a = ggdensity(edaData$EDA_Pre,
              main = "Density plot ",
              xlab = "value")
b = ggdensity(edaData$EDA_During,
              main = "Density plot ",
              xlab = "value")
c = ggdensity(edaData$EDA_Post,
              main = "Density plot ",
              xlab = "value")
plot_grid(
  a,
  b,
  c,
  labels = c("Pre", "During", "Post"),
  ncol = 1,
  nrow = 3
)
# Save density plots as png
dev.copy(png,
         paste0(plotPrefix, 'densityPlot_EDA.png'))
dev.off()

# Run Shapiro Wilks test to test for normality
print(shapiro.test(edaData$EDA_Pre))
print(shapiro.test(edaData$EDA_During))
print(shapiro.test(edaData$EDA_Post))

#### 1.2 GLMMs ####
#### 1.2.1. Neg. Affect ####
data_longAffect$timeTEXT[data_longAffect$time == 1] = 'Pre'
data_longAffect$timeTEXT[data_longAffect$time == 2] = 'Post'
data_longAffect$timeTEXT = factor(data_longAffect$timeTEXT, levels=c("Pre", "Post"))
data_longAffect$NegAff = data_longAffect$Value

d1.1 <- lmer(NegAff ~ time + (1|ID),data=data_longAffect)
# Not adding other GLM types due to zero values being present

tabel <- cbind(AIC(d1.1))

colnames(tabel) <- c("Normal")

emphasize.strong.cells(which(tabel == min(tabel), arr.ind = TRUE))
pandoc.table(tabel) # Gamma-Identity  has superior fit

## Choose right model here - based on table
anova(d1.1)
emmeans(d1.1, pairwise ~ time , adjust ="fdr", type="response", na.exclude = "TRUE")

#### 1.2.2. EDA ####
data_longEda$timeTEXT[data_longEda$time == 1] = 'Pre'
data_longEda$timeTEXT[data_longEda$time == 2] = 'During'
data_longEda$timeTEXT[data_longEda$time == 3] = 'Post'

data_longEda$timeTEXT = factor(data_longEda$timeTEXT, levels=c("Pre", "During", "Post"))
data_longEda$EDA = data_longEda$Value

par(mfrow=c(1,1))
library(fitdistrplus)
descdist(c(na.exclude(data_longEda$EDA)),discrete = FALSE) # Determine possible underlying distributions

d1.1 <- lmer(EDA ~ time + (1|ID),data=data_longEda)
d1.2 <- glmer(EDA ~ time + (1|ID),data=data_longEda, family = Gamma(link = "log"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 10000)))
d1.3 <- glmer(EDA ~ time + (1|ID),data=data_longEda, family = Gamma(link = "inverse"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 10000)))

tabel <- cbind(AIC(d1.1), AIC(d1.2), AIC(d1.3))

colnames(tabel) <- c("Normal", "Gamma-Log", "Gamme-Inverse")

emphasize.strong.cells(which(tabel == min(tabel), arr.ind = TRUE))
pandoc.table(tabel) # Gamma-Identity  has superior fit

## Choose right model here
Anova(d1.2)
emmeans(d1.2, pairwise ~ time , adjust ="fdr", type="response")

#### 1.3 Visualisations ####
#### 1.3.1. Neg. Affect ####
dpi=600    #pixels per square inch
jpeg(paste0(plotPrefix, "Figure1a_NegAff.jpeg"), width=8*dpi, height=8*dpi, res=dpi)
par(mfcol = c(1, 1))
plot1a <- pirateplot(
  formula = NegAff ~ timeTEXT,
  data = data_longAffect,
  theme = 0,
  pal = "espresso",
  main = "Negative Affect pre- and post- stressor as measured by VAS scales",
  bean.f.o = .6, # Bean fill
  point.o = .3,  # Points
  inf.f.o = .7,  # Inference fill
  inf.b.o = .8,  # Inference border
  avg.line.o = 1,  # Average line
  # bar.f.o = .5, # Bar
  inf.f.col = "white",  # Inf fill col
  inf.b.col = "black",  # Inf border col
  avg.line.col = "black",  # avg line col
  bar.f.col = gray(.8),  # bar filling color
  point.pch = 21,
  point.bg = "white",
  point.col = "black",
  point.cex = .7,
  
  xlab = "",
)
# Insert visual indicators of significance
x <- 1:3  # capture x coordinates of bars
y <- 65 # create the y coordinate of the line
offset <- 0.6  # set an offset for tick lengths
lines(c(1.1,1.9),c(y, y))  # draw first horizontal line
lines(c(1.1,1.1),c(y, y-offset))  # draw ticks
lines(c(1.9,1.9),c(y, y-offset))
text(x[1]+((x[2]-x[1])/2),y+offset,"***")  # draw asterics
dev.off()

#### 1.3.2. EDA ####
dpi=600    #pixels per square inch
jpeg(paste0(plotPrefix, "Figure1b_eda.jpeg"), width=8*dpi, height=8*dpi, res=dpi)
par(mfcol = c(1, 1))
plot1a <- pirateplot(
  formula = EDA ~ timeTEXT,
  data = data_longEda,
  theme = 0,
  pal = "espresso",
  main = "EDA pre-, during, and post- stressor",
  bean.f.o = .6, # Bean fill
  point.o = .3,  # Points
  inf.f.o = .7,  # Inference fill
  inf.b.o = .8,  # Inference border
  avg.line.o = 1,  # Average line
  # bar.f.o = .5, # Bar
  inf.f.col = "white",  # Inf fill col
  inf.b.col = "black",  # Inf border col
  avg.line.col = "black",  # avg line col
  bar.f.col = gray(.8),  # bar filling color
  point.pch = 21,
  point.bg = "white",
  point.col = "black",
  point.cex = .7,
  
  xlab = "",
)
# Insert visual indicators of significance
x <- 1:3  # capture x coordinates of bars
y <- 21  # create the y coordinate of the line
offset <- 0.2  # set an offset for tick lengths
lines(c(1.1,1.9),c(y, y))  # draw first horizontal line
lines(c(1.1,1.1),c(y, y-offset))  # draw ticks
lines(c(1.9,1.9),c(y, y-offset))
text(x[1]+((x[2]-x[1])/2),y+offset,"***")  # draw asterics

#Bar2
y <- 21
offset <- 0.2
lines(c(2.1,2.9),c(y, y))
lines(c(2.1,2.1),c(y, y-offset))
lines(c(2.9,2.9),c(y, y-offset))
text(x[2]+((x[3]-x[2])/2),y+offset,"***")

#Bar3
y <- 22
offset <- 0.2
lines(c(1.1,2.9),c(y, y))
lines(c(1.1,1.1),c(y, y-offset))
lines(c(2.9,2.9),c(y, y-offset))
text(x[1]+((x[3]-x[1])/2),y+offset,"***")

dev.off()

#### 1.4 Correlations ####
# Make Delta / change scores for both manipulation check variables
EDADelta = edaData$EDA_Post - edaData$EDA_Pre
affectDelta = affectData$NEGAFFPOST - affectData$NEGAFFPRE

# Run Pearson correlation
cor.test(EDADelta,affectDelta, use = "complete.obs", method = c("pearson"))
test <- cor.test(affectDelta,EDADelta, use = "complete.obs", method = c("pearson"))

# Save correlation plot as jpeg
dpi=600    #pixels per square inch
jpeg(paste0(plotPrefix, "manipulationCorrelation.jpeg"), width=8*dpi, height=8*dpi, res=dpi)
par(mfcol = c(1, 1))
plot(affectDelta,EDADelta)
abline(lm(EDADelta~affectDelta), col="red") # regression line (y~x)
dev.off()

###############################
#### 2. Network analyses   ####
###############################
#### 2.1 Network analysis resting state speech parameters ####
names(Data_Pre)
labels_Pre   <- c("F0",
                 "HNR",
                 "JIT",
                 "VO",
                 "F1/2")

# Make sure all variables are numeric and check details
Data_Pre[myvarspre] <- sapply(Data_Pre[myvarspre],as.numeric)
skim(Data_Pre)

#Check norm
hist(Data_Pre)

#Transform & check effect
Data_norm_Pre <- huge.npn(Data_Pre)

par(mfrow=c(2,3))
for(i in 1:v_pre) {
  hist(Data_norm_Pre[,i])}

dev.off()

par(mfrow=c(1,1))

#Obtain GGM
Cor_Data_Pre <- cor_auto(Data_norm_Pre)
PreNetwork <- qgraph(Cor_Data_Pre, layout = "spring", threshold = F, graph = "glasso", tuning = 0.5,  labels = labels_Pre, sampleSize = 148, details=TRUE, palette = "pastel", legend.cex=0.4, DoNotPlot=F)


# Add predictability to the plot
p_Pre <- ncol(Data_norm_Pre)
set.seed(111)
fit_obj_Pre <- mgm(data=Data_norm_Pre,
                type = rep('g', p_Pre),
                level = rep('1', p_Pre),
                lambdSel = 'EBIC',
                ruleReg = 'OR')
pred_obj_Pre <- predict(object = fit_obj_Pre, 
                     data = Data_norm_Pre, 
                     errorCon = 'R2')
#pred_obj_Pre$errors
#fit_obj_Pre$interactions

PreNetwork_mgm <- qgraph(fit_obj_Pre$pairwise$wadj, layout = "spring", cut=0,
                         palette = "colorblind",
                         labels = labels_Pre, vsize=4.5,
                         label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                         edge.color = fit_obj_Pre$pairwise$edgecolor)


#### 2.2 Network analysis stress state speech parameters ####
names(Data_Post)
labels_Post   <- c("F0",
                  "HNR",
                  "JIT",
                  "VO",
                  "F1/2")

Data_Post[myvarspost] <- sapply(Data_Post[myvarspost],as.numeric)
skim(Data_Post)

#Check norm
hist(Data_Post)

#Transform & check effect
Data_norm_Post <- huge.npn(Data_Post)

par(mfrow=c(2,3))
for(i in 1:v_post) {
  hist(Data_norm_Post[,i])}

dev.off()

par(mfrow=c(1,1))

#Obtain GGM
Cor_Data_Post <- cor_auto(Data_norm_Post)

PostNetwork <- qgraph(Cor_Data_Post, layout = "spring", threshold = F, graph = "glasso", tuning = 0.5,  labels = labels_Post, sampleSize = 148, details=TRUE, palette = "pastel", legend.cex=0.4, DoNotPlot=F)


# Add predictability to the plot
p_Post <- ncol(Data_norm_Post)
set.seed(111)
fit_obj_Post <- mgm(data=Data_norm_Post,
                   type = rep('g', p_Post),
                   level = rep('1', p_Post),
                   lambdSel = 'EBIC',
                   ruleReg = 'OR')
pred_obj_Post <- predict(object = fit_obj_Post, 
                        data = Data_norm_Post, 
                        errorCon = 'R2')
# pred_obj_Post$errors


PostNetwork_mgm <- qgraph(fit_obj_Post$pairwise$wadj, layout = "spring", cut=0,
                         palette = "colorblind",
                         labels = labels_Post, vsize=4.5,
                         label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                         edge.color = fit_obj_Post$pairwise$edgecolor)


#### 2.3 Plot networks Pre - Post including predictability: ####
L1 <- averageLayout(getWmat(PreNetwork), getWmat(PostNetwork))

R2_network_Pre <- qgraph(PreNetwork,
                         layout = L1,
                         pie = as.numeric(as.character(pred_obj_Pre$error[,2])), 
                         pieColor = rep('#377EB8', 5),
                         labels = labels_Pre,
                         theme="colorblind", 
                         maximum=0.78,
                         details=FALSE,
                         negDashed=TRUE)

R2_network_Post <- qgraph(PostNetwork,
                         layout = L1,
                         pie = as.numeric(as.character(pred_obj_Post$error[,2])), 
                         pieColor = rep('#377EB8', 5),
                         labels = labels_Post,
                         theme="colorblind",
                         maximum=0.78,
                         details=TRUE,
                         negDashed=TRUE)

# Centrality for both networks:
CentralityPrePost <- centralityPlot(list("Resting state" = R2_network_Pre, "Stressor" = R2_network_Post))

# Analysis of stability and accuracy of the GGM derived from Pre:
library("bootnet")  
bootPrefix = '../BootFiles/'

Bootnet_NetworkPre <- estimateNetwork(Data_norm_Pre, tuning= 0.5, corMethod = "cor_auto", default = 	"glasso")
plot(Bootnet_NetworkPre)
Preboot1 <- bootnet(Bootnet_NetworkPre, nCores = 4)
Preboot2 <- bootnet(Bootnet_NetworkPre, nBoots = 1000, type = "case",  nCores = 4)
# save(Preboot1, file = paste0(bootPrefix, "Preboot1.Rdata"))
# save(Preboot2, file = paste0(bootPrefix, "Preboot2.Rdata"))
# load(file = paste0(bootPrefix, "Preboot1.Rdata"))
# load(file = paste0(bootPrefix, "Preboot2.Rdata"))
Acc_Pre <- plot(Preboot1, labels = F, order = "sample") 
Stab_Pre <- plot(Preboot2)
Preboot3 <- plot(Preboot1, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
SignEdge_Pre <- plot(Preboot3)
# save(Preboot3, file = paste0(bootPrefix, "Preboot3.Rdata"))
# load(file = paste0(bootPrefix, "Preboot3.Rdata"))
CorrStab_Pre <- corStability(Preboot2)

# Analysis of stability and accuracy of the GGM derived from Post:
Bootnet_NetworkPost <- estimateNetwork(Data_norm_Post, tuning= 0.5, corMethod = "cor_auto", default = 	"glasso")
plot(Bootnet_NetworkPost)
Postboot1 <- bootnet(Bootnet_NetworkPost, nCores = 4)
Postboot2 <- bootnet(Bootnet_NetworkPost, nBoots = 1000, type = "case",  nCores = 4)
# save(Postboot1, file = paste0(bootPrefix, "Postboot1.Rdata"))
# save(Postboot2, file = paste0(bootPrefix, "Postboot2.Rdata"))
# load(file = paste0(bootPrefix, "Postboot1.Rdata"))
# load(file = paste0(bootPrefix, "Postboot2.Rdata"))
Acc_Post <- plot(Postboot1, labels = F, order = "sample") 
Stab_Post <- plot(Postboot2)
Postboot3 <- plot(Postboot1, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
SignEdge_Post <- plot(Postboot3)
# save(Postboot3, file = paste0(bootPrefix, "Postboot3.Rdata"))
# load(file = paste0(bootPrefix, "Postboot3.Rdata"))
CorrStab_Post <- corStability(Postboot2)

# Comparison of models obtained from both time moments (pre-post:
WeightMatrix_Pre <- getWmat(PreNetwork) 
WeightMatrix_Post <- getWmat(PostNetwork) 
cor(WeightMatrix_Pre[lower.tri(WeightMatrix_Pre)], WeightMatrix_Post[lower.tri(WeightMatrix_Post)], method="pearson") #.99

Centrality1 <- centralityTable(PreNetwork, standardized = TRUE)
Centrality2 <- centralityTable(PostNetwork, standardized = TRUE)
cor(Centrality1$value, Centrality2$value, method="pearson") #.99

R2_S1 <- as.numeric(as.character(pred_obj_Pre$errors$R2))
R2_S2 <- as.numeric(as.character(pred_obj_Post$errors$R2))
R2_S1
R2_S2
mean(R2_S1)
mean(R2_S2)
cor(R2_S1, R2_S2, method="pearson") #.99

# NetworkComparisonTest
library(NetworkComparisonTest)
comparison <- NCT(Data_norm_Pre, Data_norm_Post, gamma=0.5, it=1000, paired = T, binary.data=F, test.edges=F, test.centrality = F) 
summary(comparison)

# Save output:
dpi=600    #pixels per square inch
jpeg(paste0(plotPrefix, "Figure2.jpeg"), width=8*dpi, height=4*dpi, res=dpi)
par(mfcol = c(1, 2))
plot(R2_network_Pre)
plot(R2_network_Post)
dev.off()

jpeg(paste0(plotPrefix, "Figure3.jpeg"), width=4*dpi, height=8*dpi, res=dpi)
par(mfcol = c(1, 1))
CentralityPrePost
dev.off()


#### Supplemental: ####
Acc_Pre
Acc_Post

Stab_Pre
Stab_Post

SignEdge_Pre
SignEdge_Post

CorrStab_Pre #.75
CorrStab_Post #.75

R2_S1
R2_S2




#### 2.4 Network analyses Stress reactivity - Delta scores ####
names(Data_SR)
labels_SR   <- c("NA",
                 "F0",
                 "HNR",
                 "JIT",
                 "VO",
                 "F1/2")

Data_SR[myvars] <- sapply(Data_SR[myvars],as.numeric)
skim(Data_SR)

#Check norm
hist(Data_SR)

#Transform
Data_norm_SR <- huge.npn(Data_SR)

par(mfrow=c(2,3))
for(i in 1:v_SR) {
  hist(Data_norm_SR[,i])}

dev.off()

par(mfrow=c(1,2))

#Obtain GGM
Cor_Data_SR <- cor_auto(Data_norm_SR)

SRNetwork <- qgraph(Cor_Data_SR, layout = "circle", threshold = F, graph = "glasso", tuning = 0.5,  labels = labels_SR, sampleSize = 148, details=TRUE, palette = "pastel", legend.cex=0.4, DoNotPlot=F)
WeightMatrix_SR <- getWmat(SRNetwork)

# Add predictability to the plot
p_SR <- ncol(Data_norm_SR)
set.seed(111)
fit_obj_SR <- mgm(data=Data_norm_SR,
                   type = rep('g', p_SR),
                   level = rep('1', p_SR),
                   lambdSel = 'EBIC',
                   ruleReg = 'OR')
pred_obj_SR <- predict(object = fit_obj_SR, 
                        data = Data_norm_SR, 
                        errorCon = 'R2')
# pred_obj_SR$errors


SRNetwork_mgm <- qgraph(fit_obj_SR$pairwise$wadj, layout = "circle", cut=0,
                         palette = "colorblind",
                         labels = labels_SR, vsize=4.5,
                         label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                         edge.color = fit_obj_SR$pairwise$edgecolor)

# Plot R2-network:
par(mfrow=c(1,1))
R2_network_SR <- qgraph(SRNetwork,
                          layout = "spring",
                          labels = labels_SR,
                          theme="colorblind",
                          pie = as.numeric(as.character(pred_obj_SR$error[,2])), 
                          pieColor = rep('#377EB8', 6),
                          details=TRUE,
                          negDashed=TRUE)



# Centrality:
Centrality_SR <- centralityPlot(R2_network_SR)

# Analysis of stability and accuracy of the GGM derived from change scores:
library("bootnet")  
Bootnet_NetworkSR <- estimateNetwork(Data_norm_SR, tuning= 0.5, corMethod = "cor_auto", default = 	"glasso")
plot(Bootnet_NetworkSR)
SRboot1 <- bootnet(Bootnet_NetworkSR, nCores = 4)
SRboot2 <- bootnet(Bootnet_NetworkSR, nBoots = 1000, type = "case",  nCores = 4)
# save(SRboot1, file = paste0(bootPrefix, "SRboot1.Rdata"))
# save(SRboot2, file = paste0(bootPrefix, "SRboot2.Rdata"))
# load(file = paste0(bootPrefix, "SRboot1.Rdata"))
# load(file = paste0(bootPrefix, "SRboot2.Rdata"))

Acc_SR <- plot(SRboot1, labels = F, order = "sample") 
Stab_SR <- plot(SRboot2)
SRboot3 <- plot(SRboot1, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
SignEdge_SR <- plot(SRboot3)
# save(SRboot3, file = paste0(bootPrefix, "SRboot3.Rdata"))
# load(file = paste0(bootPrefix, "SRboot3.Rdata"))
CorrStab_SR <- corStability(SRboot2)


# Save output:
jpeg(paste0(plotPrefix, "Figure4.jpeg"), width=4*dpi, height=4*dpi, res=dpi)
plot(R2_network_SR)
dev.off()


#### Supplemental: ####
Centrality_SR
Acc_SR
Stab_SR
SignEdge_SR
CorrStab_SR #.28
pred_obj_SR$errors

# Alternative (circular) network layouts
# Aim 1
# Pre-Network
R2_network_Pre <- qgraph(PreNetwork,
                         layout = "circular",
                         pie = as.numeric(as.character(pred_obj_Pre$error[,2])), 
                         pieColor = rep('#377EB8', 5),
                         labels = labels_Pre,
                         theme="colorblind", 
                         maximum=0.78,
                         details=FALSE,
                         negDashed=TRUE)
#Post-Nework
R2_network_Post <- qgraph(PostNetwork,
                          layout = "circular",
                          pie = as.numeric(as.character(pred_obj_Post$error[,2])), 
                          pieColor = rep('#377EB8', 5),
                          labels = labels_Post,
                          theme="colorblind",
                          maximum=0.78,
                          details=TRUE,
                          negDashed=TRUE)

dpi=600    #pixels per square inch
jpeg(paste0(plotPrefix, "SupplementalFigure8.jpeg"), width=8*dpi, height=4*dpi, res=dpi)
par(mfcol = c(1, 2))
plot(R2_network_Pre)
plot(R2_network_Post)
dev.off()

# Aim 2
# Change network
R2_network_SR <- qgraph(SRNetwork,
                        layout = "circular",
                        labels = labels_SR,
                        theme="colorblind",
                        pie = as.numeric(as.character(pred_obj_SR$error[,2])), 
                        pieColor = rep('#377EB8', 6),
                        details=TRUE,
                        negDashed=TRUE)

jpeg(paste0(plotPrefix, "SupplementalFigure9.jpeg"), width=4*dpi, height=4*dpi, res=dpi)
plot(R2_network_SR)
dev.off()
# Edge Weight Matrices
WeightMatrix_Pre
WeightMatrix_Post

WeightMatrix_SR <- getWmat(SRNetwork)
WeightMatrix_SR

# Means and SD's for all speech features
# Get names
myvars <- c("NA_REACTIVITY", 
            "DELTA_F0SEMITONEFROM27.5HZ_SMA3NZ_AMEAN", 
            "DELTA_HNRDBACF_SMA3NZ_AMEAN", 
            "DELTA_JITTERLOCAL_SMA3NZ_AMEAN", 
            "DELTA_MEANVOICEDSEGMENTLENGTHSEC", 
            "DELTA_F1F2RATIO",
            "DELTA_VOICEDSEGMENTSPERSEC") 
myvarspre <- c("PRE_F0SEMITONEFROM27.5HZ_SMA3NZ_AMEAN", 
               "PRE_HNRDBACF_SMA3NZ_AMEAN", 
               "PRE_JITTERLOCAL_SMA3NZ_AMEAN", 
               "PRE_MEANVOICEDSEGMENTLENGTHSEC", 
               "PRE_F1F2RATIO",
               "PRE_VOICEDSEGMENTSPERSEC") 
myvarspost <- c("POST_F0SEMITONEFROM27.5HZ_SMA3NZ_AMEAN", 
                "POST_HNRDBACF_SMA3NZ_AMEAN", 
                "POST_JITTERLOCAL_SMA3NZ_AMEAN", 
                "POST_MEANVOICEDSEGMENTLENGTHSEC", 
                "POST_F1F2RATIO",
                "POST_VOICEDSEGMENTSPERSEC") 

Data_SR <- na.omit(FullData[,myvars])
Data_Pre<- na.omit(FullData[,myvarspre])
Data_Post<- na.omit(FullData[,myvarspost])

Data_SR[myvars] <- sapply(Data_SR[myvars],as.numeric)
Data_Pre[myvarspre] <- sapply(Data_Pre[myvarspre],as.numeric)
Data_Post[myvarspost] <- sapply(Data_Post[myvarspost],as.numeric)

# Compute descriptive statistics tables
library(pastecs)
options(scipen=100) # No scientific notation
options(digits=2) # Round to 2 decimals where possible

descriptives_PRE <- stat.desc(Data_Pre, basic = F)
colnames(descriptives_PRE) = c("F0","HNR","JIT","V0","F1/2","RATE")

descriptives_POST <- stat.desc(Data_Post, basic = F)
colnames(descriptives_POST) = c("F0","HNR","JIT","V0","F1/2","RATE")

descriptives_DELTA <- stat.desc(Data_SR, basic = F)
colnames(descriptives_DELTA) = c("NA","F0","HNR","JIT","V0","F1/2","RATE")

descriptives_PRE
descriptives_POST
descriptives_DELTA
##############
###############################
#### 3. Correlations ####
savePlot <- function(plotFunc, filename, widthval = 2000, heightval = 1900) {
  png(filename = paste0(plotPrefix, filename, ".png"), width = widthval, height = heightval, units = "px", res = 300)
  plotFunc()
  dev.off()
}
###############################
# Load the corrplot library
library(corrplot)

# Change df's
Data_Pre <- data.frame(lapply(Data_Pre, as.numeric))
Data_Post <- data.frame(lapply(Data_Post, as.numeric))
colnames(Data_Pre) <- c("F0", "HNR", "Jitter", "MeanVoicedSeg", "F1/2Ratio")
colnames(Data_Post) <- c("F0", "HNR", "Jitter", "MeanVoicedSeg", "F1/2Ratio")

# Pre
correlation_matrix_pre <- cor(Data_Pre)
# Calculate p-values
p.mat_pre <- cor.mtest(Data_Pre)$p
# Create a function for the correlation plot
plotFunc <- function() {
  corrplot(correlation_matrix_pre, method = "circle", p.mat = p.mat_pre, type = 'lower', insig='blank', order = 'AOE', diag = FALSE)
  p1_pre <- corrplot(correlation_matrix_pre, method = "circle", p.mat = p.mat_pre, type = 'lower', insig='blank', order = 'AOE', diag = FALSE)$corrPos
  text(p1_pre$x, p1_pre$y, round(p1_pre$corr, 2))
}

# Save plot
savePlot(plotFunc, "corrPlot_Pre")



# Post
correlation_matrix_post <- cor(Data_Post)
# Calculate p-values
p.mat_post <- cor.mtest(Data_Post)$p
# Create a function for the correlation plot
plotFunc <- function() {
  corrplot(correlation_matrix_post, method = "circle", p.mat = p.mat_post, type = 'lower', insig='blank', order = 'AOE', diag = FALSE)
  p1_post <- corrplot(correlation_matrix_post, method = "circle", p.mat = p.mat_post, type = 'lower', insig='blank', order = 'AOE', diag = FALSE)$corrPos
  text(p1_post$x, p1_post$y, round(p1_post$corr, 2))
}

# Save plot
savePlot(plotFunc, "corrPlot_Post")

##### Session Info #
sessionInfo()