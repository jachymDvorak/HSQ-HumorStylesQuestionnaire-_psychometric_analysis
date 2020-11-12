###############################################################
###############################################################
###############################################################
# Final project - NMST570, ZS 2018/2019 #######################
# Author: Jáchym Dvoøák, 29.1.2019 ############################
###############################################################
###############################################################
###############################################################
# Note: dataset provided via e-mail & downloadable here:
# https://openpsychometrics.org/_rawdata/ (HSQ)
###############################################################
#--------------------------------------------------------------
# Load packages
#--------------------------------------------------------------

library(tidyverse)
library(psych)
library(mirt)
library(Hmisc)
library(psychometric)
library(ShinyItemAnalysis)
library(data.table)
library(difNLR)
library(corrplot)
library(gridExtra)
library(mirt)
library(difR)
library(ltm)
library(lavaan)
library(knitr)
library(kableExtra)

#--------------------------------------------------------------
# Basic operations
#--------------------------------------------------------------

setwd("C:/Users/Jachym/Documents")
df <- read.csv("~/data.csv")
attach(df)

# whole sample descriptive statistics

summary(df)
View(df)

# marking missing data, originally marked as "-1", as NA

df <- df %>% 
  mutate_all(funs(replace(., .< 0, NA)))

# omitting missing data (due to possible issues with algorhitms + most responents missed
# more than one question)

df <- na.omit(df)

# reversing reverse scored items

rev.items <- c("Q1","Q7","Q9","Q15","Q16","Q17","Q22","Q23","Q25","Q29","Q31")
df[,rev.items] <- 6 - df[,rev.items]


#--------------------------------------------------------------
# splitting dataset into 4 scales
#--------------------------------------------------------------
# Note: the original dataset has mistakenly counted total scores
# - it does not correctly reverse items. Therefore I drop these variables.

df <- dplyr::select(df, -c(33, 34, 35, 36))

## Affiliative humor

af.items <- c("Q1","Q5","Q9","Q13","Q17","Q21","Q25","Q29")
af.h <- df[,af.items]

## Self-Enhancing humor

se.items <- c("Q2","Q6","Q10","Q14","Q18","Q22","Q26","Q30")
se.h <- df[,se.items]

## Aggressive humor

ag.items <- c("Q3","Q7","Q11","Q15","Q19","Q23","Q27","Q31")
ag.h <- df[,ag.items]

## Self-defeating humor

sd.items <- c("Q4","Q8","Q12","Q16","Q20","Q24","Q28","Q32")
sd.h <- df[,sd.items]

#--------------------------------------------------------------
# Descriptive statistics
#--------------------------------------------------------------

# calculating total scores

df$totalAF <- apply(df[,af.items], 1, sum)
df$totalSE <- apply(df[,se.items], 1, sum)
df$totalAG <- apply(df[,ag.items], 1, sum)
df$totalSD <- apply(df[,sd.items], 1, sum)

# calculating descriptive statistics

af.h.desc <- base::round(c(min(df$totalAF), max(df$totalAF), mean(df$totalAF), 
                median(df$totalAF), sd(df$totalAF)),3)
se.h.desc <- base::round(c(min(df$totalSE), max(df$totalSE), mean(df$totalSE), 
                median(df$totalSE), sd(df$totalSE)),3)
ag.h.desc <- base::round(c(min(df$totalAG), max(df$totalAG), mean(df$totalAG), 
                median(df$totalAG), sd(df$totalAG)),3)
sd.h.desc <- base::round(c(min(df$totalSD), max(df$totalSD), mean(df$totalSD), 
                median(df$totalSD), sd(df$totalSD)),3)

# Creating a table of descriptive statistics

desc_all <- t(cbind(af.h.desc, se.h.desc, ag.h.desc, sd.h.desc))
colnames(desc_all) <- c("Minimum","Maximum","Mean","Median","SD")
rownames(desc_all) <- c("Affiliative humor","Self-Enhancing humor",
                        "Aggressive humor","Self-Defeating humor")
desc_all %>%
  kable() %>%
  kable_styling()

# Percentiles for each scale

df$percentileAF <- base::round(ecdf(df$totalAF)(df$totalAF),2)*100
df$percentileSE <- base::round(ecdf(df$totalSE)(df$totalSE),2)*100
df$percentileAG <- base::round(ecdf(df$totalAG)(df$totalAG),2)*100
df$percentileSD <- base::round(ecdf(df$totalSD)(df$totalSD),2)*100

### Plotting total scores on a histogram

theme_set(theme_classic())

h1 <- ggplot(df, aes(totalAF)) + 
  geom_bar(width = 0.5, fill = "#00ba38") + 
  theme(axis.text.x = element_text(vjust=0.6)) + 
  labs(title="Affiliative")

h2 <- ggplot(df, aes(totalSE)) + 
  geom_bar(width = 0.5, fill = "#00ba38") + 
  theme(axis.text.x = element_text(vjust=0.6)) + 
  labs(title="Self-enhancing")

h3 <- ggplot(df, aes(totalAG)) + 
  geom_bar(width = 0.5, fill = "#00ba38") + 
  theme(axis.text.x = element_text(vjust=0.6)) + 
  labs(title="Aggressive")

h4 <- ggplot(df, aes(totalSD)) + 
  geom_bar(width = 0.5, fill = "#00ba38") + 
  theme(axis.text.x = element_text(vjust=0.6)) + 
  labs(title="Self-defeating")

# Plotting all histograms in one grid

grid.arrange(h1,h2,h3,h4, nrow = 2) # GRID WITH ALL HISTOGRAMS

# Testing normality

attach(df)
shapiro.test(totalAF)
shapiro.test(totalSE)
shapiro.test(totalAG)
shapiro.test(totalSD)

#--------------------------------------------------------------
# Frequency of answers on a likert scale
#--------------------------------------------------------------

# Plotting frequency of answers on the whole test

all.freq <- gather(df[,1:32])
ggplot(all.freq, aes(x=value)) + 
  geom_histogram(aes(y=..density..), fill = "#00ba38", binwidth = 0.5) +
                       scale_y_continuous(labels = scales::percent)

### Plotting frequency of answers on each scale

remove <- colnames(df[,33:39])

# Affiliative

af.freq <- gather(dplyr::select(df, -remove)[,af.items])
f1 <- ggplot(af.freq, aes(x=value)) + 
  geom_histogram(aes(y=..density..), fill = "#00ba38", binwidth = 0.5) +
  scale_y_continuous(labels = scales::percent) + ggtitle("Affiliative")

# S-E

se.freq <- gather(dplyr::select(df, -remove)[,se.items])
f2 <- ggplot(se.freq, aes(x=value)) + 
  geom_histogram(aes(y=..density..), fill = "#00ba38", binwidth = 0.5) +
  scale_y_continuous(labels = scales::percent) + ggtitle("Self-enhancing")

# Aggressive

ag.freq <- gather(dplyr::select(df, -remove)[,ag.items])
f3 <- ggplot(ag.freq, aes(x=value)) + 
  geom_histogram(aes(y=..density..), fill = "#00ba38", binwidth = 0.5) +
  scale_y_continuous(labels = scales::percent) + ggtitle("Aggressive")

# S-D

sd.freq <- gather(dplyr::select(df, -remove)[,sd.items])
f4 <- ggplot(sd.freq, aes(x=value)) + 
  geom_histogram(aes(y=..density..), fill = "#00ba38", binwidth = 0.5) +
  scale_y_continuous(labels = scales::percent) + ggtitle("Self-defeating")

# Grid of all frequency distributions for all scales 

grid.arrange(f1,f2,f3,f4, nrow = 2)

# Individual item answer frequency per scale

hist(af.h)
hist(se.h)
hist(ag.h)
hist(sd.h)

#--------------------------------------------------------------
# Internal consistency 
#--------------------------------------------------------------

### Calculating p-values of internal correlations 

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# Calculating matrixes of the p-values of the correlations for all scales

af.h.p <- cor.mtest(af.h)
se.h.p <- cor.mtest(se.h)
ag.h.p <- cor.mtest(ag.h)
sd.h.p <- cor.mtest(sd.h)

### Correlation structure for all four scales

col3 <- colorRampPalette(c("red", "white", "blue")) 

cor.mat.af <- base::round(cor(af.h, use = "complete.obs"),3)
rcorr(cor.mat.af)
c1 <- corrplot(cor.mat.af, method="color",  
               type="upper", addCoef.col = "black", col = col3(200),
               tl.col="black", tl.srt=45,
               p.mat = af.h.p, sig.level = 0.01, insig = "blank",diag=FALSE,
              title = "Affiliative")

cor.mat.se <- base::round(cor(se.h, use = "complete.obs"),3)
rcorr(cor.mat.se)
c2 <- corrplot(cor.mat.se, method="color",col = col3(200),
               type="upper", addCoef.col = "black",
               tl.col="black", tl.srt=45,
               p.mat = se.h.p, sig.level = 0.01, insig = "blank",diag=FALSE,
              title = "Self-enhancing")

cor.mat.ag <- base::round(cor(ag.h, use = "complete.obs"),3)
rcorr(cor.mat.ag)
c3 <- corrplot(cor.mat.ag, method="color", col = col3(200),
               type="upper", addCoef.col = "black",
               tl.col="black", tl.srt=45,
               p.mat = ag.h.p, sig.level = 0.01, insig = "blank", diag=FALSE)

cor.mat.sd <- base::round(cor(sd.h, use = "complete.obs"),3)
rcorr(cor.mat.sd)
c4 <- corrplot(cor.mat.sd, method="color", col = col3(200), 
               type="upper", addCoef.col = "black",
               tl.col="black", tl.srt=45,
               p.mat = sd.h.p, sig.level = 0.01, insig = "blank", diag=FALSE,
               title = "Self-defeating")

### Calculating correlations between scales

all.total <- cbind(af.h.total,se.h.total,ag.h.total,sd.h.total)
colnames(all.total) <- c("Affiliative", "Self-enhancing","Aggressive", "Self-defeating")
total.p <- cor.mtest(all.total)
cor.mat.total <- base::round(cor(all.total, use = "complete.obs"),3)
rcorr(cor.mat.total)
cormat.all <- corrplot(cor.mat.total, method="color",col = col3(200),
                       type="upper", addCoef.col = "black",
                       tl.col="black", tl.srt=45,
                       p.mat = total.p, sig.level = 0.01, insig = "blank",diag=FALSE)

#--------------------------------------------------------------
# Reliability 
#--------------------------------------------------------------

### Split-half reliability

# Randomly splitting sample

set.seed(123)
samp <- sample(1:8, 4)

# Calculating random split halves for each scale

af.h.1 <- af.h[, samp]
af.h.2 <- af.h[, setdiff(1:8, samp)]
af_1 <- apply(af.h.1, 1, sum)
af_2 <- apply(af.h.2, 1, sum)

cor(af_1, af_2, use = "complete.obs") # Affiliative correlation

se.h.1 <- se.h[, samp]
se.h.2 <- se.h[, setdiff(1:8, samp)]
se_1 <- apply(se.h.1, 1, sum)
se_2 <- apply(se.h.2, 1, sum)

cor(se_1, se_2, use = "complete.obs") # S-E correlation

ag.h.1 <- ag.h[, samp]
ag.h.2 <- ag.h[, setdiff(1:8, samp)]
ag_1 <- apply(ag.h.1, 1, sum)
ag_2 <- apply(ag.h.2, 1, sum)

cor(ag_1, ag_2, use = "complete.obs") # Aggressive correlation

sd.h.1 <- sd.h[, samp]
sd.h.2 <- sd.h[, setdiff(1:8, samp)]
sd_1 <- apply(sd.h.1, 1, sum)
sd_2 <- apply(sd.h.2, 1, sum)

cor(sd_1, sd_2, use = "complete.obs") # S-D correlation

## Spearman-Brown formulas to adjust for test length for each scale

sb.1 <- 2*cor(af_1, af_2, use = "complete.obs")/(1 + cor(af_1, af_2, use = "complete.obs"))
sb.2 <- 2*cor(se_1, se_2, use = "complete.obs")/(1 + cor(se_1, se_2, use = "complete.obs"))
sb.3 <- 2*cor(ag_1, ag_2, use = "complete.obs")/(1 + cor(ag_1, ag_2, use = "complete.obs"))
sb.4 <- 2*cor(sd_1, sd_2, use = "complete.obs")/(1 + cor(sd_1, sd_2, use = "complete.obs"))

## Calculating all possible split halves & their means

all.af <- splitHalf(af.h, raw = TRUE, brute = TRUE)
all.1 <- mean(all.af$raw)

all.se <- splitHalf(se.h, raw = TRUE, brute = TRUE)
all.2 <- mean(all.se$raw)

all.ag <- splitHalf(ag.h, raw = TRUE, brute = TRUE)
all.3 <- mean(all.ag$raw)

all.sd <- splitHalf(sd.h, raw = TRUE, brute = TRUE)
all.4 <- mean(all.sd$raw)

### Cronbach's alpha for all scales

af.alpha <- psychometric::alpha(af.h)
psychometric::alpha.CI(af.alpha, N = nrow(af.h), k = ncol(af.h), level = 0.95)

se.alpha <- psychometric::alpha(se.h)
psychometric::alpha.CI(se.alpha, N = nrow(se.h), k = ncol(se.h), level = 0.95)

ag.alpha <- psychometric::alpha(ag.h)
psychometric::alpha.CI(ag.alpha, N = nrow(ag.h), k = ncol(ag.h), level = 0.95)

sd.alpha <- psychometric::alpha(sd.h)
psychometric::alpha.CI(sd.alpha, N = nrow(sd.h), k = ncol(sd.h), level = 0.95)

### Creating a table of all reliability estimates 

# creating table

sb.all <- rbind(sb.1, sb.2, sb.3, sb.4)
rownames(sb.all) <- c("Affiliative", "Self-enhancing", "Aggressive", "Self-defeating")
all.all <- rbind(all.1, all.2, all.3, all.4)
alphas <- rbind(af.alpha, se.alpha, ag.alpha, sd.alpha)

rel.table <- cbind(sb.all, all.all, alphas) 
colnames(rel.table) <- c("Random split-half", "All split-halves", "Cronbach's alpha")

rel.table %>%
  kable() %>%
  kable_styling()

#--------------------------------------------------------------
# Traditional item analysis for polytomous data 
#--------------------------------------------------------------
# Note: to compare binarized data, use the "bin = T" argument in the DDplot function

## Affiliative 

dd1 <- DDplot(af.h, k = 3, l = 1, u = 3)

TIA.table.af <- base::round(data.frame(item.exam(af.h, discr = TRUE)[, c(4, 1, 5, 2, 3)], 
                        psych::alpha(af.h)$alpha.drop[, 1], 
                        gDiscrim(af.h, k = 3, l = 1, u = 3)), 2) 
colnames(TIA.table.af) <- c("Difficulty", "SD", "Discrimination ULI", 
                         "Discrimination RIT", "Discrimination RIR", 
                         "Alpha Drop", "Customized Discrimination")
TIA.table.af <- cbind("Scale" = "AF", TIA.table.af)

## S-E 

dd2 <-DDplot(se.h, k = 3, l = 1, u = 3)

TIA.table.se <- base::round(data.frame(item.exam(se.h, discr = TRUE)[, c(4, 1, 5, 2, 3)], 
                                 psych::alpha(se.h)$alpha.drop[, 1], 
                                 gDiscrim(se.h, k = 3, l = 1, u = 3)), 2) 
colnames(TIA.table.se) <- c("Difficulty", "SD", "Discrimination ULI", 
                            "Discrimination RIT", "Discrimination RIR", 
                            "Alpha Drop", "Customized Discrimination")
TIA.table.se <- cbind("Scale" = "SE", TIA.table.se)

## Aggressive 

dd3 <- DDplot(ag.h, k = 3, l = 1, u = 3)

TIA.table.ag <- base::round(data.frame(item.exam(ag.h, discr = TRUE)[, c(4, 1, 5, 2, 3)], 
                                 psych::alpha(ag.h)$alpha.drop[, 1], 
                                 gDiscrim(ag.h, k = 3, l = 1, u = 3)), 2) 
colnames(TIA.table.ag) <- c("Difficulty", "SD", "Discrimination ULI", 
                            "Discrimination RIT", "Discrimination RIR", 
                            "Alpha Drop", "Customized Discrimination")
TIA.table.ag <- cbind("Scale" = "AG", TIA.table.ag)

## S-D

dd4 <- DDplot(sd.h, k = 3, l = 1, u = 3)

TIA.table.sd <- base::round(data.frame(item.exam(sd.h, discr = TRUE)[, c(4, 1, 5, 2, 3)], 
                                 psych::alpha(sd.h)$alpha.drop[, 1], 
                                 gDiscrim(sd.h, k = 3, l = 1, u = 3)), 2) 
colnames(TIA.table.sd) <- c("Difficulty", "SD", "Discrimination ULI", 
                            "Discrimination RIT", "Discrimination RIR", 
                            "Alpha Drop", "Customized Discrimination")
TIA.table.sd <- cbind("Scale" = "SD", TIA.table.sd)

# AG tables

TIA.table.af %>%
  kable() %>%
  kable_styling()

TIA.table.se %>%
  kable() %>%
  kable_styling()

TIA.table.ag %>%
  kable() %>%
  kable_styling()

TIA.table.sd %>%
  kable() %>%
  kable_styling()

## All graphs in one grid

grid.arrange(dd1,dd2, dd3, dd4, nrow = 2)

## All TIA tables in one

TIA.tables <- rbind(TIA.table.af, TIA.table.se, TIA.table.ag, TIA.table.sd)
TIA.tables %>%
  kable() %>%
  kable_styling()

#--------------------------------------------------------------
# Polytomous models
#--------------------------------------------------------------

# constrain (all 8 items in each scale measure one latent trait)

c <- 'F = 1-8
CONSTRAIN = (1-8, a1)'

### Affiliative

# deciding for best model (GRM/GPCM, constricted/unconstricted)

GPCM_af.constr <- mirt(af.h, model = c, itemtype = "gpcm", SE = T)
GPCM_af <- mirt(af.h, model = 1, itemtype = "gpcm", SE = T)
GRM_af.constr <- mirt(af.h, model = c, itemtype = "graded", SE = T)
GRM_af <- mirt(af.h, model = 1, itemtype = "graded", SE = T)

an1.af <- anova(GRM_af, GRM_af.constr)
rownames(an1.af) <- c("GRM unconstricted", "GRM constricted")
an2.af <- anova(GPCM_af.constr, GPCM_af)
rownames(an2.af) <- c("GPCM unconstricted", "GPCM constricted")

poly_anova_af <- rbind(an1.af, an2.af)
poly_anova_af[,c(1, 2, 3, 5)] %>%
  kable() %>%
  kable_styling()

# plotting & summary of better model

summary(GRM_af.constr)
coef(GRM_af.constr, simplify = T)
plot(GRM_af.constr, type = "trace") # Category response curve
plot(GRM_af.constr, type = "infotrace", facet_items = F) # Item information curve
plot(GRM_af.constr, type = "infoSE", facet_items = F) # Test information curve

## S-E

# deciding for best model (GRM/GPCM, constricted/unconstricted)

GPCM_se.constr <- mirt(se.h, model = c, itemtype = "gpcm", SE = T)
GPCM_se <- mirt(se.h, model = 1, itemtype = "gpcm", SE = T)
GRM_se.constr <- mirt(se.h, model = c, itemtype = "graded", SE = T)
GRM_se <- mirt(se.h, model = 1, itemtype = "graded", SE = T)

an1.se <- anova(GRM_se, GRM_se.constr)
rownames(an1.se) <- c("GRM unconstricted", "GRM constricted")
an2.se <- anova(GPCM_se.constr, GPCM_se)
rownames(an2.se) <- c("GPCM unconstricted", "GPCM constricted")

poly_anova_se <- rbind(an1.se, an2.se)
poly_anova_se[,c(1, 2, 3, 5)] %>%
  kable() %>%
  kable_styling()

# plotting & summary of better model

summary(GRM_se.constr)
coef(GRM_se.constr, simplify = T)
plot(GRM_se.constr, type = "trace") # Category response curve
plot(GRM_se.constr, type = "infotrace", facet_items = F) # Item information curve
plot(GRM_se.constr, type = "infoSE", facet_items = F) # Test information curve

## Aggressive

# deciding for best model (GRM/GPCM, constricted/unconstricted)

GPCM_ag.constr <- mirt(ag.h, model = c, itemtype = "gpcm", SE = T)
GPCM_ag <- mirt(ag.h, model = 1, itemtype = "gpcm", SE = T)
GRM_ag.constr <- mirt(ag.h, model = c, itemtype = "graded", SE = T)
GRM_ag <- mirt(ag.h, model = 1, itemtype = "graded", SE = T)

an1.ag <- anova(GRM_ag, GRM_ag.constr)
rownames(an1) <- c("GRM unconstricted", "GRM constricted")
an2.ag <- anova(GPCM_ag.constr, GPCM_ag)
rownames(an2) <- c("GPCM unconstricted", "GPCM constricted")

poly_anova_ag <- rbind(an1.ag, an2.ag)
poly_anova_ag[,c(1, 2, 3, 5)] %>%
  kable() %>%
  kable_styling()

# plotting & summary of better model

summary(GRM_ag.constr)
coef(GRM_ag.constr, simplify = T)
plot(GRM_ag.constr, type = "trace") # Category response curve
plot(GRM_ag.constr, type = "infotrace", facet_items = F) # Item information curve
plot(GRM_ag.constr, type = "infoSE", facet_items = F) # Test information curve
                            
## S-D

# deciding for best model (GRM/GPCM, constricted/unconstricted)

GPCM_sd.constr <- mirt(sd.h, model = c, itemtype = "gpcm", SE = T)
GPCM_sd <- mirt(sd.h, model = 1, itemtype = "gpcm", SE = T)
GRM_sd.constr <- mirt(sd.h, model = c, itemtype = "graded", SE = T)
GRM_sd <- mirt(sd.h, model = 1, itemtype = "graded", SE = T)

an1.sd <- anova(GRM_sd, GRM_sd.constr)
rownames(an1.sd) <- c("GRM unconstricted", "GRM constricted")
an2.sd <- anova(GPCM_sd.constr, GPCM_sd)
rownames(an2.sd) <- c("GPCM unconstricted", "GPCM constricted")

poly_anova_sd <- rbind(an1.sd, an2.sd)
poly_anova_sd[,c(1, 2, 3, 5)] %>%
  kable() %>%
  kable_styling()

# plotting & summary of better model

summary(GRM_sd.constr)
coef(GRM_sd.constr, simplify = T)
plot(GRM_sd.constr, type = "trace") # Category response curve
plot(GRM_sd.constr, type = "infotrace", facet_items = F) # Item information curve
plot(GRM_sd.constr, type = "infoSE", facet_items = F) # Test information curve

### Creating a table of parameter estimates & SEs for the AG scale

# Calculating item fit

ag_itemfit <- itemfit(GRM_ag.constr)

# Extracting parameter estimates

par_tab <- coef(GRM_ag.constr, simplify = T)$items 

# Extracting SE estimates

se_list <- coef(GRM_ag.constr, printSE = T) 
se_tab <- t(sapply(1:nrow(par_tab), function(i) se_list[[i]]["SE", ])) 

# Creating table

tab_ag <- cbind(par_tab, se_tab, ag_itemfit) 
tab_ag <- tab_ag[, c(1, 6, 2, 7, 3, 8, 4, 9, 5, 10, 12, 13, 14)] 
colnames(tab_ag) <- c("a", "SE(a)", "b1", "SE(b1)", "b2", "SE(b2)",
                            "b3", "SE(b3)", "b4", "SE(b4)","S_X2 value","df","S_X2 p-value")

tab_ag %>%
  kable() %>%
  kable_styling() 

#--------------------------------------------------------------
# Binarized IRT models
#--------------------------------------------------------------

### binarization ###

df.b <- df
df.b[, 1:32] <- (df.b[, 1:32] > 3) + 0

# Affiliative humor

af.h.b <- df.b[,af.items]

# Self-Enhancing humor

se.h.b <- df.b[,se.items]

# Aggressive humor

ag.h.b <- df.b[,ag.items]

# Self-defeating humor

sd.h.b <- df.b[,sd.items]


### Best model estimation

s <- paste("F = 1-", ncol(af.h.b), "\n",
           "CONSTRAIN = (1-", ncol(af.h.b), ", a1)")
model <- mirt.model(s)

# Fitting models

fit1PL <- mirt(af.h.b, model = model, itemtype = "2PL")
fit2PL <- mirt(af.h.b, model = 1, itemtype = "2PL")
fit3PL <- mirt(af.h.b, model = 1, itemtype = "3PL") 
fit4PL <- mirt(af.h.b, model = 1, itemtype = "4PL") 

# Comparing models

anova(fit1PL, fit2PL) 
anova(fit2PL, fit3PL) 
anova(fit3PL, fit4PL)
anova(fit2PL, fit4PL)

# Creating a table to compare models

IRT.ag1 <- anova(fit1PL, fit2PL) 
rownames(IRT.ag1) <- c("1PL", "2PL")
IRT.ag2 <- anova(fit3PL, fit4PL)
rownames(IRT.ag2) <- c("3PL", "4PL")

IRT_anova_ag <- rbind(IRT.ag1, IRT.ag2)
IRT_anova_ag[,c(1, 2, 3, 5)] %>%
  kable() %>%
  kable_styling()

# Some criteria prefers the 2PL model, some the 3PL model. 
# Due to the nature of the data (personality test) I decided
# for the 2PL model.

### IRT 2PL Models ###

af.h.2PL <- mirt(af.h.b, model = 1, itemtype = "2PL", SE = T)
se.h.2PL <- mirt(se.h.b, model = 1, itemtype = "2PL", SE = T) 
ag.h.2PL <- mirt(ag.h.b, model = 1, itemtype = "2PL", SE = T) 
sd.h.2PL <- mirt(sd.h.b, model = 1, itemtype = "2PL", SE = T) 

# Item characteristic curves 

plot(af.h.2PL, type = "trace", facet_items = F)
plot(se.h.2PL, type = "trace", facet_items = F) 
plot(ag.h.2PL, type = "trace", facet_items = F) 
plot(sd.h.2PL, type = "trace", facet_items = F) 

# Item information curves 

plot(af.h.2PL, type = "infotrace", facet_items = F)
plot(se.h.2PL, type = "infotrace", facet_items = F) 
plot(ag.h.2PL, type = "infotrace", facet_items = F) 
plot(sd.h.2PL, type = "infotrace", facet_items = F) 

# Test information curve 

plot(af.h.2PL, type = "infoSE") 
plot(se.h.2PL, type = "infoSE") 
plot(ag.h.2PL, type = "infoSE") 
plot(sd.h.2PL, type = "infoSE") 

# Coefficients 

coef(af.h.2PL, IRTpars = TRUE, simplify = TRUE) 
coef(se.h.2PL, IRTpars = TRUE, simplify = TRUE) 
coef(ag.h.2PL, IRTpars = TRUE, simplify = TRUE) 
coef(sd.h.2PL, IRTpars = TRUE, simplify = TRUE) 

# Item fit statistics 

itemfit(af.h.2PL)
itemfit(se.h.2PL) 
itemfit(ag.h.2PL) 
itemfit(sd.h.2PL) 

### Wright maps ### 

af.h.Rasch <- mirt(af.h.b, model = 1, itemtype = "Rasch")
fsc1 <- as.vector(fscores(af.h.Rasch)) 
d1 <- coef(af.h.Rasch, simplify = T)$items[, "d"] 
ggWrightMap(fsc1, d1,color = "#00ba38")

se.h.Rasch <- mirt(se.h.b, model = 1, itemtype = "Rasch")
fsc2 <- as.vector(fscores(se.h.Rasch)) 
d2 <- coef(se.h.Rasch, simplify = T)$items[, "d"] 
ggWrightMap(fsc2, d2,color = "#00ba38")

ag.h.Rasch <- mirt(ag.h.b, model = 1, itemtype = "Rasch")
fsc3 <- as.vector(fscores(ag.h.Rasch)) 
d3 <- coef(ag.h.Rasch, simplify = T)$items[, "d"] 
ggWrightMap(fsc3, d3, color = "#00ba38")

sd.h.Rasch <- mirt(sd.h.b, model = 1, itemtype = "Rasch")
fsc4 <- as.vector(fscores(sd.h.Rasch)) 
d4 <- coef(sd.h.Rasch, simplify = T)$items[, "d"] 
ggWrightMap(fsc4, d4,color = "#00ba38")

### Creating a table of parameter estimates & SEs for the AG scale

# Calculating item fit

ag_itemfit_2PL <- itemfit(ag.h.2PL)

# Extracting parameter estimates

par_tab_2PL <- coef(ag.h.2PL, simplify = T)$items 

# Extracting SE estimates

se_list_2PL <- coef(ag.h.2PL, printSE = T) 
se_tab_2PL <- t(sapply(1:nrow(par_tab_2PL), function(i) se_list_2PL[[i]]["SE", ])) 

# Creating table

tab_ag_2PL <- cbind(par_tab_2PL, se_tab_2PL, ag_itemfit_2PL) 
tab_ag_2PL <- tab_ag_2PL[, c(1,5,2,6,10,11,12)]
colnames(tab_ag_2PL) <- c("a", "SE(a)", "b", "SE(b)","S_X2 value","df","S_X2 p-value")

tab_ag_2PL %>%
  kable() %>%
  kable_styling() 

### IRT model for specific items

plot(af.h.2PL, type = "trace")
plot(se.h.2PL, type = "trace")
plot(ag.h.2PL, type = "trace")
plot(sd.h.2PL, type = "trace")

#--------------------------------------------------------------
# Unidimensional model
#--------------------------------------------------------------

### constricted GRM model

c <- 'F = 1-32
CONSTRAIN = (1-32, a1)'
uni_GRM
uni_GRM <- mirt(df[, 1:32], model = c, itemtype = "graded", SE = T)

plot(uni_GRM, type = "trace") # Category response curve
plot(uni_GRM, type = "infotrace", facet_items = F) # Item information curve
plot(uni_GRM, type = "infoSE", facet_items = F) # Test information curve

#--------------------------------------------------------------
# DIF items with binarized data 
#--------------------------------------------------------------
# Note: I decided to use two IRL detection methods and not use 
# simpler methods, as these are more sensitive and precise.

# removing genders "0" and "3" to create two groups to compare

data <- df
data <- transform(data, gender = ifelse(gender == 3, NA, gender))
data <- transform(data, gender = ifelse(gender == 0, NA, gender))
data <- na.omit(data)

# recoding women to 0  

data <- transform(data, gender = ifelse(gender == 2, 0, gender))

# creating csv to input to ShinyItemAnalysis app

remove <- colnames(data[,33:39])
all.it <- dplyr::select(data, -remove)
write.csv(all.it[,af.items],"HSQ_af", row.names = F)
write.csv(data$gender,"group", row.names = F)

### dividing into 4 scales ###

## binarization ##

df.b.dif <- data
df.b.dif[, 1:32] <- (df.b.dif[, 1:32] > 3) + 0

## Affiliative humor

af.dif <- df.b.dif[,af.items]

## Self-Enhancing humor

se.dif <- df.b.dif[,se.items]

## Aggressive humor

ag.dif <- df.b.dif[,ag.items]

## Self-defeating humor

sd.dif <- df.b.dif[,sd.items]

###################
### Models ########
###################

group <- data$gender

### Affiliative Humor ###

# Lord

DIF.2PL.af <- difLord(Data = af.dif, group = group, focal.name = 1, 
                      model = "2PL", p.adjust.method = "none", purify = F) 
DIF.2PL.af

# plot

plot(DIF.2PL.af, col = "red")
plot(DIF.2PL.af, col = "red", item = "Q5" , plot = "itemCurve", 
     group.names = c("women","men"))
plot(DIF.2PL.af, col = "red", item = "Q9" , plot = "itemCurve", 
     group.names = c("women","men"))

# Raju

DIF.2PL.Raju.af <- difRaju(Data = af.dif, group = group, focal.name = 1,
                      model = "2PL", p.adjust.method = "none", purify = F) 
DIF.2PL.Raju.af

### S-E Humor ###

# Lord

DIF.2PL.se <- difLord(Data = se.dif, group = group, focal.name = 1, 
                      model = "2PL", p.adjust.method = "none", purify = F) 
DIF.2PL.se

# Raju

DIF.2PL.Raju.se <- difRaju(Data = se.dif, group = group, focal.name = 1,
                     model = "2PL", p.adjust.method = "none", purify = F) 
DIF.2PL.Raju.se

### Aggressive Humor ###

# Lord

DIF.2PL.ag <- difLord(Data = ag.dif, group = group, focal.name = 1, 
                      model = "2PL", p.adjust.method = "none", purify = F) 
DIF.2PL.ag

# plot

plot(DIF.2PL.ag, col = "red")
plot(DIF.2PL.ag, col = "red", item = "Q19" , plot = "itemCurve", 
     group.names = c("women","men"))

# Raju

DIF.2PL.Raju.ag <- difRaju(Data = ag.dif, group = group, focal.name = 1,
                           model = "2PL", p.adjust.method = "none", purify = F) 
DIF.2PL.Raju.ag


### S-D Humor ###

# Lord

DIF.2PL.sd <- difLord(Data = sd.dif, group = group, focal.name = 1, 
                      model = "2PL", p.adjust.method = "none", purify = F) 
DIF.2PL.sd

# Raju

DIF.2PL.Raju.sd <- difRaju(Data = sd.dif, group = group, focal.name = 1,
                           model = "2PL", p.adjust.method = "none", purify = F) 
DIF.2PL.Raju.sd

#--------------------------------------------------------------
# Factor analysis
#--------------------------------------------------------------

# data manipulation

remove <- colnames(df[,33:39])
all.it <- dplyr::select(df, -remove)

### EFA ###

EFA.fit <- factanal(all.it, 4)
print(EFA.fit, digits=2, cutoff=.3, sort=TRUE)


### CFA ###

HS.model <- ' affiliative  =~ Q1 + Q5 + Q9 + Q13 + Q17 + Q21 + Q25 + Q29 
              selfEnhancing =~ Q2 + Q6 + Q10 + Q14 + Q18 + Q22 + Q26 + Q30
              aggressive   =~ Q3 + Q7 + Q11 + Q15 + Q19 + Q23 + Q27 + Q31
              selfDefeating   =~ Q4 + Q8 + Q12 + Q16 + Q20 + Q24 + Q28 + Q32'

CFA.fit <- cfa(HS.model, data=all.it, std.lv=TRUE)
summary(CFA.fit)

#--------------------------------------------------------------
# Differences between groups
#--------------------------------------------------------------

### Gender differences

# t-tests

t.test(apply(data[,af.items], 1, sum) ~ data$gender)
t.test(apply(data[,se.items], 1, sum) ~ data$gender)
t.test(apply(data[,ag.items], 1, sum) ~ data$gender) # SIGNIFICANT p < .001
t.test(apply(data[,sd.items], 1, sum) ~ data$gender) # SIGNIFICANT p < .01

# recoding data

data <- df
data$totalAF <- apply(data[,af.items], 1, sum)
data$totalSE <- apply(data[,se.items], 1, sum)
data$totalAG <- apply(data[,ag.items], 1, sum)
data$totalSD <- apply(data[,sd.items], 1, sum)
bp.data <- transform(data, gender = ifelse(gender == 1, "male", "female"))

length(which(bp.data$gender == "male"))
length(which(bp.data$gender == "female"))

# boxplots

bp1 <- ggplot(bp.data[,c(ag.items,"gender","totalAG")], 
       aes(group= gender, x=gender, y=totalAG, fill = gender)) +
  geom_boxplot() +
  labs(title="AG scale",x="Gender", y = "Total score") +
  theme_classic()

bp2 <- ggplot(bp.data[,c(sd.items,"gender","totalSD")], 
       aes(group= gender, x=gender, y=totalSD, fill = gender)) +
  geom_boxplot() +
  labs(title="SD scale",x="Gender", y = "Total score") +
  theme_classic()
grid.arrange(bp1, bp2, nrow = 1)


### Age differences

data <- data[data$age < 100 & data$age > 10, ]
age.clean <- transform(data, age = ifelse(age > 24, "old", "young"))
age.clean$total <- apply(age.clean[,1:32], 1, sum)

length(which(age.clean$age == "young"))
length(which(age.clean$age == "old"))

# t-tests

t.test(age.clean$total ~ age.clean$age)
t.test(apply(age.clean[,af.items], 1, sum) ~ age.clean$age)
t.test(apply(age.clean[,se.items], 1, sum) ~ age.clean$age)
t.test(apply(age.clean[,ag.items], 1, sum) ~ age.clean$age) # SIGNIFICANT p < .001
t.test(apply(age.clean[,sd.items], 1, sum) ~ age.clean$age)

# boxplot

ggplot(age.clean[,c(ag.items,"age","totalAG")], 
       aes(group= age, x=age, y=totalAG, fill = age)) +
  geom_boxplot() +
  labs(title="AG scale",x="Age", y = "Total score") +
  theme_classic()
