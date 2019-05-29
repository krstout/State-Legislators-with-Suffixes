rm(list = ls())

library(plyr)
library(ggplot2)
library(rdrobust)
library(texreg)
library(stargazer)

setwd("C:\\Users\\Kevin\\Box Sync\\RD State Legislative Dynasties")
d <- read.csv("State Legislative Election Data.csv")

margin <- 10

# Primaries select on close suffix races #

p <- d[d$v16 == "DP" | d$v16 == "RP" | d$v16 == "DPR" | d$v16 == "RPR",]
p <- p[p$v25 > 1,]
p <- p[p$v35 < margin & p$v37 < margin,]
p <- p[p$v12 == 1,]

p <- p[!is.na(p$caseid),]

p$famsuf <- ifelse(p$v51 == " ", 0, 1)

p <- p[p$famsuf == 1,]

# Use identifying info from close elections #
key <- p[c(2:6, 8:10, 21)]

c <- join(d, key, type = "inner")

# Only pull primary winners #
e <- c[c$v16 != "G",]
e <- e[e$v24 == 1,]

# Carry forward primary margin #
e$pmar <- e$v35

key2 <- e[c(2:6, 8:10, 23, 24, 64)]

# Select general elections with more than one candidate running #
key3 <- p[c(2:6, 8:10)]

f <- join(d, key3, type = "inner")

g <- f[f$v17 == 1,]
g <- g[g$v25 > 1,]

h <- unique(g)

h <- join(h, key2)

final <- unique(h)

# Make margin negative if not suffix candidate #

final$famsuf <- ifelse(final$v51 == " ", -1, 1)
final$prange <- final$famsuf*final$pmar
final$cut <- ifelse(final$prange > 0, 1, 0)

final$cut <- as.factor(final$cut)

final <- final[final$pmar < margin,]
final$genwin <- ifelse(final$v24 == 1, 1, -1)
final$genmar <- final$genwin*final$v35

# Determine appropriate margin size #
# From Hainmueller et al. 2016
# When coef on forcing variable (prange in this case) is significant, 
# you've reached the end of the range you can estimate.
# I don't reach the edge with 15 pt. margin but that could be due to underpower?

bin1 <- final[final$prange < 5 & final$prange > 0,]
bin2 <- final[final$prange > -5 & final$prange < 0,]
bin3 <- final[final$prange < 10 & final$prange > 0,]
bin4 <- final[final$prange > -10 & final$prange < 0,]
bin5 <- final[final$prange < 15 & final$prange > 0,]
bin6 <- final[final$prange > -15 & final$prange < 0,]

rbin1 <- lm(v36 ~ prange + v03 + v05 + v07 + v21 + v22, data = bin1)
summary(rbin1)

rbin2 <- lm(v36 ~ prange + v03 + v05 + v07 + v21 + v22, data = bin2)
summary(rbin2)

rbin3 <- lm(v36 ~ prange + v03 + v05 + v07 + v21 + v22, data = bin3)
summary(rbin3)

rbin4 <- lm(v36 ~ prange + v03 + v05 + v07 + v21 + v22, data = bin4)
summary(rbin4)

rbin5 <- lm(v36 ~ prange + v03 + v05 + v07 + v21 + v22, data = bin5)
summary(rbin5)

rbin6 <- lm(v36 ~ prange + v03 + v05 + v07 + v21 + v22, data = bin6)
summary(rbin6)

# Plot #

plot <- ggplot(final, aes(final$prange, final$v36, shape = final$cut)) + geom_point(size = 2) +
                geom_smooth(method = "lm", fullrange = F) +
                geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
                ggtitle("Plot of the Data Near Discontinuity") +
                xlab("Primary Election Margin of Victory for Suffix Candidate") +
                ylab("General Election Vote Percentage") +
                theme_bw() +
                guides(shape = guide_legend(title = "Suffix\nCandidate", reverse = T))
plot

#ggsave(file = "Figure 1.pdf", width = 6, height = 4.5)
# Export as PDF in 4 x 6 in. dimensions

### RD Estimate ###

keepers <- c("v36", "prange", "v03", "v05", "v07", "v19", "v21", "v22", "genmar", "cut")
final <- final[keepers]
final <- final[complete.cases(final),]

rplot <- rdplot(final$genmar, final$prange)

model <- rdrobust(final$v36, final$prange)
summary(model)

omo <- lm(v36 ~ prange + cut, data = final)
summary(omo)

write.csv(final, "RD Data.csv", row.names = F)

# Regular Regression #

rdat <- d[d$v17 == 1,]
rdat <- rdat[rdat$v12 == 1,]
rdat <- rdat[rdat$v25 > 1,]
rdat$famsuf <- ifelse(rdat$v51 == " ", 0, 1)
rdat$senate <- ifelse(rdat$v07 == 9, 1, 0)

linear <- lm(v36 ~ famsuf + as.factor(v02) + as.factor(v05) + senate + as.factor(v21) + v22, data = rdat)
summary(linear)

htmlreg(linear, "Regular Regression.doc", omit.coef = "as.factor", digits = 3)
texreg(linear, "OLS Regression.tex", omit.coef = "as.factor", digits = 3)


### EXTRA CODE ###

# Primary Elections #
dprim <- d[d$v02 == "KY" | d$v02 == "MD" | d$v02 == "MO" | d$v02 == "TN" | d$v02 == "VA" | d$v02 == "WV",]
dprim <- dprim[dprim$v16 == "DP" | dprim$v16 == "RP",]
dprim <- dprim[dprim$v25 > 1,]
dprim <- dprim[dprim$v35 < 5 & dprim$v37 < 5,]
dprim <- dprim[dprim$v12 == 1,]

dprim <- dprim[!is.na(dprim$caseid),]

dprim$famsuf <- ifelse(dprim$v51 == " ", 0, 1)
table(dprim$famsuf)

write.csv(dprim, "Primary Elections.csv", row.names = F)

# General Election #
dsub <- d[d$v17 == 1,]
# Single Member District #
dsub <- dsub[dsub$v12 == 1,]
# More than one Candidate #
dsub <- dsub[dsub$v25 > 1,]
# Margin of Victory for Election #
dsub <- dsub[dsub$v35 < .5,]
# Margin of Victory for Candidate #
dsub <- dsub[dsub$v37 < .5,]

summary(dsub$v35)
summary(dsub$v37)

# Family Suffix Dummy #
dsub$famsuf <- ifelse(dsub$v51 == " ", 0, 1)
table(dsub$famsuf)

dprim$famsuf <- ifelse(dprim$v51 == " ", 0, 1)
table(dprim$famsuf)

table(dsub$famsuf, dsub$v24)



# Just use states without runoffs #
drun <- d[d$v16 == "DPR" | d$v16 == "RPR",]
summary(drun$v02)
dnorun <- d[d$v16 == "DP" | d$v16 == "RP",]
summary(dnorun$v02)
# Those states are: KY, MD, MO, TN, VA, WV