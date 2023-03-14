# UNIVARIATE ANALYSES OF FISH INJURIES AFTER PASSAGE THROUGH A HYDROPOWER TURBINE.

install.packages("ggplot2")
install.packages("dplyr")
install.packages("plotly")
library(ggplot2)
library(dplyr)
library(plotly)

# Loading data
setwd("F:/Office/R_Python_Scripts/hydroturbine fish injury analyses")
getwd()

abiotik1 <- read.table("2022-01-18 Fish Injuries and Hydropower Turbine Variables.csv", dec = ".", sep = ",", header = TRUE)
vit1 <- read.table("2022-01-18 Fish Injuries and Hydropower Turbine Variables.csv", dec = ".", sep = ",", header = TRUE)

head(abiotik1)
head(vit1)
colnames(abiotik1)
colnames(vit1)

# Fallh.he..m.
# Umfangsgeschwindigkeit..m.s.
# RPM
# Anzahl.Schaufeln

# Schwimmblase_F.llung       
# Kum.Amputationen          
# Kum.StauchungenVerformungen 
# Kum.Frakturen               
# Kum.InnereGasblasen

# Prepare data
# Select treatment type - Only OR and TURBINE - 
abiotik1.OR <- abiotik1[abiotik1$Treatment == "OR", ]
abiotik1.OR.TUR <- rbind(abiotik1.OR, abiotik1[abiotik1$Treatment == "TURB", ])
nrow(abiotik1.OR.TUR)
head(abiotik1.OR.TUR)

abiotik1.OR.TUR$Treatment <- as.character(abiotik1.OR.TUR$Treatment)
abiotik1.OR.TUR$Treatment <- as.factor(abiotik1.OR.TUR$Treatment)
levels(abiotik1.OR.TUR$Treatment)

levels(abiotik1.OR.TUR$Artname) = c("Aal", "Bachforelle", "Barbe", "Barsch","Huchen","Nase","Rotauge","Äsche")
levels(abiotik1.OR.TUR$Artname)

vit1.OR <- vit1[vit1$Treatment == "OR", ]
vit1.OR.TUR <- rbind(vit1.OR, vit1[vit1$Treatment == "TURB", ])
nrow(vit1.OR.TUR)

levels(vit1.OR.TUR$Artname) = c("Aal", "Bachforelle", "Barbe", "Barsch","Huchen","Nase","Rotauge","Äsche")
levels(vit1.OR.TUR$Artname)


# Prepare data frames for individual species
# Aal
vit1.OR.TUR.Aal <- vit1.OR.TUR[vit1.OR.TUR$Artname == "Aal", ]
nrow(vit1.OR.TUR.Aal)

abiotik1.OR.TUR.Aal <- abiotik1.OR.TUR[abiotik1.OR.TUR$Artname == "Aal", ]
nrow(abiotik1.OR.TUR.Aal)

# Bachforelle
vit1.OR.TUR.Bafo <- vit1.OR.TUR[vit1.OR.TUR$Artname == "Bachforelle", ]
nrow(vit1.OR.TUR.Bafo)

abiotik1.OR.TUR.Bafo <- abiotik1.OR.TUR[abiotik1.OR.TUR$Artname == "Bachforelle", ]
nrow(abiotik1.OR.TUR.Bafo)

# Barbe
vit1.OR.TUR.Barbe <- vit1.OR.TUR[vit1.OR.TUR$Artname == "Barbe", ]
nrow(vit1.OR.TUR.Barbe)

abiotik1.OR.TUR.Barbe <- abiotik1.OR.TUR[abiotik1.OR.TUR$Artname == "Barbe", ]
nrow(abiotik1.OR.TUR.Barbe)

# Huchen
vit1.OR.TUR.Huchen <- vit1.OR.TUR[vit1.OR.TUR$Artname == "Huchen", ]
nrow(vit1.OR.TUR.Huchen)

abiotik1.OR.TUR.Huchen <- abiotik1.OR.TUR[abiotik1.OR.TUR$Artname == "Huchen", ]
nrow(abiotik1.OR.TUR.Huchen)

# Flussbarsch
vit1.OR.TUR.Barsch <- vit1.OR.TUR[vit1.OR.TUR$Artname == "Barsch", ]
nrow(vit1.OR.TUR.Barsch)

abiotik1.OR.TUR.Barsch <- abiotik1.OR.TUR[abiotik1.OR.TUR$Artname == "Barsch", ]
nrow(abiotik1.OR.TUR.Barsch)

# Aesche
vit1.OR.TUR.Äsche <- vit1.OR.TUR[vit1.OR.TUR$Artname == "Äsche", ]
nrow(vit1.OR.TUR.Äsche)

abiotik1.OR.TUR.Äsche <- abiotik1.OR.TUR[abiotik1.OR.TUR$Artname == "Äsche", ]
nrow(abiotik1.OR.TUR.Äsche)

# Nase
vit1.OR.TUR.Nase <- vit1.OR.TUR[vit1.OR.TUR$Artname == "Nase", ]
nrow(vit1.OR.TUR.Nase)

abiotik1.OR.TUR.Nase <- abiotik1.OR.TUR[abiotik1.OR.TUR$Artname == "Nase", ]
nrow(abiotik1.OR.TUR.Nase)

# Rotauge
vit1.OR.TUR.Rotauge <- vit1.OR.TUR[vit1.OR.TUR$Artname == "Rotauge", ]
nrow(vit1.OR.TUR.Rotauge)

abiotik1.OR.TUR.Rotauge <- abiotik1.OR.TUR[abiotik1.OR.TUR$Artname == "Rotauge", ]
nrow(abiotik1.OR.TUR.Rotauge)



# Carry out univariate analyses
################## Aal (Umfangsgeschwindigkeit):
# 1.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Aal$Kum.Amputationen, abiotik1.OR.TUR.Aal$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("Amputationen.Kopf.Körper", "Umfangsgeschwindigkeit")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen.Kopf.Körper)~aalDataZerosReplaced$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# exp model
expModel1 <- lm(aalData$Amputationen.Kopf.Körper~exp(aalData$Umfangsgeschwindigkeit))
summary(expModel1)
plot(expModel1)


# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Umfangsgeschwindigkeit, log(Amputationen.Kopf.Körper))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Umfangsgeschwindigkeit - Aal") + 
  theme(plot.title = element_text(size = 11))
p1

# exp plot
p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = Amputationen.Kopf.Körper)) + geom_point(color = "darkred") +
  stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
fig <- ggplotly(p)
fig



# 1.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Aal$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Aal$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("StauchungenVerformungen", "Umfangsgeschwindigkeit")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$StauchungenVerformungen)~aalDataZerosReplaced2$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# exp model
expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Umfangsgeschwindigkeit))
summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Umfangsgeschwindigkeit, log(StauchungenVerformungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Umfangsgeschwindigkeit - Aal") + 
  theme(plot.title = element_text(size = 11))

# exp plot
p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
  stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
fig <- ggplotly(p)
fig


# 1.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Aal$Kum.Frakturen, abiotik1.OR.TUR.Aal$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("Frakturen", "Umfangsgeschwindigkeit")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Umfangsgeschwindigkeit, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Umfangsgeschwindigkeit - Aal") + 
  theme(plot.title = element_text(size = 11))



################## Nase (Fallhöhe):
# 2.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Nase$Kum.Amputationen, abiotik1.OR.TUR.Nase$Fallh.he..m.)
colnames(aalData) <- c("Amputationen.Kopf.Körper", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen.Kopf.Körper)~aalDataZerosReplaced$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Fallhöhe, log(Amputationen.Kopf.Körper))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Fallhöhe - Nase") + 
  theme(plot.title = element_text(size = 11))


# 2.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Nase$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Nase$Fallh.he..m.)
colnames(aalData) <- c("Stauchungen", "Fallhöhe")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced2)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# exp model
expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Fallhöhe))
summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Fallhöhe - Nase") + 
  theme(plot.title = element_text(size = 11))

# exp plot
p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
  stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
fig <- ggplotly(p)
fig


# 2.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Nase$Kum.Frakturen, abiotik1.OR.TUR.Nase$Fallh.he..m.)
colnames(aalData) <- c("Frakturen", "Fallhöhe")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Fallhöhe - Nase") + 
  theme(plot.title = element_text(size = 11))


# 2.4 Schwimmblase Füllung
aalData <- cbind(vit1.OR.TUR.Nase$Schwimmblase_F.llung  , abiotik1.OR.TUR.Nase$Fallh.he..m.)
colnames(aalData) <- c("Schwimmblase", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Schwimmblase)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Schwimmblase))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Schwimmblase) ~ Fallhöhe - Nase") + 
  theme(plot.title = element_text(size = 11))



# 2.5 Gasblasen innere
aalData <- cbind(vit1.OR.TUR.Nase$Kum.InnereGasblasen   , abiotik1.OR.TUR.Nase$Fallh.he..m.)
colnames(aalData) <- c("Gasblasen", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Gasblasen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Gasblasen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Gasblasen) ~ Fallhöhe - Nase") + 
  theme(plot.title = element_text(size = 11))





#################### Bachforelle (Anzahl Schaufelblätter)
# 3.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Bafo$Kum.Amputationen, abiotik1.OR.TUR.Bafo$Anzahl.Schaufeln)
colnames(aalData) <- c("Amputationen", "Anzahl.Schaufeln")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Anzahl.Schaufeln, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Anzahl.Schaufeln - Bafo") + 
  theme(plot.title = element_text(size = 11))




# 3.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Bafo$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Bafo$Anzahl.Schaufeln)
colnames(aalData) <- c("Stauchungen", "Anzahl.Schaufeln")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Anzahl.Schaufeln, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Anzahl.Schaufeln - Bafo") + 
  theme(plot.title = element_text(size = 11))


# 3.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Bafo$Kum.Frakturen, abiotik1.OR.TUR.Bafo$Anzahl.Schaufeln)
colnames(aalData) <- c("Frakturen", "Anzahl.Schaufeln")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Anzahl.Schaufeln, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Anzahl.Schaufeln - Bafo") + 
  theme(plot.title = element_text(size = 11))




#################### Flussbarsch (Anzahl Schaufelblätter)
# 4.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Barsch$Kum.Amputationen, abiotik1.OR.TUR.Barsch$Anzahl.Schaufeln)
colnames(aalData) <- c("Amputationen", "Anzahl.Schaufeln")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Anzahl.Schaufeln, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Anzahl.Schaufeln - Barsch") + 
  theme(plot.title = element_text(size = 11))


# 4.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Barsch$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Barsch$Anzahl.Schaufeln)
colnames(aalData) <- c("Stauchungen", "Anzahl.Schaufeln")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Anzahl.Schaufeln, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Anzahl.Schaufeln - Barsch") + 
  theme(plot.title = element_text(size = 11))


# 4.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Barsch$Kum.Frakturen, abiotik1.OR.TUR.Barsch$Anzahl.Schaufeln)
colnames(aalData) <- c("Frakturen", "Anzahl.Schaufeln")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Anzahl.Schaufeln, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Anzahl.Schaufeln - Barsch") + 
  theme(plot.title = element_text(size = 11))



#################### Flussbarsch (Fallhöhe)
# 5.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Barsch$Kum.Amputationen, abiotik1.OR.TUR.Barsch$Fallh.he..m.)
colnames(aalData) <- c("Amputationen", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Fallhöhe, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Fallhöhe - Barsch") + 
  theme(plot.title = element_text(size = 11))


# 5.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Barsch$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Barsch$Fallh.he..m.)
colnames(aalData) <- c("Stauchungen", "Fallhöhe")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)


# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Fallhöhe - Barsch") + 
  theme(plot.title = element_text(size = 11))


# 5.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Barsch$Kum.Frakturen, abiotik1.OR.TUR.Barsch$Fallh.he..m.)
colnames(aalData) <- c("Frakturen", "Fallhöhe")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Fallhöhe - Barsch") + 
  theme(plot.title = element_text(size = 11))


# 5.4 Schwimmblase Füllung
aalData <- cbind(vit1.OR.TUR.Barsch$Schwimmblase_F.llung  , abiotik1.OR.TUR.Barsch$Fallh.he..m.)
colnames(aalData) <- c("Füllung.Schwimmblase", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Füllung.Schwimmblase)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Füllung.Schwimmblase))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Schwimmblase) ~ Fallhöhe - Barsch") + 
  theme(plot.title = element_text(size = 11))



# 5.5 Gasblasen innere
aalData <- cbind(vit1.OR.TUR.Barsch$Kum.InnereGasblasen   , abiotik1.OR.TUR.Barsch$Fallh.he..m.)
colnames(aalData) <- c("Gasblasen", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced2)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Gasblasen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Gasblasen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Gasblasen) ~ Fallhöhe - Barsch") + 
  theme(plot.title = element_text(size = 11))




#################### Rotauge (Drehzahl / RPM)
# 6.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Rotauge$Kum.Amputationen, abiotik1.OR.TUR.Rotauge$RPM)
colnames(aalData) <- c("Amputationen", "Drehzahl")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Drehzahl)
summary(logModel1)
plot(logModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Drehzahl, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Drehzahl - Rotauge") + 
  theme(plot.title = element_text(size = 11))


# 6.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Rotauge$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Rotauge$RPM)
colnames(aalData) <- c("Stauchungen", "Drehzahl")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Drehzahl)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Drehzahl, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Drehzahl - Rotauge") + 
  theme(plot.title = element_text(size = 11))


# 6.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Rotauge$Kum.Frakturen, abiotik1.OR.TUR.Rotauge$RPM)
colnames(aalData) <- c("Frakturen", "Drehzahl")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Drehzahl)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Drehzahl, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Drehzahl - Rotauge") + 
  theme(plot.title = element_text(size = 11))


#################### Barbe (Drehzahl / RPM)
# 7.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Barbe$Kum.Amputationen, abiotik1.OR.TUR.Barbe$RPM)
colnames(aalData) <- c("Amputationen", "Drehzahl")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Drehzahl)
summary(logModel1)
plot(logModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Drehzahl, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Drehzahl - Barbe") + 
  theme(plot.title = element_text(size = 11))


# 7.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Barbe$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Barbe$RPM)
colnames(aalData) <- c("Stauchungen", "Drehzahl")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Drehzahl)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Drehzahl, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Drehzahl - Barbe") + 
  theme(plot.title = element_text(size = 11))


# 7.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Barbe$Kum.Frakturen, abiotik1.OR.TUR.Barbe$RPM)
colnames(aalData) <- c("Frakturen", "Drehzahl")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Drehzahl)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Drehzahl, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Drehzahl - Barbe") + 
  theme(plot.title = element_text(size = 11))




#################### Äsche (Fallhöhe)
# 8.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Äsche$Kum.Amputationen, abiotik1.OR.TUR.Äsche$Fallh.he..m.)
colnames(aalData) <- c("Amputationen", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Fallhöhe, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Fallhöhe - Äsche") + 
  theme(plot.title = element_text(size = 11))


# 8.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Äsche$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Äsche$Fallh.he..m.)
colnames(aalData) <- c("Stauchungen", "Fallhöhe")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Fallhöhe - Äsche") + 
  theme(plot.title = element_text(size = 11))


# 8.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Äsche$Kum.Frakturen, abiotik1.OR.TUR.Äsche$Fallh.he..m.)
colnames(aalData) <- c("Frakturen", "Fallhöhe")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Fallhöhe - Äsche") + 
  theme(plot.title = element_text(size = 11))


# 8.4 Schwimmblase Füllung
aalData <- cbind(vit1.OR.TUR.Äsche$Schwimmblase_F.llung  , abiotik1.OR.TUR.Äsche$Fallh.he..m.)
colnames(aalData) <- c("Füllung.Schwimmblase", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Füllung.Schwimmblase)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Füllung.Schwimmblase))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Schwimmblase) ~ Fallhöhe - Äsche") + 
  theme(plot.title = element_text(size = 11))



# 8.5 Gasblasen innere
aalData <- cbind(vit1.OR.TUR.Äsche$Kum.InnereGasblasen   , abiotik1.OR.TUR.Äsche$Fallh.he..m.)
colnames(aalData) <- c("Gasblasen", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Gasblasen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Gasblasen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Gasblasen) ~ Fallhöhe - Äsche") + 
  theme(plot.title = element_text(size = 11))





#################### Huchen (Anzahl Schaufelblätter)
# 9.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Huchen$Kum.Amputationen, abiotik1.OR.TUR.Huchen$Anzahl.Schaufeln)
colnames(aalData) <- c("Amputationen", "Anzahl.Schaufeln")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Anzahl.Schaufeln, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Anzahl.Schaufeln - Huchen") + 
  theme(plot.title = element_text(size = 11))


# 9.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Huchen$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Huchen$Anzahl.Schaufeln)
colnames(aalData) <- c("Stauchungen", "Anzahl.Schaufeln")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Anzahl.Schaufeln, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Anzahl.Schaufeln - Huchen") + 
  theme(plot.title = element_text(size = 11))


# 9.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Huchen$Kum.Frakturen, abiotik1.OR.TUR.Huchen$Anzahl.Schaufeln)
colnames(aalData) <- c("Frakturen", "Anzahl.Schaufeln")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Anzahl.Schaufeln, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Anzahl.Schaufeln - Huchen") + 
  theme(plot.title = element_text(size = 11))



################## Aal (Fallhöhe):
# 10.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Aal$Kum.Amputationen, abiotik1.OR.TUR.Aal$Fallh.he..m.)
colnames(aalData) <- c("Amputationen.Kopf.Körper", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen.Kopf.Körper)~aalDataZerosReplaced$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Fallhöhe, log(Amputationen.Kopf.Körper))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Fallhöhe - Aal") + 
  theme(plot.title = element_text(size = 11))



# 10.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Aal$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Aal$Fallh.he..m.)
colnames(aalData) <- c("Stauchungen", "Fallhöhe")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced2)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Fallhöhe - Aal") + 
  theme(plot.title = element_text(size = 11))


# 10.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Aal$Kum.Frakturen, abiotik1.OR.TUR.Aal$Fallh.he..m.)
colnames(aalData) <- c("Frakturen", "Fallhöhe")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Fallhöhe - Aal") + 
  theme(plot.title = element_text(size = 11))


# 10.4 Schwimmblase Füllung
aalData <- cbind(vit1.OR.TUR.Aal$Schwimmblase_F.llung  , abiotik1.OR.TUR.Aal$Fallh.he..m.)
colnames(aalData) <- c("Schwimmblase", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Schwimmblase)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Schwimmblase))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Schwimmblase) ~ Fallhöhe - Aal") + 
  theme(plot.title = element_text(size = 11))



# 10.5 Gasblasen innere
aalData <- cbind(vit1.OR.TUR.Aal$Kum.InnereGasblasen, abiotik1.OR.TUR.Aal$Fallh.he..m.)
colnames(aalData) <- c("Gasblasen", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Gasblasen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Gasblasen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Gasblasen) ~ Fallhöhe - Aal") + 
  theme(plot.title = element_text(size = 11))



################## Bachforelle (Fallhöhe):
# 11.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Bafo$Kum.Amputationen, abiotik1.OR.TUR.Bafo$Fallh.he..m.)
colnames(aalData) <- c("Amputationen.Kopf.Körper", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen.Kopf.Körper)~aalDataZerosReplaced$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Fallhöhe, log(Amputationen.Kopf.Körper))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Fallhöhe - Bafo") + 
  theme(plot.title = element_text(size = 11))



# 11.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Bafo$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Bafo$Fallh.he..m.)
colnames(aalData) <- c("Stauchungen", "Fallhöhe")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced2)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Fallhöhe - Bafo") + 
  theme(plot.title = element_text(size = 11))


# 11.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Bafo$Kum.Frakturen, abiotik1.OR.TUR.Bafo$Fallh.he..m.)
colnames(aalData) <- c("Frakturen", "Fallhöhe")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Fallhöhe - Bafo") + 
  theme(plot.title = element_text(size = 11))


# 11.4 Schwimmblase Füllung
aalData <- cbind(vit1.OR.TUR.Bafo$Schwimmblase_F.llung  , abiotik1.OR.TUR.Bafo$Fallh.he..m.)
colnames(aalData) <- c("Schwimmblase", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Schwimmblase)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Schwimmblase))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Schwimmblase) ~ Fallhöhe - Bafo") + 
  theme(plot.title = element_text(size = 11))



# 11.5 Gasblasen innere
aalData <- cbind(vit1.OR.TUR.Bafo$Kum.InnereGasblasen   , abiotik1.OR.TUR.Bafo$Fallh.he..m.)
colnames(aalData) <- c("Gasblasen", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Gasblasen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Gasblasen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Gasblasen) ~ Fallhöhe - Bafo") + 
  theme(plot.title = element_text(size = 11))




################## Rotauge (Fallhöhe):
# 12.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Rotauge$Kum.Amputationen, abiotik1.OR.TUR.Rotauge$Fallh.he..m.)
colnames(aalData) <- c("Amputationen.Kopf.Körper", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen.Kopf.Körper)~aalDataZerosReplaced$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Fallhöhe, log(Amputationen.Kopf.Körper))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Fallhöhe - Rotauge") + 
  theme(plot.title = element_text(size = 11))



# 12.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Rotauge$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Rotauge$Fallh.he..m.)
colnames(aalData) <- c("Stauchungen", "Fallhöhe")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced2)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Fallhöhe - Rotauge") + 
  theme(plot.title = element_text(size = 11))


# 12.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Rotauge$Kum.Frakturen, abiotik1.OR.TUR.Rotauge$Fallh.he..m.)
colnames(aalData) <- c("Frakturen", "Fallhöhe")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Fallhöhe - Rotauge") + 
  theme(plot.title = element_text(size = 11))


# 12.4 Schwimmblase Füllung
aalData <- cbind(vit1.OR.TUR.Rotauge$Schwimmblase_F.llung, abiotik1.OR.TUR.Rotauge$Fallh.he..m.)
colnames(aalData) <- c("Schwimmblase", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Schwimmblase)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Schwimmblase))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Schwimmblase) ~ Fallhöhe - Rotauge") + 
  theme(plot.title = element_text(size = 11))



# 12.5 Gasblasen innere
aalData <- cbind(vit1.OR.TUR.Rotauge$Kum.InnereGasblasen, abiotik1.OR.TUR.Rotauge$Fallh.he..m.)
colnames(aalData) <- c("Gasblasen", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Gasblasen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Gasblasen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Gasblasen) ~ Fallhöhe - Rotauge") + 
  theme(plot.title = element_text(size = 11))




################## Barbe (Fallhöhe):
# 13.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Barbe$Kum.Amputationen, abiotik1.OR.TUR.Barbe$Fallh.he..m.)
colnames(aalData) <- c("Amputationen.Kopf.Körper", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen.Kopf.Körper)~aalDataZerosReplaced$Fallhöhe)
summary(logModel1)
plot(logModel1)

# exp model
#expModel1 <- lm(aalData$Amputationen.Kopf.Körper~exp(aalData$Fallhöhe))
#summary(expModel1)
#plot(expModel1)


# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Fallhöhe, log(Amputationen.Kopf.Körper))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Fallhöhe - Barbe") + 
  theme(plot.title = element_text(size = 11))

# exp plot
#p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = Amputationen.Kopf.Körper)) + geom_point(color = "darkred") +
#  stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
#fig <- ggplotly(p)
#fig



# 13.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Barbe$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Barbe$Fallh.he..m.)
colnames(aalData) <- c("Stauchungen", "Fallhöhe")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced2)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# exp model
#expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Fallhöhe))
#summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Fallhöhe - Barbe") + 
  theme(plot.title = element_text(size = 11))

# exp plot
#p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#  stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
#fig <- ggplotly(p)
#fig


# 13.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Barbe$Kum.Frakturen, abiotik1.OR.TUR.Barbe$Fallh.he..m.)
colnames(aalData) <- c("Frakturen", "Fallhöhe")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Fallhöhe - Barbe") + 
  theme(plot.title = element_text(size = 11))


# 13.4 Schwimmblase Füllung
aalData <- cbind(vit1.OR.TUR.Barbe$Schwimmblase_F.llung  , abiotik1.OR.TUR.Barbe$Fallh.he..m.)
colnames(aalData) <- c("Schwimmblase", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Schwimmblase)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Schwimmblase))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Schwimmblase) ~ Fallhöhe - Barbe") + 
  theme(plot.title = element_text(size = 11))



# 13.5 Gasblasen innere
aalData <- cbind(vit1.OR.TUR.Barbe$Kum.InnereGasblasen, abiotik1.OR.TUR.Barbe$Fallh.he..m.)
colnames(aalData) <- c("Gasblasen", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Gasblasen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Gasblasen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Gasblasen) ~ Fallhöhe - Barbe") + 
  theme(plot.title = element_text(size = 11))




################## Huchen (Fallhöhe):
# 14.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Huchen$Kum.Amputationen, abiotik1.OR.TUR.Huchen$Fallh.he..m.)
colnames(aalData) <- c("Amputationen.Kopf.Körper", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen.Kopf.Körper)~aalDataZerosReplaced$Fallhöhe)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$Amputationen.Kopf.Körper~exp(aalData$Fallhöhe))
# summary(expModel1)
# plot(expModel1)


# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Fallhöhe, log(Amputationen.Kopf.Körper))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Fallhöhe - Huchen") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = Amputationen.Kopf.Körper)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig



# 14.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Huchen$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Huchen$Fallh.he..m.)
colnames(aalData) <- c("Stauchungen", "Fallhöhe")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced2)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Fallhöhe))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Fallhöhe - Huchen") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 14.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Huchen$Kum.Frakturen, abiotik1.OR.TUR.Huchen$Fallh.he..m.)
colnames(aalData) <- c("Frakturen", "Fallhöhe")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Fallhöhe - Huchen") + 
  theme(plot.title = element_text(size = 11))


# 14.4 Schwimmblase Füllung
aalData <- cbind(vit1.OR.TUR.Huchen$Schwimmblase_F.llung  , abiotik1.OR.TUR.Huchen$Fallh.he..m.)
colnames(aalData) <- c("Schwimmblase", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Schwimmblase)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Schwimmblase))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Schwimmblase) ~ Fallhöhe - Huchen") + 
  theme(plot.title = element_text(size = 11))



# 14.5 Gasblasen innere
aalData <- cbind(vit1.OR.TUR.Huchen$Kum.InnereGasblasen, abiotik1.OR.TUR.Huchen$Fallh.he..m.)
colnames(aalData) <- c("Gasblasen", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Gasblasen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Gasblasen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Gasblasen) ~ Fallhöhe - Huchen") + 
  theme(plot.title = element_text(size = 11))



################## Nase (Umfangsgeschwindigkeit):
# 15.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Nase$Kum.Amputationen, abiotik1.OR.TUR.Nase$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("Amputationen.Kopf.Körper", "Umfangsgeschwindigkeit")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen.Kopf.Körper)~aalDataZerosReplaced$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$Amputationen.Kopf.Körper~exp(aalData$Umfangsgeschwindigkeit))
# summary(expModel1)
# plot(expModel1)


# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Umfangsgeschwindigkeit, log(Amputationen.Kopf.Körper))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Umfangsgeschwindigkeit - Nase") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = Amputationen.Kopf.Körper)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig



# 15.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Nase$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Nase$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("StauchungenVerformungen", "Umfangsgeschwindigkeit")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$StauchungenVerformungen)~aalDataZerosReplaced2$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Umfangsgeschwindigkeit))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Umfangsgeschwindigkeit, log(StauchungenVerformungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Umfangsgeschwindigkeit - Nase") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 15.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Nase$Kum.Frakturen, abiotik1.OR.TUR.Nase$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("Frakturen", "Umfangsgeschwindigkeit")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Umfangsgeschwindigkeit, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Umfangsgeschwindigkeit - Nase") + 
  theme(plot.title = element_text(size = 11))



################## Bafo (Umfangsgeschwindigkeit):
# 16.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Bafo$Kum.Amputationen, abiotik1.OR.TUR.Bafo$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("Amputationen.Kopf.Körper", "Umfangsgeschwindigkeit")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen.Kopf.Körper)~aalDataZerosReplaced$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$Amputationen.Kopf.Körper~exp(aalData$Umfangsgeschwindigkeit))
# summary(expModel1)
# plot(expModel1)


# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Umfangsgeschwindigkeit, log(Amputationen.Kopf.Körper))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Umfangsgeschwindigkeit - Bafo") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = Amputationen.Kopf.Körper)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig



# 16.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Bafo$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Bafo$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("StauchungenVerformungen", "Umfangsgeschwindigkeit")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$StauchungenVerformungen)~aalDataZerosReplaced2$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Umfangsgeschwindigkeit))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Umfangsgeschwindigkeit, log(StauchungenVerformungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Umfangsgeschwindigkeit - Bafo") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 16.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Bafo$Kum.Frakturen, abiotik1.OR.TUR.Bafo$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("Frakturen", "Umfangsgeschwindigkeit")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Umfangsgeschwindigkeit, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Umfangsgeschwindigkeit - Bafo") + 
  theme(plot.title = element_text(size = 11))



################## Barsch (Umfangsgeschwindigkeit):
# 17.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Barsch$Kum.Amputationen, abiotik1.OR.TUR.Barsch$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("Amputationen.Kopf.Körper", "Umfangsgeschwindigkeit")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen.Kopf.Körper)~aalDataZerosReplaced$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$Amputationen.Kopf.Körper~exp(aalData$Umfangsgeschwindigkeit))
# summary(expModel1)
# plot(expModel1)


# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Umfangsgeschwindigkeit, log(Amputationen.Kopf.Körper))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Umfangsgeschwindigkeit - Barsch") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = Amputationen.Kopf.Körper)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig



# 17.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Barsch$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Barsch$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("StauchungenVerformungen", "Umfangsgeschwindigkeit")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$StauchungenVerformungen)~aalDataZerosReplaced2$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Umfangsgeschwindigkeit))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Umfangsgeschwindigkeit, log(StauchungenVerformungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Umfangsgeschwindigkeit - Barsch") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 17.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Barsch$Kum.Frakturen, abiotik1.OR.TUR.Barsch$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("Frakturen", "Umfangsgeschwindigkeit")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Umfangsgeschwindigkeit, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Umfangsgeschwindigkeit - Barsch") + 
  theme(plot.title = element_text(size = 11))



################## Rotauge (Umfangsgeschwindigkeit):
# 18.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Rotauge$Kum.Amputationen, abiotik1.OR.TUR.Rotauge$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("Amputationen.Kopf.Körper", "Umfangsgeschwindigkeit")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen.Kopf.Körper)~aalDataZerosReplaced$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$Amputationen.Kopf.Körper~exp(aalData$Umfangsgeschwindigkeit))
# summary(expModel1)
# plot(expModel1)


# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Umfangsgeschwindigkeit, log(Amputationen.Kopf.Körper))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Umfangsgeschwindigkeit - Rotauge") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = Amputationen.Kopf.Körper)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig



# 18.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Rotauge$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Rotauge$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("StauchungenVerformungen", "Umfangsgeschwindigkeit")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$StauchungenVerformungen)~aalDataZerosReplaced2$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Umfangsgeschwindigkeit))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Umfangsgeschwindigkeit, log(StauchungenVerformungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Umfangsgeschwindigkeit - Rotauge") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 18.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Rotauge$Kum.Frakturen, abiotik1.OR.TUR.Rotauge$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("Frakturen", "Umfangsgeschwindigkeit")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Umfangsgeschwindigkeit, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Umfangsgeschwindigkeit - Rotauge") + 
  theme(plot.title = element_text(size = 11))



################## Barbe (Umfangsgeschwindigkeit):
# 19.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Barbe$Kum.Amputationen, abiotik1.OR.TUR.Barbe$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("Amputationen.Kopf.Körper", "Umfangsgeschwindigkeit")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen.Kopf.Körper)~aalDataZerosReplaced$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$Amputationen.Kopf.Körper~exp(aalData$Umfangsgeschwindigkeit))
# summary(expModel1)
# plot(expModel1)


# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Umfangsgeschwindigkeit, log(Amputationen.Kopf.Körper))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Umfangsgeschwindigkeit - Barbe") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = Amputationen.Kopf.Körper)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig



# 19.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Barbe$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Barbe$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("StauchungenVerformungen", "Umfangsgeschwindigkeit")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$StauchungenVerformungen)~aalDataZerosReplaced2$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Umfangsgeschwindigkeit))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Umfangsgeschwindigkeit, log(StauchungenVerformungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Umfangsgeschwindigkeit - Barbe") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 19.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Barbe$Kum.Frakturen, abiotik1.OR.TUR.Barbe$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("Frakturen", "Umfangsgeschwindigkeit")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Umfangsgeschwindigkeit, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Umfangsgeschwindigkeit - Barbe") + 
  theme(plot.title = element_text(size = 11))



################## Äsche (Umfangsgeschwindigkeit):
# 20.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Äsche$Kum.Amputationen, abiotik1.OR.TUR.Äsche$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("Amputationen.Kopf.Körper", "Umfangsgeschwindigkeit")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen.Kopf.Körper)~aalDataZerosReplaced$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$Amputationen.Kopf.Körper~exp(aalData$Umfangsgeschwindigkeit))
# summary(expModel1)
# plot(expModel1)


# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Umfangsgeschwindigkeit, log(Amputationen.Kopf.Körper))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Umfangsgeschwindigkeit - Äsche") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = Amputationen.Kopf.Körper)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig



# 20.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Äsche$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Äsche$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("StauchungenVerformungen", "Umfangsgeschwindigkeit")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$StauchungenVerformungen)~aalDataZerosReplaced2$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Umfangsgeschwindigkeit))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Umfangsgeschwindigkeit, log(StauchungenVerformungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Umfangsgeschwindigkeit - Äsche") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 20.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Äsche$Kum.Frakturen, abiotik1.OR.TUR.Äsche$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("Frakturen", "Umfangsgeschwindigkeit")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Umfangsgeschwindigkeit, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Umfangsgeschwindigkeit - Äsche") + 
  theme(plot.title = element_text(size = 11))



################## Huchen (Umfangsgeschwindigkeit):
# 21.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Huchen$Kum.Amputationen, abiotik1.OR.TUR.Huchen$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("Amputationen.Kopf.Körper", "Umfangsgeschwindigkeit")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen.Kopf.Körper)~aalDataZerosReplaced$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$Amputationen.Kopf.Körper~exp(aalData$Umfangsgeschwindigkeit))
# summary(expModel1)
# plot(expModel1)


# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Umfangsgeschwindigkeit, log(Amputationen.Kopf.Körper))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Umfangsgeschwindigkeit - Huchen") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = Amputationen.Kopf.Körper)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig



# 21.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Huchen$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Huchen$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("StauchungenVerformungen", "Umfangsgeschwindigkeit")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$StauchungenVerformungen)~aalDataZerosReplaced2$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Umfangsgeschwindigkeit))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Umfangsgeschwindigkeit, log(StauchungenVerformungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Umfangsgeschwindigkeit - Huchen") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 21.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Huchen$Kum.Frakturen, abiotik1.OR.TUR.Huchen$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("Frakturen", "Umfangsgeschwindigkeit")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Umfangsgeschwindigkeit, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Umfangsgeschwindigkeit - Huchen") + 
  theme(plot.title = element_text(size = 11))







#################### Aal (Drehzahl / RPM)
# 22.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Aal$Kum.Amputationen, abiotik1.OR.TUR.Aal$RPM)
colnames(aalData) <- c("Amputationen", "Drehzahl")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Drehzahl)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$Amputationen~exp(aalData$Anzahl.Schaufeln))
# summary(expModel1)
# plot(expModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Drehzahl, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Drehzahl - Aal") + 
  theme(plot.title = element_text(size = 11))


# 22.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Aal$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Aal$RPM)
colnames(aalData) <- c("Stauchungen", "Drehzahl")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Drehzahl)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Fallhöhe))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Drehzahl, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Drehzahl - Aal") + 
  theme(plot.title = element_text(size = 11))

# exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 22.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Aal$Kum.Frakturen, abiotik1.OR.TUR.Aal$RPM)
colnames(aalData) <- c("Frakturen", "Drehzahl")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced2)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Drehzahl)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Drehzahl, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Drehzahl - Aal") + 
  theme(plot.title = element_text(size = 11))





#################### Nase (Drehzahl / RPM)
# 23.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Nase$Kum.Amputationen, abiotik1.OR.TUR.Nase$RPM)
colnames(aalData) <- c("Amputationen", "Drehzahl")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Drehzahl)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$Amputationen~exp(aalData$Anzahl.Schaufeln))
# summary(expModel1)
# plot(expModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Drehzahl, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Drehzahl - Nase") + 
  theme(plot.title = element_text(size = 11))


# 23.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Nase$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Nase$RPM)
colnames(aalData) <- c("Stauchungen", "Drehzahl")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced2)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Drehzahl)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Fallhöhe))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Drehzahl, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Drehzahl - Nase") + 
  theme(plot.title = element_text(size = 11))

# exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 23.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Nase$Kum.Frakturen, abiotik1.OR.TUR.Nase$RPM)
colnames(aalData) <- c("Frakturen", "Drehzahl")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Drehzahl)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Drehzahl, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Drehzahl - Nase") + 
  theme(plot.title = element_text(size = 11))






#################### Bafo (Drehzahl / RPM)
# 24.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Bafo$Kum.Amputationen, abiotik1.OR.TUR.Bafo$RPM)
colnames(aalData) <- c("Amputationen", "Drehzahl")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Drehzahl)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$Amputationen~exp(aalData$Anzahl.Schaufeln))
# summary(expModel1)
# plot(expModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Drehzahl, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Drehzahl - Bafo") + 
  theme(plot.title = element_text(size = 11))


# 24.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Bafo$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Bafo$RPM)
colnames(aalData) <- c("Stauchungen", "Drehzahl")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Drehzahl)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Fallhöhe))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Drehzahl, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Drehzahl - Bafo") + 
  theme(plot.title = element_text(size = 11))

# exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 24.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Bafo$Kum.Frakturen, abiotik1.OR.TUR.Bafo$RPM)
colnames(aalData) <- c("Frakturen", "Drehzahl")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Drehzahl)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Drehzahl, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Drehzahl - Bafo") + 
  theme(plot.title = element_text(size = 11))





#################### Barsch (Drehzahl / RPM)
# 25.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Barsch$Kum.Amputationen, abiotik1.OR.TUR.Barsch$RPM)
colnames(aalData) <- c("Amputationen", "Drehzahl")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Drehzahl)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$Amputationen~exp(aalData$Anzahl.Schaufeln))
# summary(expModel1)
# plot(expModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Drehzahl, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Drehzahl - Barsch") + 
  theme(plot.title = element_text(size = 11))


# 25.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Barsch$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Barsch$RPM)
colnames(aalData) <- c("Stauchungen", "Drehzahl")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Drehzahl)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Fallhöhe))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Drehzahl, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Drehzahl - Barsch") + 
  theme(plot.title = element_text(size = 11))

# exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 25.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Barsch$Kum.Frakturen, abiotik1.OR.TUR.Barsch$RPM)
colnames(aalData) <- c("Frakturen", "Drehzahl")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Drehzahl)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Drehzahl, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Drehzahl - Barsch") + 
  theme(plot.title = element_text(size = 11))




#################### Äsche (Drehzahl / RPM)
# 26.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Äsche$Kum.Amputationen, abiotik1.OR.TUR.Äsche$RPM)
colnames(aalData) <- c("Amputationen", "Drehzahl")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Drehzahl)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$Amputationen~exp(aalData$Anzahl.Schaufeln))
# summary(expModel1)
# plot(expModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Drehzahl, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Drehzahl - Äsche") + 
  theme(plot.title = element_text(size = 11))


# 26.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Äsche$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Äsche$RPM)
colnames(aalData) <- c("Stauchungen", "Drehzahl")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Drehzahl)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Fallhöhe))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Drehzahl, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Drehzahl - Äsche") + 
  theme(plot.title = element_text(size = 11))

# exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 26.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Äsche$Kum.Frakturen, abiotik1.OR.TUR.Äsche$RPM)
colnames(aalData) <- c("Frakturen", "Drehzahl")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Drehzahl)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Drehzahl, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Drehzahl - Äsche") + 
  theme(plot.title = element_text(size = 11))




#################### Huchen (Drehzahl / RPM)
# 27.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Huchen$Kum.Amputationen, abiotik1.OR.TUR.Huchen$RPM)
colnames(aalData) <- c("Amputationen", "Drehzahl")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Drehzahl)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$Amputationen~exp(aalData$Anzahl.Schaufeln))
# summary(expModel1)
# plot(expModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Drehzahl, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Drehzahl - Huchen") + 
  theme(plot.title = element_text(size = 11))


# 27.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Huchen$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Huchen$RPM)
colnames(aalData) <- c("Stauchungen", "Drehzahl")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Drehzahl)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Fallhöhe))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Drehzahl, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Drehzahl - Huchen") + 
  theme(plot.title = element_text(size = 11))

# exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 27.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Huchen$Kum.Frakturen, abiotik1.OR.TUR.Huchen$RPM)
colnames(aalData) <- c("Frakturen", "Drehzahl")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Drehzahl)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Drehzahl, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Drehzahl - Huchen") + 
  theme(plot.title = element_text(size = 11))


#################### Aal (Anzahl Schaufeln)
# 28.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Aal$Kum.Amputationen, abiotik1.OR.TUR.Aal$Anzahl.Schaufeln)
colnames(aalData) <- c("Amputationen", "Anzahl.Schaufeln")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$Amputationen~exp(aalData$Anzahl.Schaufeln))
# summary(expModel1)
# plot(expModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Anzahl.Schaufeln, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Anzahl.Schaufeln - Aal") + 
  theme(plot.title = element_text(size = 11))



# 28.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Aal$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Aal$Anzahl.Schaufeln)
colnames(aalData) <- c("Stauchungen", "Anzahl.Schaufeln")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Fallhöhe))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Anzahl.Schaufeln, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Anzahl.Schaufeln - Aal") + 
  theme(plot.title = element_text(size = 11))

# exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 28.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Aal$Kum.Frakturen, abiotik1.OR.TUR.Aal$Anzahl.Schaufeln)
colnames(aalData) <- c("Frakturen", "Anzahl.Schaufeln")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Anzahl.Schaufeln, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Anzahl.Schaufeln - Aal") + 
  theme(plot.title = element_text(size = 11))


#################### Nase (Anzahl Schaufeln)
# 29.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Nase$Kum.Amputationen, abiotik1.OR.TUR.Nase$Anzahl.Schaufeln)
colnames(aalData) <- c("Amputationen", "Anzahl.Schaufeln")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$Amputationen~exp(aalData$Anzahl.Schaufeln))
# summary(expModel1)
# plot(expModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Anzahl.Schaufeln, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Anzahl.Schaufeln - Nase") + 
  theme(plot.title = element_text(size = 11))



# 29.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Nase$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Nase$Anzahl.Schaufeln)
colnames(aalData) <- c("Stauchungen", "Anzahl.Schaufeln")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Fallhöhe))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Anzahl.Schaufeln, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Anzahl.Schaufeln - Nase") + 
  theme(plot.title = element_text(size = 11))

# exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 29.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Nase$Kum.Frakturen, abiotik1.OR.TUR.Nase$Anzahl.Schaufeln)
colnames(aalData) <- c("Frakturen", "Anzahl.Schaufeln")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Anzahl.Schaufeln, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Anzahl.Schaufeln - Nase") + 
  theme(plot.title = element_text(size = 11))


#################### Bafo (Anzahl Schaufeln)
# 30.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Rotauge$Kum.Amputationen, abiotik1.OR.TUR.Rotauge$Anzahl.Schaufeln)
colnames(aalData) <- c("Amputationen", "Anzahl.Schaufeln")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$Amputationen~exp(aalData$Anzahl.Schaufeln))
# summary(expModel1)
# plot(expModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Anzahl.Schaufeln, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Anzahl.Schaufeln - Rotauge") + 
  theme(plot.title = element_text(size = 11))



# 30.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Rotauge$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Rotauge$Anzahl.Schaufeln)
colnames(aalData) <- c("Stauchungen", "Anzahl.Schaufeln")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Fallhöhe))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Anzahl.Schaufeln, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Anzahl.Schaufeln - Rotauge") + 
  theme(plot.title = element_text(size = 11))

# exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 30.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Rotauge$Kum.Frakturen, abiotik1.OR.TUR.Rotauge$Anzahl.Schaufeln)
colnames(aalData) <- c("Frakturen", "Anzahl.Schaufeln")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Anzahl.Schaufeln, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Anzahl.Schaufeln - Rotauge") + 
  theme(plot.title = element_text(size = 11))


#################### Barsch (Anzahl Schaufeln)
# 31.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Barbe$Kum.Amputationen, abiotik1.OR.TUR.Barbe$Anzahl.Schaufeln)
colnames(aalData) <- c("Amputationen", "Anzahl.Schaufeln")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$Amputationen~exp(aalData$Anzahl.Schaufeln))
# summary(expModel1)
# plot(expModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Anzahl.Schaufeln, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Anzahl.Schaufeln - Barbe") + 
  theme(plot.title = element_text(size = 11))



# 31.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Barbe$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Barbe$Anzahl.Schaufeln)
colnames(aalData) <- c("Stauchungen", "Anzahl.Schaufeln")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Fallhöhe))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Anzahl.Schaufeln, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Anzahl.Schaufeln - Barbe") + 
  theme(plot.title = element_text(size = 11))

# exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 31.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Barbe$Kum.Frakturen, abiotik1.OR.TUR.Barbe$Anzahl.Schaufeln)
colnames(aalData) <- c("Frakturen", "Anzahl.Schaufeln")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Anzahl.Schaufeln, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Anzahl.Schaufeln - Barbe") + 
  theme(plot.title = element_text(size = 11))


#################### Äsche (Anzahl Schaufeln)
# 32.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR.Äsche$Kum.Amputationen, abiotik1.OR.TUR.Äsche$Anzahl.Schaufeln)
colnames(aalData) <- c("Amputationen", "Anzahl.Schaufeln")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$Amputationen~exp(aalData$Anzahl.Schaufeln))
# summary(expModel1)
# plot(expModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Anzahl.Schaufeln, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Anzahl.Schaufeln - Äsche") + 
  theme(plot.title = element_text(size = 11))



# 32.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR.Äsche$Kum.StauchungenVerformungen, abiotik1.OR.TUR.Äsche$Anzahl.Schaufeln)
colnames(aalData) <- c("Stauchungen", "Anzahl.Schaufeln")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Fallhöhe))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Anzahl.Schaufeln, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Anzahl.Schaufeln - Äsche") + 
  theme(plot.title = element_text(size = 11))

# exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 32.3 Frakturen
aalData <- cbind(vit1.OR.TUR.Äsche$Kum.Frakturen, abiotik1.OR.TUR.Äsche$Anzahl.Schaufeln)
colnames(aalData) <- c("Frakturen", "Anzahl.Schaufeln")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Anzahl.Schaufeln, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Anzahl.Schaufeln - Äsche") + 
  theme(plot.title = element_text(size = 11))







################## Alle Arte (Fallhöhe):
# 33.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR$Kum.Amputationen, abiotik1.OR.TUR$Fallh.he..m.)
colnames(aalData) <- c("Amputationen", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Fallhöhe)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$Amputationen.Kopf.Körper~exp(aalData$Fallhöhe))
# summary(expModel1)
# plot(expModel1)


# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Fallhöhe, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Fallhöhe - Alle Arten") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = Amputationen.Kopf.Körper)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig



# 33.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR$Kum.StauchungenVerformungen, abiotik1.OR.TUR$Fallh.he..m.)
colnames(aalData) <- c("Stauchungen", "Fallhöhe")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced2)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Fallhöhe))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Fallhöhe - Alle Arten") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 33.3 Frakturen
aalData <- cbind(vit1.OR.TUR$Kum.Frakturen, abiotik1.OR.TUR$Fallh.he..m.)
colnames(aalData) <- c("Frakturen", "Fallhöhe")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Fallhöhe - Alle Arten") + 
  theme(plot.title = element_text(size = 11))


# 33.4 Schwimmblase Füllung
aalData <- cbind(vit1.OR.TUR$Schwimmblase_F.llung  , abiotik1.OR.TUR$Fallh.he..m.)
colnames(aalData) <- c("Schwimmblase", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Schwimmblase)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Schwimmblase))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Schwimmblase) ~ Fallhöhe - Alle Arten") + 
  theme(plot.title = element_text(size = 11))


# 33.5 Gasblasen innere
aalData <- cbind(vit1.OR.TUR$Kum.InnereGasblasen   , abiotik1.OR.TUR$Fallh.he..m.)
colnames(aalData) <- c("Gasblasen", "Fallhöhe")
head(aalData)
aalData <- as.data.frame(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Gasblasen)~aalDataZerosReplaced2$Fallhöhe)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Fallhöhe, log(Gasblasen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Gasblasen) ~ Fallhöhe - Alle Arte") + 
  theme(plot.title = element_text(size = 11))




################## Alle Arten (Umfangsgeschwindigkeit):
# 34.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR$Kum.Amputationen, abiotik1.OR.TUR$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("Amputationen", "Umfangsgeschwindigkeit")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$Amputationen.Kopf.Körper~exp(aalData$Umfangsgeschwindigkeit))
# summary(expModel1)
# plot(expModel1)


# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Umfangsgeschwindigkeit, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Umfangsgeschwindigkeit - Alle Arten") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = Amputationen.Kopf.Körper)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig



# 34.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR$Kum.StauchungenVerformungen, abiotik1.OR.TUR$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("StauchungenVerformungen", "Umfangsgeschwindigkeit")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$StauchungenVerformungen)~aalDataZerosReplaced2$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# # exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Umfangsgeschwindigkeit))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Umfangsgeschwindigkeit, log(StauchungenVerformungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Umfangsgeschwindigkeit - Alle Arten") + 
  theme(plot.title = element_text(size = 11))

# # exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 34.3 Frakturen
aalData <- cbind(vit1.OR.TUR$Kum.Frakturen, abiotik1.OR.TUR$Umfangsgeschwindigkeit..m.s.)
colnames(aalData) <- c("Frakturen", "Umfangsgeschwindigkeit")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Umfangsgeschwindigkeit)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Umfangsgeschwindigkeit, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Umfangsgeschwindigkeit - Alle Arten") + 
  theme(plot.title = element_text(size = 11))





#################### Alle Arte (Drehzahl / RPM)
# 35.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR$Kum.Amputationen, abiotik1.OR.TUR$RPM)
colnames(aalData) <- c("Amputationen", "Drehzahl")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Drehzahl)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$Amputationen~exp(aalData$Anzahl.Schaufeln))
# summary(expModel1)
# plot(expModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Drehzahl, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Drehzahl - Alle Arten") + 
  theme(plot.title = element_text(size = 11))


# 35.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR$Kum.StauchungenVerformungen, abiotik1.OR.TUR$RPM)
colnames(aalData) <- c("Stauchungen", "Drehzahl")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Drehzahl)
summary(logModel1)
plot(logModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Drehzahl, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Drehzahl - Alle Arten") + 
  theme(plot.title = element_text(size = 11))


# 35.3 Frakturen
aalData <- cbind(vit1.OR.TUR$Kum.Frakturen, abiotik1.OR.TUR$RPM)
colnames(aalData) <- c("Frakturen", "Drehzahl")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Drehzahl)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Drehzahl, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Drehzahl - Alle Arten") + 
  theme(plot.title = element_text(size = 11))






#################### Alle Arte (Anzahl Schaufelblätter)
# 36.1 Amputationen Kopf und Körper (ohne Flossen, Kiemendeckel und Augen)
aalData <- cbind(vit1.OR.TUR$Kum.Amputationen, abiotik1.OR.TUR$Anzahl.Schaufeln)
colnames(aalData) <- c("Amputationen", "Anzahl.Schaufeln")
head(aalData)
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced$Amputationen)~aalDataZerosReplaced$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# log plot
p1 <- ggplot(aalDataZerosReplaced, aes(Anzahl.Schaufeln, log(Amputationen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Amputationen) ~ Anzahl.Schaufeln - Alle Arten") + 
  theme(plot.title = element_text(size = 11))




# 36.2 Stauchungen Wirbelsäule und Verformungen von Skelettelementen
aalData <- cbind(vit1.OR.TUR$Kum.StauchungenVerformungen, abiotik1.OR.TUR$Anzahl.Schaufeln)
colnames(aalData) <- c("Stauchungen", "Anzahl.Schaufeln")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Stauchungen)~aalDataZerosReplaced2$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# exp model
# expModel1 <- lm(aalData$StauchungenVerformungen~exp(aalData$Fallhöhe))
# summary(expModel1)

# log plot
p2 <- ggplot(aalDataZerosReplaced2, aes(Anzahl.Schaufeln, log(Stauchungen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Stauchungen) ~ Anzahl.Schaufeln - Alle Arten") + 
  theme(plot.title = element_text(size = 11))

# exp plot
# p <- ggplot(aalData, aes(x = Umfangsgeschwindigkeit, y = StauchungenVerformungen)) + geom_point(color = "darkred") +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), se = FALSE, start = list(a=1,b=1))
# fig <- ggplotly(p)
# fig


# 36.3 Frakturen
aalData <- cbind(vit1.OR.TUR$Kum.Frakturen, abiotik1.OR.TUR$Anzahl.Schaufeln)
colnames(aalData) <- c("Frakturen", "Anzahl.Schaufeln")
aalData <- as.data.frame(aalData)
nrow(aalData)

# Replace 0s with NAs for log model
aalDataZerosReplaced2 <- na_if(aalData, 0)
head(aalDataZerosReplaced)

# log model
logModel1 <- lm(log(aalDataZerosReplaced2$Frakturen)~aalDataZerosReplaced2$Anzahl.Schaufeln)
summary(logModel1)
plot(logModel1)

# log plot
p3 <- ggplot(aalDataZerosReplaced2, aes(Anzahl.Schaufeln, log(Frakturen))) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("log(Frakturen) ~ Anzahl.Schaufeln - Alle Arten") + 
  theme(plot.title = element_text(size = 11))







