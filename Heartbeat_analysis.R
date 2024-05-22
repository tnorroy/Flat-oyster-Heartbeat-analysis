library(lubridate)
library(mgcv)
library(lme4)
library(ggplot2)
library(dplyr)


#Import the data (change the path for the file)
data<- read.table("/Users/tomnorroy/Documents/AGRO/Stage/S7/Yerseke/Rscript Local/data_Local/dataset_final.csv", header = TRUE, stringsAsFactors = FALSE, sep = ";", dec = ",")

#Conversion of the time in Day/Month/Year_Hour/Minute
data$DateTime <- dmy_hm(data$DateTime)

#Conversion of the time in numeric values in a new column because the model inputs are numeric only
data$num_time <- as.numeric(data$DateTime) 

#The number and the Origine of the Oysters are now factors
data$Origin <- as.factor(data$Origin)

data$treatment <- ifelse(data$Oyster <= 7, "treated", "non-treated")

### The temperature is a factor with two levels : warm if temp > 22°C and cold if not 
data$cat_temp <- ifelse(data$Temperature <= 22.5, "cold", "warm")
data$cat_temp <- as.factor(data$cat_temp)

#Index of the start of each oyster's data
index<-c(1,985,1969,2953,3937,4921,5905,6889,7873,8857)

data$BPM_scaled <- scale(data$BPM)
data$Amplitude_scaled <- scale(data$Amplitude)

## BPM scale
min_BPM <- min(data$BPM)
max_BPM <- max(data$BPM)

new_min <- 0
new_max <- 1

data$BPM_scaled2 <- ((data$BPM - min_BPM) / (max_BPM - min_BPM)) * (new_max - new_min) + new_min


data <- data %>%
  mutate(periode = if_else(hour(DateTime) >= 8 & hour(DateTime) < 20, "matin", "nuit"))
data$periode <- as.factor(data$periode)

plot( dataOyster1$DateTime, dataOyster1$BPM_scaled2)

# Optional selection of the data
dataHW <- head(data, n = nrow(data) - 2952)
dataOyster1 <- head(data, n = nrow(data) - 8856)
dataOyster5 <- data[3937:4920,]

#Delete data from the start, lot of noise
datafiltered <- dataHW %>%
  filter(DateTime >= ymd("2022-10-16"))

datafiltered <- datafiltered %>%
  slice(seq(1, n(), by = 2))

dataOyster5_filtered  <- datafiltered[3469:4335,]

#plot Oyster 5 BPM
plot(dataOyster5$DateTime, dataOyster5$BPM_scaled2)
plot(dataOyster5$DateTime, dataOyster5$BPM_scaled2, 
     type = "l",
     xlab = "Date",
     ylab = "BPM from the oyster n°5 (NL -HW) ",
     main = "BPM from the oyster n°5 function of time",
     xaxt = "n")
dates_to_plot <- unique(as.Date(dataOyster5$DateTime))

# Axis labels
axis(side = 1, at = as.POSIXct(dates_to_plot), labels = format(dates_to_plot, "%Y-%m-%d"), las = 1)
axis.POSIXct(side = 1, at = dataOyster5$DateTime, format = "%m-%d", las = 2)

#Mean by origin
Oyster_Norway <- filter(dataHW, Origin == "Norway")
Oyster_Croatia <- filter(dataHW, Origin == "Croatia")
Oyster_NL <- filter(dataHW, Origin == "Netherlands")
mean(dataHW$BPM, na.rm = TRUE)
mean(Oyster_Norway$BPM, na.rm = TRUE)
mean(Oyster_Croatia$BPM, na.rm = TRUE)
mean(Oyster_NL$BPM, na.rm = TRUE)

means_BPM <- aggregate(BPM ~Origin, data = datafiltered, FUN = mean, na.rm = TRUE)
means_BPM_Origin <- means_BPM[means_BPM$Origin %in% c("Norway", "Netherlands", "Croatia"), ]
print(means_BPM_Origin)
ggplot(means_BPM, aes(x = cat_temp, y = BPM)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.4) +
  labs(title = "Mean of the BPM of the different origin",
       x = "cat_temp",
       y = "Mean of the BPM") +
  theme_minimal()

#Anova
res <- aov(BPM_scaled2 ~ cat_temp, data = dataHW)
summary(res)
tukey <- TukeyHSD(res)
# Tukey Test
tukey

#Linear Model
linear_model <- lm(BPM_scaled2 ~ DateTime + cat_temp , data = datafiltered)
linear_model_updated <- lm (BPM_scaled2 ~ DateTime + cat_temp + Origin + Condition.index, 
    data = datafiltered)

AIC(linear_model,linear_model_updated)
summary(linear_model_updated)
plot(datafiltered$DateTime, datafiltered$BPM_scaled2)
abline(linear_model_updated, col = "red")
plot(linear_model_updated, residuals = TRUE)

ggplot(data, aes(x = DateTime, y = BPM_scaled2, color = cat_temp)) +
  geom_point() +
  labs(x = "Nom de l'axe X", y = "Nom de l'axe Y", color = "Catégorie de température") +
  theme_minimal()

`?`(family.mgcv)

#GAM
model <- gam(BPM_scaled2 ~ s(num_time, k = 20) + s(Amplitude_scaled, k = 10) + cat_temp + Condition.index + Origin, data = datafiltered)
model_gam <- gam(BPM_scaled2 ~ s(num_time) + cat_temp , data = data)
plot(model)
summary(model)$s.table
summary(model)$p.table

AIC(model_gam, linear_model_updated, linear_model, model)
plot(model, residuals = TRUE)

plot(model, select = 1, xlab = "Time", ylab = "Estimated BPM (scaled from 0 to 1)", main = "GAM estimated curve", xaxt = "n", residuals = TRUE)

# Axis labels
dates_to_plot <- unique(as.Date(data$DateTime))
axis(side = 1, at = as.POSIXct(dates_to_plot), labels = format(dates_to_plot, "%Y-%m-%d"))

par(mfrow = c(1, 2))
plot(model, all.terms = TRUE)
summary(model)
gam.check(model)

resid_gam <- residuals(model)
ks.test(resid_gam, "pnorm")
print(test)


library(ggplot2)
library(gridExtra)

# 1st graph: BPM
plot1 <- ggplot(datafiltered, aes(x = num_time, y = BPM_scaled2)) +
  geom_point() +
  geom_smooth(method = "loess")

# 2nd graph: Amplitude
plot2 <- ggplot(datafiltered, aes(x = Amplitude_scaled, y = BPM_scaled2)) +
  geom_point() +
  geom_smooth(method = "loess")
grid.arrange(plot1, plot2, nrow = 1)

#GAM for the amplitude
modelamp <- gam(Amplitude_scaled ~ s(num_time) + Origin + cat_temp, data = datafiltered)
summary(modelamp)
plot(modelamp, residuals = TRUE)

#GLM for the BPM
model2 <- glm(BPM_scaled2 ~ DateTime + Origin  + cat_temp  , data = datafiltered, family = Gamma)
plot(model2)
summary(model2)

AIC(model,model2, modelHW)
BIC(model,model_gam, linear_model_updated)






