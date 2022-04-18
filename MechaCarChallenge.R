library(dplyr)
library(tidyverse)


mpg_data <- read_csv('MechaCar_mpg.csv')
formul <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, mpg_data)
summary(formul)

head(mpg_data)

plt <- ggplot(mpg_data,aes(x=mpg,y=vehicle_length))#import dataset into ggplot2
plt + geom_point()

data_info <- as.matrix(mpg_data[,c("vehicle_length","vehicle_weight","spoiler_angle","ground_clearance", "AWD", "mpg")]) 
cor(data_info)



coil_data <- read_csv('Suspension_Coil.csv')
str(coil_data)
tot_sum <- summarize(coil_data, Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))
summary <- coil_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))

lot3 <- coil_data[(coil_data$Manufacturing_Lot == "Lot3") ,]
lot3_greater <- lot3[(lot3$PSI < 1486),]
lot3_lower <- lot3[(lot3$PSI > 1506),]

count(lot3)
count(lot3_greater) + count(lot3_lower)



t.test(log10(coil_data$PSI),mu=log10(1500))
lot1_t <- subset(coil_data, Manufacturing_Lot=="Lot1")
lot2_t <- subset(coil_data, Manufacturing_Lot=="Lot2")
lot3_t <- subset(coil_data, Manufacturing_Lot=="Lot3")


t.test(log10(lot1_t$PSI),mu=log10(1500))
t.test(log10(lot2_t$PSI),mu=log10(1500))
t.test(log10(lot3_t$PSI),mu=log10(1500))