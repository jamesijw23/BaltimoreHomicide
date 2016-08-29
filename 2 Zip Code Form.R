library(plyr)
zip_data01 <- read.table("bmorecityzip.txt",header = T)
zip_data02 <- zip_data01[order(zip_data01[,1]),]

mur_data01 <- read.csv("Homicide_Data.csv")
mur_data02 <- mur_data01[order(mur_data01$Zipcode),]
mur_data03 <- table(mur_data02$Zipcode)

## 1. Race
mur_race01 <- as.data.frame.matrix(table(mur_data02$Zipcode,mur_data02$Race_factor))
colnames(mur_race01) <- c("Freq_AA","Freq_WH","Freq_UN")
# Freq_AA - African America
# Freq_WH - White
# Freq_UN - Unknown

## 2. Age a
mur_age01a <- as.data.frame.matrix(table(mur_data02$Zipcode,mur_data02$Age_factor1))
colnames(mur_age01a) <- c("Freqa_L18","Freqa_G18L25","Freqa_G25")
# Freqa_L18    - Age < 18
# Freqa_G18L25 - 18 <= Age < 25
# Freqa_G18    - Age >= 25

## 3. Age b
mur_age01b <- as.data.frame.matrix(table(mur_data02$Zipcode,mur_data02$Age_factor2))
colnames(mur_age01b) <- c("Freqb_L18","Freqb_G18")
# Freqb_L18    - Age < 18
# Freqb_G18L25 - 18 <= Age < 25


## 4. Day Week a
mur_day01a <- as.data.frame.matrix(table(mur_data02$Zipcode,mur_data02$Weekday_factor1))
colnames(mur_day01a) <- c("Freqa_Sun","Freqa_Mon",
                          "Freqa_Tue","Freqa_Wed",
                          "Freqa_Thu","Freqa_Fri",
                          "Freqa_Sat")


## 5. Day Week b
mur_day01b <- as.data.frame.matrix(table(mur_data02$Zipcode,mur_data02$Weekday_factor2))
colnames(mur_day01b) <- c("Freqb_End","Freqb_Day")


## 6. Day Week c
mur_day01c <- as.data.frame.matrix(table(mur_data02$Zipcode,mur_data02$Weekday_factor3))
colnames(mur_day01c) <- c("Freqc_sun","Freqc_Oth")


## 7. Time a
mur_tim01a <- as.data.frame.matrix(table(mur_data02$Zipcode,mur_data02$Time_factor1))
colnames(mur_tim01a) <- c("Freq_AM","Freq_PM")

## 8. Time b
mur_tim01b <- as.data.frame.matrix(table(mur_data02$Zipcode,mur_data02$Time_factor2))
colnames(mur_tim01b) <- c("Freqc_NiG","Freqc_Mor")

zip_final <- cbind(mur_data03,
                   mur_race01,
                   mur_age01a,
                   mur_age01b,
                   mur_day01a,
                   mur_day01b,
                   mur_day01c,
                   mur_tim01a,
                   mur_tim01b)

rownames(zip_final) <- NULL
colnames(zip_final)[1] <- "Zipcode"

setwd("C:/Users/jamesijw23/Desktop/Life/GATO365/Research/Urban Statistics RESEARCH/Disinvestment In Baltimore_Williams_Orozco/Baltimore/Data_BMore/Census_data_cleaned")
census1 <- read.csv("ACS_14_5YR_B02001_with_ann.csv")
imp_zip <- zip_data01$Zipcode %in% census1$Zipcode  
census2 <- census1[imp_zip,]
meet1 <- transform(census2, Zipcode = as.factor(Zipcode))
meet2 <- transform(zip_final, Zipcode = as.factor(Zipcode))
meet <-join_all(list(meet1,meet2), by = "Zipcode")
write.csv(meet,"merge_data_stem2015.csv",row.names =F)
