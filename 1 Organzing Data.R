library(stringr)
setwd("C:/Users/jamesijw23/Desktop/Life/GATO365/Research/Urban Statistics RESEARCH/Disinvestment In Baltimore_Williams_Orozco/Baltimore/Real_Baltmoredata03160716")

#set directory
setwd("~/Baltimore")
setwd("C:/Users/Bianca/Documents/Baltimore")

#read in file from assigned directory
homcide <- read.csv("data_as_csv.csv")

#print set directory
getwd()

#list files in working directory
list.files()

#list column names and first 7 entries
head(homcide)


## Creating new Race variable

#convert all objects in homcide with respect to Race as character values
race_id <- as.character(homcide$Race)

#emtpy vector
Race_factor = vector()

#for loop; length = 100
for(i in 1:length(homcide$Race)){
  
if(race_id[i]=="Black"){
  tmp = "A"
} else if (race_id[i]=="Asian"){
  tmp = "B"
} else if (race_id[i]=="Hispanic"){
  tmp = "C"
} else if (race_id[i]=="White"){
  tmp = "D"
} else if (race_id[i]=="Unknown"){
  tmp = "E"
}
  #combine empty vector named Race_factor and all tmp character values and replace Race_factor
  Race_factor = rbind(Race_factor,tmp)
}
#encodes vector as a factor
Race_factor <- as.factor(Race_factor)

#combine vector of the new Race variable with homcide data
new_homcide <- cbind(homcide,Race_factor)


## Creating 2 new age variables a and b

#convert all objects in homcide with respect to Age as character values
age_id <- as.character(homcide$Age)

#Empty vector
Age_factor1 = vector()

for(i in 1:length(homcide$Age)){
if(age_id[i]<18){
  tmp = "A"
}else if (age_id[i]>=18 & age_id[i]<25){
  tmp = "B"
}else if (age_id[i]>=25){
  tmp = "C"
}
  #combine empty vector named Age_factor1 and all tmp character values and replace Age_factor
  Age_factor1 = rbind(Age_factor1,tmp)
}
#encodes vector as a factor
Age_factor1 <- as.factor(Age_factor1)  

#combine vector of the new Age variable with homcide data
new_homcide2 <- cbind(new_homcide,Age_factor1)

## Second Age Variable 
age_id2 <- as.character(homcide$Age)
Age_factor2 = vector()

for(i in 1:length(homcide$Age)){
if(age_id2[i] <18 ){
  tmp = "A"
}else if (age_id2[i] >=18 ){
  tmp = "B"
}
  Age_factor2 = rbind(Age_factor2,tmp)
}
Age_factor2 <- as.factor(Age_factor2)
new_homcide3 <- cbind(new_homcide2,Age_factor2)




#Changing dates to days of the week
date<-as.character(homcide$Date)
day_id <- weekdays(as.Date(date, format = "%m/%d/%y"))

##Creating "Day of the Week" Variables
#Letter for each day of the week
Weekday_factor1 = vector()

for(i in 1:length(homcide$Date)){
if(day_id[i]=="Sunday"){
  tmp = "A"
} else if (day_id[i]=="Monday"){
  tmp = "B"
} else if (day_id[i]=="Tuesday"){
  tmp = "C"
} else if (day_id[i]=="Wednesday"){
  tmp = "D"
} else if (day_id[i]=="Thursday"){
  tmp = "E"
} else if (day_id[i]=="Friday"){
  tmp = "F"
} else if (day_id[i]=="Saturday"){
  tmp = "G"
}
  Weekday_factor1 = rbind(Weekday_factor1, tmp)
}
Weekday_factor1 <- as.factor(Weekday_factor1)
new_homcide4 <- cbind(new_homcide3, Weekday_factor1)



#Weekend = A and Weekday = B
Weekday_factor2 = vector()

for(i in 1:length(homcide$Date)){
  if(day_id[i]=="Sunday" | day_id[i]=="Saturday"){
    tmp = "A"
  } else {
    tmp = "B"
  } 
  Weekday_factor2 = rbind(Weekday_factor2, tmp)
}
Weekday_factor2 <- as.factor(Weekday_factor2)
new_homcide5 <- cbind(new_homcide4, Weekday_factor2)



#Sunday = A and every other day = B
Weekday_factor3 = vector()

for(i in 1:length(homcide$Date)){
  if(day_id[i]=="Sunday"){
    tmp = "A"
  } else  {
    tmp = "B"
  } 
  Weekday_factor3 = rbind(Weekday_factor3, tmp)
}
Weekday_factor3 <- as.factor(Weekday_factor3)
new_homcide6 <- cbind(new_homcide5, Weekday_factor3)

head(new_homcide6)
####Doesn't work yet#####





#AM = A and PM= B
df1= as.character(homcide$time)
class(homcide$time)
df.t = format(strptime(df1, "%I:%M %p"), format="%H%M")
# str_replace_all(df.t, "[[:punct:]]", "") # Other way
time_id1 <- as.numeric(df.t)

Time_factor1 = vector()
for(i in 1:length(homcide$time)){
  if(time_id1[i]>=0 & time_id1[i]<=1159 ){
    tmp = "A"
  }else if (time_id1[i]>=1200 & time_id1[i]<=2359 ){
    tmp = "B"
  }
  Time_factor1 = rbind(Time_factor1,tmp)
}

Time_factor1 <- as.factor(Time_factor1)
rownames(Time_factor1) <- NULL
new_homcide7 <- data.frame(new_homcide6, Time_factor1)


# ***** arb. defined: Night = A and Daytime = B


Time_factor2 = vector()
for(i in 1:length(homcide$time)){
  if(time_id1[i]>=1900 | time_id1[i]<=759 ){
    tmp = "A"
  }else if (time_id1[i]>=800 & time_id1[i]<=1859 ){
    tmp = "B"
  }
  Time_factor2 = rbind(Time_factor2,tmp)
}

Time_factor2 <- as.factor(Time_factor2)
rownames(Time_factor2) <- NULL
new_homcide8 <- cbind(new_homcide7, Time_factor2)
fn = "Homicide_Data.csv"
write.csv(new_homcide8,fn,row.names = F)


tab_gen <- table(new_homcide8$Zipcode)
tab_gen/100
barplot(tab_gen)
