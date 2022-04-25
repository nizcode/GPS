#GPS apex data
library(zoo)
library(ggplot2)
source("~/GPSproj/HMLcal.R")


path <- "~/GPSproj/data/2022-03-04-R.-Finn-SHRVFC-VS-ST-PATS---1-0-A--4-3-22.csv"
gps<-read.csv(path)

gps$Time<-as.POSIXct(gps$Time,format='%H:%M:%S.%OS')
#HMLD <- min 5.5m/s and min +-3m/s/s
Dist <- ((c(0,gps$Speed..m.s.)+c(gps$Speed..m.s.,0))/2)*0.1
gps$Distance <- Dist[-length(Dist)]
#Need to put a minus in front of the deceleration
u <- c(0,gps$Speed..m.s.[-nrow(gps)])
v <- gps$Speed..m.s.
avgV  <- (u+v)/2
a <- (v-u)/0.1
gps$Instantaneous.Acceleration.Impulse <- a
# Create Metabolic column
# using metabolic function from HMLcal.R script to caluclate EC
EC <- sapply(gps[,c("Instantaneous.Acceleration.Impulse")],metabolic)
#This part is important, I ommited the first ro and added a 0 on the last row.
# for example, if you u = 0.2, v = 0.6, t = 0.1
#therefore a = 4
#in the gps file it's arranged like this:
#u | a
#0.2 | previous(a)
#0.6 | 4
#But technically you are accelerating from 0.2 at 4m/s/s
#thus When you calculate EC (energy cost, based off a) and multiply it by the speed (using vectorised multiplication)
#you will be multiplying the EC by the velocity at the point of arrival
#Which doesn't make sense cause you are accelerating from a initial vel of 0.2 at 4 ms-2 (EC equivalent = 18.62) to get to 0.6m/s
#Therfore you should multiply EC by the initial velocity
EC <- c(EC[-1],0)
metabol <- EC*gps$Speed..m.s.
gps$HML <- c(0,metabol[-length(metabol)])

HMLD.tot <- sum(gps$Distance[gps$HML>=25.5])
# Adding booleans for sprint distance, HSR, explosive distance 
gps$sprintD <- ifelse(gps$Speed..m.s. >= 7, 1, 0)
gps$HSR <- ifelse(gps$Speed..m.s. >= 5.5, 1, 0)
gps$accel <- ifelse(gps$Instantaneous.Acceleration.Impulse >3 & gps$Distance >0.001 , 1, 0)
## mnumber of accels that are sustained for 1 sec.
avgA <- rollapply(gps$Instantaneous.Acceleration.Impulse,10,mean)
sum(avgA < -3.5)

#--------------
#Rolling Average Calculation
#Seperating out only the distance covered at HML
HMLD <- gps[,c("Time","Distance")]
HMLD[gps$metabolic < 25.5, "Distance"] <- 0

#Rolling window, 10 with a width of 1 min
#records position every 0.1 sec
## 1 sec in 60 observation
by <- 100
width <- 600 # 1 min interval

#function to get the time inerval for the max output

time.max <- function(width,by,index){
  t2 <- width+((index-1)*by)
  t1 <- t2 - width
  
  t1 <- as.character(t1/600)
  t2 <- as.character(t2/600)
  return(paste(t1,"-",t2)) # 600 is 1 min, results in minutes
}



avg <- rollapply(HMLD$Distance,width = 600, by = 100, mean)
max(avg)
which.max(avg)
time.max(width,by,which.max(avg))
#create df, Time interval and Intenisty lvl
times <- sapply(1:length(avg),time.max,width = width, by = by) 
rol.df <- data.frame("Times" = times,"Intensity" = avg)


#Intensity vs time
ggplot(rol.df,aes(x=Times,y=Intensity))+
  geom_point()



#Looking at the different intensities for different time width

#1min, 3min, 5min, 10min, 15min

min <- c(1,3,5,10,15)
int.df <- matrix(nrow = 1, ncol = length(min))
colnames(int.df) <- as.character(min)

for (i in 1:length(min)){
  width <- min[i]*600
  by <- 100 # keep this constant
  avg <- rollapply(HMLD$Distance,width = width, by = by, mean)
  int.df[1,i] <- round(max(avg),2)
}

barplot(int.df)
for(i in 1:length(min)){
  text(i,int.df[1,i]-0.05,as.character(int.df[1,i]),col = "red",cex=0.8)
}
