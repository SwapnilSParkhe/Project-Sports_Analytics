#################################################################
#Analysis of youth data (FIFA-2007) for experienced players of FIFA 2018
#################################################################

#******************************************
#CALIBRATING R & IMPORTING PACKAGES
#******************************************
#*******Getting and loading relevant packages*******
library(pastecs)
library(PerformanceAnalytics)
library(sqldf)
library(dplyr)
library(radarchart)
library(tidyr)
library(qtlcharts)

#***********************
#EDA of GOALKEEPERS
#***********************

#*****Distribution of variables in 2007 for good and avg(and bad) players
#---Overall player ratings
#Numeric- Ratings (overall,potential), Reputation, Value, Wage, Release_clause
par(mfrow=c(1,6))
for(i in names(fifa_07_ADS_yng_18_exp_gkp)[c(6,7,19,4,5,9)]){
  boxplot(data=fifa_07_ADS_yng_18_exp_gkp,
          as.formula(paste(i,"~good_flag")),
          xlab="Good_flag", ylab=i,
          main=i, col=c("red","green"))
}

#---Body related variables 
#Numeric- Age, Height, Weight, Weak-foot
par(mfrow=c(1,3))
for(i in names(fifa_07_ADS_yng_18_exp_gkp)[c(8,10,11)]){
  boxplot(data=fifa_07_ADS_yng_18_exp_gkp,
          as.formula(paste(i,"~good_flag")),
          xlab="Good_flag", ylab=i,
          main=i, col=c("red","green"))
}

#Catg- Body-type and Preferred
par(mfrow=c(2,2))
for(i in names(fifa_07_ADS_yng_18_exp_gkp)[c(12,24)]){
  avg<-table(fifa_07_ADS_yng_18_exp_gkp[fifa_07_ADS_yng_18_exp_gkp[,"good_flag"]==0,][,i])*100/length(fifa_07_ADS_yng_18_exp_gkp[fifa_07_ADS_yng_18_exp_gkp[,"good_flag"]==0,]$ID)
  barplot(avg,main="Good_flag=0",col="red",
          xlab=i,ylab = "%count",ylim=c(0,100))
  
  good<-table(fifa_07_ADS_yng_18_exp_gkp[fifa_07_ADS_yng_18_exp_gkp[,"good_flag"]==1,][,i])*100/length(fifa_07_ADS_yng_18_exp_gkp[fifa_07_ADS_yng_18_exp_gkp[,"good_flag"]==1,]$ID)
  barplot(good,main="Good_flag=1",col="green",
          xlab=i,ylab = "%count",ylim=c(0,100))
}

#---Basic level football skill metrics
#Numeric- Pace, Shoot, Pass, Dribble, Def, Physical
par(mfrow=c(1,6))
for(i in names(fifa_07_ADS_yng_18_exp_gkp)[c(13:18)]){
  boxplot(data=fifa_07_ADS_yng_18_exp_gkp,
          as.formula(paste(i,"~good_flag")),
          xlab="Good_flag", ylab=i,
          main=i, col=c("red","green"))
}


#Catg: Work_rate attack, Work_rate def
par(mfrow=c(2,2))
for(i in names(fifa_07_ADS_yng_18_exp_gkp)[c(22,23)]){
  avg<-table(fifa_07_ADS_yng_18_exp_gkp[fifa_07_ADS_yng_18_exp_gkp[,"good_flag"]==0,][,i])*100/length(fifa_07_ADS_yng_18_exp_gkp[fifa_07_ADS_yng_18_exp_gkp[,"good_flag"]==0,]$ID)
  barplot(avg,main="Good_flag=0",col="red",
          xlab=i,ylab = "%count",ylim=c(0,100))
  
  good<-table(fifa_07_ADS_yng_18_exp_gkp[fifa_07_ADS_yng_18_exp_gkp[,"good_flag"]==1,][,i])*100/length(fifa_07_ADS_yng_18_exp_gkp[fifa_07_ADS_yng_18_exp_gkp[,"good_flag"]==1,]$ID)
  barplot(good,main="Good_flag=1",col="green",
          xlab=i,ylab = "%count",ylim=c(0,100))
}
   
#---Advanced level football skill metrics
#Numeric vars - Skill radar chart for Some bottom (based on overall rating)players (based on overall rating in 2007)
par(mfrow=c(1,1))
radarDF_bot<-arrange(fifa_07_ADS_yng_18_exp_gkp,desc(overall))%>%
  top_n(n=-5,wt=overall)%>%
  select(name,6,25:58)%>% 
  as.data.frame()
radarDF_bot<-gather(radarDF_bot, 
                    key=Label,value=Score,-name) %>%
  spread(key=name,value=Score)
chartJSRadar(scores = radarDF_bot, 
             maxScale = 86, showToolTipLabel = TRUE,
             main="Some bottom (based on overall rating)good players", labelSize = 12)

#Numeric vars - Skill radar chart for Some top (based on overall rating)players (based on overall rating in 2007)
radarDF_top<-arrange(fifa_07_ADS_yng_18_exp_gkp,desc(overall))%>%
             top_n(n=5,wt=overall)%>%
             select(name,6,25:58)%>% 
             as.data.frame()
radarDF_top<-gather(radarDF_top, 
             key=Label,value=Score,-name) %>%
             spread(key=name,value=Score)
chartJSRadar(scores = radarDF_top, 
             maxScale = 86, showToolTipLabel = TRUE,
             main="Some top (based on overall rating)good players", labelSize = 12)

#---Position play metrics (even though the players might have a preferred position but they might play at different positions in the game)
#Numeric vars - Position play heatmap for Some bottom (based on overall rating) players (based on overall rating in 2007)
position_data<-arrange(fifa_07_ADS_yng_18_exp_gkp,desc(overall))%>%
               top_n(n=-10,wt=overall)%>%
               select(name,overall,59:85)
row.names(position_data)<-position_data$name
heatmap(data.matrix(position_data),Rowv=NA, Colv=NA, 
        col = cm.colors(256), scale="column", 
        margins=c(5,10),main="Some bottom (based on overall rating) Players w.r.t their position play",
        xlab = "Position play", ylab="Players (bottom to top -> rating decreases)")

#Numeric vars - Position play heatmap for Some top (based on overall rating) players (based on overall rating in 2007)
position_data<-arrange(fifa_07_ADS_yng_18_exp_gkp,desc(overall))%>%
                       top_n(n=10,wt=overall)%>%
                       select(name,overall,59:85)
row.names(position_data)<-position_data$name
heatmap(data.matrix(position_data),Rowv=NA, Colv=NA, 
        col = cm.colors(256), scale="column", 
        margins=c(5,10),main="Some top (based on overall rating) Players w.r.t their position play",
        xlab = "Position play", ylab="Players (bottom to top -> rating decreases)")

#---All unique metrics (including more complex subtle skills, discipline, and popularity, etc)
#Catg: All binary variables
avg<-NULL
good<-NULL
j<-1
for(i in names(fifa_07_ADS_yng_18_exp_gkp)[c(86:149)]){
  avg[j]<-table(fifa_07_ADS_yng_18_exp_gkp[fifa_07_ADS_yng_18_exp_gkp[,"good_flag"]==0,][,i])*100/length(fifa_07_ADS_yng_18_exp_gkp[fifa_07_ADS_yng_18_exp_gkp[,"good_flag"]==0,]$ID)
  good[j]<-table(fifa_07_ADS_yng_18_exp_gkp[fifa_07_ADS_yng_18_exp_gkp[,"good_flag"]==1,][,i])*100/length(fifa_07_ADS_yng_18_exp_gkp[fifa_07_ADS_yng_18_exp_gkp[,"good_flag"]==1,]$ID)
  j=j+1
}

names(avg)<-names(fifa_07_ADS_yng_18_exp_gkp)[c(86:149)]
names(good)<-names(fifa_07_ADS_yng_18_exp_gkp)[c(86:149)]
other_skills_pct<-data.matrix(rbind(avg,good))
row.names(other_skills_pct)<-c("avg","good")
other_skills_pct<-t(other_skills_pct)
heatmap(other_skills_pct,Rowv=NA, Colv=NA, 
        col = heat.colors(256), scale="column", cexCol = 1,
        margins=c(5,15),main="Other skills for good and avg(or bad) players",
        xlab="Good or avg(or bad)", ylab="Other skills [where colour denotes High(White) to Medium(Yellow) to Low(Red)]")


#***********************
#EDA of DEFENDERS
#***********************

#*****Distribution of variables in 2007 for good and avg(and bad) players
#---Overall player ratings
#Numeric- Ratings (overall,potential), Reputation, Value, Wage, Release_clause
par(mfrow=c(1,6))
for(i in names(fifa_07_ADS_yng_18_exp_def)[c(6,7,19,4,5,9)]){
  boxplot(data=fifa_07_ADS_yng_18_exp_def,
          as.formula(paste(i,"~good_flag")),
          xlab="Good_flag", ylab=i,
          main=i, col=c("red","green"))
}

#---Body related variables 
#Numeric- Age, Height, Weight, Weak-foot
par(mfrow=c(1,3))
for(i in names(fifa_07_ADS_yng_18_exp_def)[c(8,10,11)]){
  boxplot(data=fifa_07_ADS_yng_18_exp_def,
          as.formula(paste(i,"~good_flag")),
          xlab="Good_flag", ylab=i,
          main=i, col=c("red","green"))
}

#Catg- Body-type and Preferred
par(mfrow=c(2,2))
for(i in names(fifa_07_ADS_yng_18_exp_def)[c(12,24)]){
  avg<-table(fifa_07_ADS_yng_18_exp_def[fifa_07_ADS_yng_18_exp_def[,"good_flag"]==0,][,i])*100/length(fifa_07_ADS_yng_18_exp_def[fifa_07_ADS_yng_18_exp_def[,"good_flag"]==0,]$ID)
  barplot(avg,main="Good_flag=0",col="red",
          xlab=i,ylab = "%count",ylim=c(0,100))
  
  good<-table(fifa_07_ADS_yng_18_exp_def[fifa_07_ADS_yng_18_exp_def[,"good_flag"]==1,][,i])*100/length(fifa_07_ADS_yng_18_exp_def[fifa_07_ADS_yng_18_exp_def[,"good_flag"]==1,]$ID)
  barplot(good,main="Good_flag=1",col="green",
          xlab=i,ylab = "%count",ylim=c(0,100))
}

#---Basic level football skill metrics
#Numeric- Pace, Shoot, Pass, Dribble, Def, Physical
par(mfrow=c(1,6))
for(i in names(fifa_07_ADS_yng_18_exp_def)[c(13:18)]){
  boxplot(data=fifa_07_ADS_yng_18_exp_def,
          as.formula(paste(i,"~good_flag")),
          xlab="Good_flag", ylab=i,
          main=i, col=c("red","green"))
}


#Catg: Work_rate attack, Work_rate def
par(mfrow=c(2,2))
for(i in names(fifa_07_ADS_yng_18_exp_def)[c(22,23)]){
  avg<-table(fifa_07_ADS_yng_18_exp_def[fifa_07_ADS_yng_18_exp_def[,"good_flag"]==0,][,i])*100/length(fifa_07_ADS_yng_18_exp_def[fifa_07_ADS_yng_18_exp_def[,"good_flag"]==0,]$ID)
  barplot(avg,main="Good_flag=0",col="red",
          xlab=i,ylab = "%count",ylim=c(0,100))
  
  good<-table(fifa_07_ADS_yng_18_exp_def[fifa_07_ADS_yng_18_exp_def[,"good_flag"]==1,][,i])*100/length(fifa_07_ADS_yng_18_exp_def[fifa_07_ADS_yng_18_exp_def[,"good_flag"]==1,]$ID)
  barplot(good,main="Good_flag=1",col="green",
          xlab=i,ylab = "%count",ylim=c(0,100))
}

#---Advanced level football skill metrics
#Numeric vars - Skill radar chart for Some bottom (based on overall rating)players (based on overall rating in 2007)
par(mfrow=c(1,1))
radarDF_bot<-arrange(fifa_07_ADS_yng_18_exp_def,desc(overall))%>%
  top_n(n=-5,wt=overall)%>%
  select(name,6,25:58)%>% 
  as.data.frame()
radarDF_bot<-gather(radarDF_bot, 
                    key=Label,value=Score,-name) %>%
  spread(key=name,value=Score)
chartJSRadar(scores = radarDF_bot, 
             maxScale = 86, showToolTipLabel = TRUE,
             main="Some bottom (based on overall rating)good players", labelSize = 12)

#Numeric vars - Skill radar chart for Some top (based on overall rating)players (based on overall rating in 2007)
radarDF_top<-arrange(fifa_07_ADS_yng_18_exp_def,desc(overall))%>%
  top_n(n=5,wt=overall)%>%
  select(name,6,25:58)%>% 
  as.data.frame()
radarDF_top<-gather(radarDF_top, 
                    key=Label,value=Score,-name) %>%
  spread(key=name,value=Score)
chartJSRadar(scores = radarDF_top, 
             maxScale = 86, showToolTipLabel = TRUE,
             main="Some top (based on overall rating)good players", labelSize = 12)

#---Position play metrics (even though the players might have a preferred position but they might play at different positions in the game)
#Numeric vars - Position play heatmap for Some bottom (based on overall rating) players (based on overall rating in 2007)
position_data<-arrange(fifa_07_ADS_yng_18_exp_def,desc(overall))%>%
  top_n(n=-10,wt=overall)%>%
  select(name,overall,59:85)
row.names(position_data)<-position_data$name
heatmap(data.matrix(position_data),Rowv=NA, Colv=NA, 
        col = cm.colors(256), scale="column", 
        margins=c(5,10),main="Some bottom (based on overall rating) Players w.r.t their position play",
        xlab = "Position play", ylab="Players (bottom to top -> rating decreases)")

#Numeric vars - Position play heatmap for Some top (based on overall rating) players (based on overall rating in 2007)
position_data<-arrange(fifa_07_ADS_yng_18_exp_def,desc(overall))%>%
  top_n(n=10,wt=overall)%>%
  select(name,overall,59:85)
row.names(position_data)<-position_data$name
heatmap(data.matrix(position_data),Rowv=NA, Colv=NA, 
        col = cm.colors(256), scale="column", 
        margins=c(5,10),main="Some top (based on overall rating) Players w.r.t their position play",
        xlab = "Position play", ylab="Players (bottom to top -> rating decreases)")

#---All unique metrics (including more complex subtle skills, discipline, and popularity, etc)
#Catg: All binary variables
avg<-NULL
good<-NULL
j<-1
for(i in names(fifa_07_ADS_yng_18_exp_def)[c(86:149)]){
  avg[j]<-table(fifa_07_ADS_yng_18_exp_def[fifa_07_ADS_yng_18_exp_def[,"good_flag"]==0,][,i])*100/length(fifa_07_ADS_yng_18_exp_def[fifa_07_ADS_yng_18_exp_def[,"good_flag"]==0,]$ID)
  good[j]<-table(fifa_07_ADS_yng_18_exp_def[fifa_07_ADS_yng_18_exp_def[,"good_flag"]==1,][,i])*100/length(fifa_07_ADS_yng_18_exp_def[fifa_07_ADS_yng_18_exp_def[,"good_flag"]==1,]$ID)
  j=j+1
}

names(avg)<-names(fifa_07_ADS_yng_18_exp_def)[c(86:149)]
names(good)<-names(fifa_07_ADS_yng_18_exp_def)[c(86:149)]
other_skills_pct<-data.matrix(rbind(avg,good))
row.names(other_skills_pct)<-c("avg","good")
other_skills_pct<-t(other_skills_pct)
heatmap(other_skills_pct,Rowv=NA, Colv=NA, 
        col = heat.colors(256), scale="column", cexCol = 1,
        margins=c(5,15),main="Other skills for good and avg(or bad) players",
        xlab="Good or avg(or bad)", ylab="Other skills [where colour denotes High(White) to Medium(Yellow) to Low(Red)]")



#***********************
#EDA of MIDFIELDERS
#***********************

#*****Distribution of variables in 2007 for good and avg(and bad) players
#---Overall player ratings
#Numeric- Ratings (overall,potential), Reputation, Value, Wage, Release_clause
par(mfrow=c(1,6))
for(i in names(fifa_07_ADS_yng_18_exp_mid)[c(6,7,19,4,5,9)]){
  boxplot(data=fifa_07_ADS_yng_18_exp_mid,
          as.formula(paste(i,"~good_flag")),
          xlab="Good_flag", ylab=i,
          main=i, col=c("red","green"))
}

#---Body related variables 
#Numeric- Age, Height, Weight, Weak-foot
par(mfrow=c(1,3))
for(i in names(fifa_07_ADS_yng_18_exp_mid)[c(8,10,11)]){
  boxplot(data=fifa_07_ADS_yng_18_exp_mid,
          as.formula(paste(i,"~good_flag")),
          xlab="Good_flag", ylab=i,
          main=i, col=c("red","green"))
}

#Catg- Body-type and Preferred
par(mfrow=c(2,2))
for(i in names(fifa_07_ADS_yng_18_exp_mid)[c(12,24)]){
  avg<-table(fifa_07_ADS_yng_18_exp_mid[fifa_07_ADS_yng_18_exp_mid[,"good_flag"]==0,][,i])*100/length(fifa_07_ADS_yng_18_exp_mid[fifa_07_ADS_yng_18_exp_mid[,"good_flag"]==0,]$ID)
  barplot(avg,main="Good_flag=0",col="red",
          xlab=i,ylab = "%count",ylim=c(0,100))
  
  good<-table(fifa_07_ADS_yng_18_exp_mid[fifa_07_ADS_yng_18_exp_mid[,"good_flag"]==1,][,i])*100/length(fifa_07_ADS_yng_18_exp_mid[fifa_07_ADS_yng_18_exp_mid[,"good_flag"]==1,]$ID)
  barplot(good,main="Good_flag=1",col="green",
          xlab=i,ylab = "%count",ylim=c(0,100))
}

#---Basic level football skill metrics
#Numeric- Pace, Shoot, Pass, Dribble, Def, Physical
par(mfrow=c(1,6))
for(i in names(fifa_07_ADS_yng_18_exp_mid)[c(13:18)]){
  boxplot(data=fifa_07_ADS_yng_18_exp_mid,
          as.formula(paste(i,"~good_flag")),
          xlab="Good_flag", ylab=i,
          main=i, col=c("red","green"))
}


#Catg: Work_rate attack, Work_rate def
par(mfrow=c(2,2))
for(i in names(fifa_07_ADS_yng_18_exp_mid)[c(22,23)]){
  avg<-table(fifa_07_ADS_yng_18_exp_mid[fifa_07_ADS_yng_18_exp_mid[,"good_flag"]==0,][,i])*100/length(fifa_07_ADS_yng_18_exp_mid[fifa_07_ADS_yng_18_exp_mid[,"good_flag"]==0,]$ID)
  barplot(avg,main="Good_flag=0",col="red",
          xlab=i,ylab = "%count",ylim=c(0,100))
  
  good<-table(fifa_07_ADS_yng_18_exp_mid[fifa_07_ADS_yng_18_exp_mid[,"good_flag"]==1,][,i])*100/length(fifa_07_ADS_yng_18_exp_mid[fifa_07_ADS_yng_18_exp_mid[,"good_flag"]==1,]$ID)
  barplot(good,main="Good_flag=1",col="green",
          xlab=i,ylab = "%count",ylim=c(0,100))
}

#---Advanced level football skill metrics
#Numeric vars - Skill radar chart for Some bottom (based on overall rating)players (based on overall rating in 2007)
par(mfrow=c(1,1))
radarDF_bot<-arrange(fifa_07_ADS_yng_18_exp_mid,desc(overall))%>%
  top_n(n=-5,wt=overall)%>%
  select(name,6,25:58)%>% 
  as.data.frame()
radarDF_bot<-gather(radarDF_bot, 
                    key=Label,value=Score,-name) %>%
  spread(key=name,value=Score)
chartJSRadar(scores = radarDF_bot, 
             maxScale = 86, showToolTipLabel = TRUE,
             main="Some bottom (based on overall rating)good players", labelSize = 12)

#Numeric vars - Skill radar chart for Some top (based on overall rating)players (based on overall rating in 2007)
radarDF_top<-arrange(fifa_07_ADS_yng_18_exp_mid,desc(overall))%>%
  top_n(n=5,wt=overall)%>%
  select(name,6,25:58)%>% 
  as.data.frame()
radarDF_top<-gather(radarDF_top, 
                    key=Label,value=Score,-name) %>%
  spread(key=name,value=Score)
chartJSRadar(scores = radarDF_top, 
             maxScale = 86, showToolTipLabel = TRUE,
             main="Some top (based on overall rating)good players", labelSize = 12)

#---Position play metrics (even though the players might have a preferred position but they might play at different positions in the game)
#Numeric vars - Position play heatmap for Some bottom (based on overall rating) players (based on overall rating in 2007)
position_data<-arrange(fifa_07_ADS_yng_18_exp_mid,desc(overall))%>%
  top_n(n=-10,wt=overall)%>%
  select(name,overall,59:85)
row.names(position_data)<-position_data$name
heatmap(data.matrix(position_data),Rowv=NA, Colv=NA, 
        col = cm.colors(256), scale="column", 
        margins=c(5,10),main="Some bottom (based on overall rating) Players w.r.t their position play",
        xlab = "Position play", ylab="Players (bottom to top -> rating decreases)")

#Numeric vars - Position play heatmap for Some top (based on overall rating) players (based on overall rating in 2007)
position_data<-arrange(fifa_07_ADS_yng_18_exp_mid,desc(overall))%>%
  top_n(n=10,wt=overall)%>%
  select(name,overall,59:85)
row.names(position_data)<-position_data$name
heatmap(data.matrix(position_data),Rowv=NA, Colv=NA, 
        col = cm.colors(256), scale="column", 
        margins=c(5,10),main="Some top (based on overall rating) Players w.r.t their position play",
        xlab = "Position play", ylab="Players (bottom to top -> rating decreases)")

#---All unique metrics (including more complex subtle skills, discipline, and popularity, etc)
#Catg: All binary variables
avg<-NULL
good<-NULL
j<-1
for(i in names(fifa_07_ADS_yng_18_exp_mid)[c(86:149)]){
  avg[j]<-table(fifa_07_ADS_yng_18_exp_mid[fifa_07_ADS_yng_18_exp_mid[,"good_flag"]==0,][,i])*100/length(fifa_07_ADS_yng_18_exp_mid[fifa_07_ADS_yng_18_exp_mid[,"good_flag"]==0,]$ID)
  good[j]<-table(fifa_07_ADS_yng_18_exp_mid[fifa_07_ADS_yng_18_exp_mid[,"good_flag"]==1,][,i])*100/length(fifa_07_ADS_yng_18_exp_mid[fifa_07_ADS_yng_18_exp_mid[,"good_flag"]==1,]$ID)
  j=j+1
}

names(avg)<-names(fifa_07_ADS_yng_18_exp_mid)[c(86:149)]
names(good)<-names(fifa_07_ADS_yng_18_exp_mid)[c(86:149)]
other_skills_pct<-data.matrix(rbind(avg,good))
row.names(other_skills_pct)<-c("avg","good")
other_skills_pct<-t(other_skills_pct)
heatmap(other_skills_pct,Rowv=NA, Colv=NA, 
        col = heat.colors(256), scale="column", cexCol = 1,
        margins=c(5,15),main="Other skills for good and avg(or bad) players",
        xlab="Good or avg(or bad)", ylab="Other skills [High(White) to Medium(Yellow) to Low(Red)]")


#***********************
#EDA of ATTACKERS
#***********************

#*****Distribution of variables in 2007 for good and avg(and bad) players
#---Overall player ratings
#Numeric- Ratings (overall,potential), Reputation, Value, Wage, Release_clause
par(mfrow=c(1,6))
for(i in names(fifa_07_ADS_yng_18_exp_atk)[c(6,7,19,4,5,9)]){
  boxplot(data=fifa_07_ADS_yng_18_exp_atk,
          as.formula(paste(i,"~good_flag")),
          xlab="Good_flag", ylab=i,
          main=i, col=c("red","green"))
}

#---Body related variables 
#Numeric- Age, Height, Weight, Weak-foot
par(mfrow=c(1,3))
for(i in names(fifa_07_ADS_yng_18_exp_atk)[c(8,10,11)]){
  boxplot(data=fifa_07_ADS_yng_18_exp_atk,
          as.formula(paste(i,"~good_flag")),
          xlab="Good_flag", ylab=i,
          main=i, col=c("red","green"))
}

#Catg- Body-type and Preferred
par(mfrow=c(2,2))
for(i in names(fifa_07_ADS_yng_18_exp_atk)[c(12,24)]){
  avg<-table(fifa_07_ADS_yng_18_exp_atk[fifa_07_ADS_yng_18_exp_atk[,"good_flag"]==0,][,i])*100/length(fifa_07_ADS_yng_18_exp_atk[fifa_07_ADS_yng_18_exp_atk[,"good_flag"]==0,]$ID)
  barplot(avg,main="Good_flag=0",col="red",
          xlab=i,ylab = "%count",ylim=c(0,100))
  
  good<-table(fifa_07_ADS_yng_18_exp_atk[fifa_07_ADS_yng_18_exp_atk[,"good_flag"]==1,][,i])*100/length(fifa_07_ADS_yng_18_exp_atk[fifa_07_ADS_yng_18_exp_atk[,"good_flag"]==1,]$ID)
  barplot(good,main="Good_flag=1",col="green",
          xlab=i,ylab = "%count",ylim=c(0,100))
}

#---Basic level football skill metrics
#Numeric- Pace, Shoot, Pass, Dribble, Def, Physical
par(mfrow=c(1,6))
for(i in names(fifa_07_ADS_yng_18_exp_atk)[c(13:18)]){
  boxplot(data=fifa_07_ADS_yng_18_exp_atk,
          as.formula(paste(i,"~good_flag")),
          xlab="Good_flag", ylab=i,
          main=i, col=c("red","green"))
}


#Catg: Work_rate attack, Work_rate def
par(mfrow=c(2,2))
for(i in names(fifa_07_ADS_yng_18_exp_atk)[c(22,23)]){
  avg<-table(fifa_07_ADS_yng_18_exp_atk[fifa_07_ADS_yng_18_exp_atk[,"good_flag"]==0,][,i])*100/length(fifa_07_ADS_yng_18_exp_atk[fifa_07_ADS_yng_18_exp_atk[,"good_flag"]==0,]$ID)
  barplot(avg,main="Good_flag=0",col="red",
          xlab=i,ylab = "%count",ylim=c(0,100))
  
  good<-table(fifa_07_ADS_yng_18_exp_atk[fifa_07_ADS_yng_18_exp_atk[,"good_flag"]==1,][,i])*100/length(fifa_07_ADS_yng_18_exp_atk[fifa_07_ADS_yng_18_exp_atk[,"good_flag"]==1,]$ID)
  barplot(good,main="Good_flag=1",col="green",
          xlab=i,ylab = "%count",ylim=c(0,100))
}

#---Advanced level football skill metrics
#Numeric vars - Skill radar chart for Some bottom (based on overall rating)players (based on overall rating in 2007)
par(mfrow=c(1,1))
radarDF_bot<-arrange(fifa_07_ADS_yng_18_exp_atk,desc(overall))%>%
  top_n(n=-5,wt=overall)%>%
  select(name,6,25:58)%>% 
  as.data.frame()
radarDF_bot<-gather(radarDF_bot, 
                    key=Label,value=Score,-name) %>%
  spread(key=name,value=Score)
chartJSRadar(scores = radarDF_bot, 
             maxScale = 86, showToolTipLabel = TRUE,
             main="Some bottom (based on overall rating)good players", labelSize = 12)

#Numeric vars - Skill radar chart for Some top (based on overall rating)players (based on overall rating in 2007)
radarDF_top<-arrange(fifa_07_ADS_yng_18_exp_atk,desc(overall))%>%
  top_n(n=5,wt=overall)%>%
  select(name,6,25:58)%>% 
  as.data.frame()
radarDF_top<-gather(radarDF_top, 
                    key=Label,value=Score,-name) %>%
  spread(key=name,value=Score)
chartJSRadar(scores = radarDF_top, 
             maxScale = 86, showToolTipLabel = TRUE,
             main="Some top (based on overall rating)good players", labelSize = 12)

#---Position play metrics (even though the players might have a preferred position but they might play at different positions in the game)
#Numeric vars - Position play heatmap for Some bottom (based on overall rating) players (based on overall rating in 2007)
position_data<-arrange(fifa_07_ADS_yng_18_exp_atk,desc(overall))%>%
  top_n(n=-10,wt=overall)%>%
  select(name,overall,59:85)
row.names(position_data)<-position_data$name
heatmap(data.matrix(position_data),Rowv=NA, Colv=NA, 
        col = cm.colors(256), scale="column", 
        margins=c(5,10),main="Some bottom (based on overall rating) Players w.r.t their position play",
        xlab = "Position play", ylab="Players (bottom to top -> rating decreases)")

#Numeric vars - Position play heatmap for Some top (based on overall rating) players (based on overall rating in 2007)
position_data<-arrange(fifa_07_ADS_yng_18_exp_atk,desc(overall))%>%
  top_n(n=10,wt=overall)%>%
  select(name,overall,59:85)
row.names(position_data)<-position_data$name
heatmap(data.matrix(position_data),Rowv=NA, Colv=NA, 
        col = cm.colors(256), scale="column", 
        margins=c(5,10),main="Some top (based on overall rating) Players w.r.t their position play",
        xlab = "Position play", ylab="Players (bottom to top -> rating decreases)")

#---All unique metrics (including more complex subtle skills, discipline, and popularity, etc)
#Catg: All binary variables
avg<-NULL
good<-NULL
j<-1
for(i in names(fifa_07_ADS_yng_18_exp_atk)[c(86:149)]){
  avg[j]<-table(fifa_07_ADS_yng_18_exp_atk[fifa_07_ADS_yng_18_exp_atk[,"good_flag"]==0,][,i])*100/length(fifa_07_ADS_yng_18_exp_atk[fifa_07_ADS_yng_18_exp_atk[,"good_flag"]==0,]$ID)
  good[j]<-table(fifa_07_ADS_yng_18_exp_atk[fifa_07_ADS_yng_18_exp_atk[,"good_flag"]==1,][,i])*100/length(fifa_07_ADS_yng_18_exp_atk[fifa_07_ADS_yng_18_exp_atk[,"good_flag"]==1,]$ID)
  j=j+1
}

names(avg)<-names(fifa_07_ADS_yng_18_exp_atk)[c(86:149)]
names(good)<-names(fifa_07_ADS_yng_18_exp_atk)[c(86:149)]
other_skills_pct<-data.matrix(rbind(avg,good))
row.names(other_skills_pct)<-c("avg","good")
other_skills_pct<-t(other_skills_pct)
heatmap(other_skills_pct,Rowv=NA, Colv=NA, 
        col = heat.colors(256), scale="column", cexCol = 1,
        margins=c(5,15),main="Other skills for good and avg(or bad) players",
        xlab="Good or avg(or bad)", ylab="Other skills [where colour denotes High(White) to Medium(Yellow) to Low(Red)]")

