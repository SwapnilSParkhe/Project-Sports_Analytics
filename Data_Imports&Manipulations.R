##################################################################################################
#--Data Imports
##################################################################################################

#******************************************
#CALIBRATING R & IMPORTING PACKAGES
#******************************************
#*******Getting and loading relevant packages*******
library(pastecs)
library(PerformanceAnalytics)
library(sqldf)
library(dplyr)


#********Setting up work directory******
getwd()
setwd("/Users/swapnilparkhe/Desktop/MSBA-UIC/IDS 575 - Biz Stats/Assignments/Project/Datasets/Data2b/fifa-18-more-complete-player-dataset")
dir()


#*******Importing data*******
#Note: FIFA07 is missing 5 columns (not so important for the anlaysis like logo, image, etc.) and only includes only those rows that correspond to players of 2018 whose age is greater that 15 in 2007
#Note: The accents of in the players names were also stripped using strip-accent function through Excel VBA
fifa_18_complete<-read.csv("FIFA_18_complete.csv")
fifa_07_complete<-read.csv("FIFA_07_common.csv")


#******Overall zoomed out level - inspection of complete data********
#Note: Data seems to be farely consistent
dim(fifa_18_complete)
names(fifa_18_complete)
str(fifa_18_complete)
summary(fifa_18_complete)
stat.desc(fifa_18_complete)

dim(fifa_07_complete)
names(fifa_07_complete)
str(fifa_07_complete)
summary(fifa_07_complete)
stat.desc(fifa_07_complete)


##################################################################################################
#--Data manipulations: Analytical Dataset creation
#ADS-1: Feature engineering and Treatment for FIFA-18 and FIFA-07SOCCER
#ADS-2: Data Segregation, Collation, Feature engineering for young or experienced; good or average/bad players
#ADS-3: Clustering (results not so aligned with real world), hence followed by more apt data segregation into gkp, def, mid, atk groups
##################################################################################################

#----------------------------------------------
#Analytical dataset (ADS-1) creation: 
#----------------------------------------------
#******Making sure both 2018 and 2007 data have same set of columns***
#Removing columns which are not available in FIFA_07 or noise or unimportant or relevant for the analysis (based on domain knowledge); Imputing missing values by 0 (on the basis of observations from summary and footballers names)
fifa_18_ADS<-subset(fifa_18_complete, 
                    select=-c(club_logo,flag,photo,special,birth_date,real_face))

#Removing columns which are not available in FIFA_18 or noise or unimportant or relevant for the analysis (based on domain knowledge); Imputing missing values by 0 (on the basis of observations from summary and footballers names)
fifa_07_ADS<-subset(fifa_07_complete, 
                    select=-c(X))

#*****Imputing missing value with 0 (its logical to impute them as 0 after data inspection it is clear that these value are missing as the data for them must actually have zero value)
fifa_18_ADS[is.na(fifa_18_ADS)]<-0
fifa_07_ADS[is.na(fifa_07_ADS)]<-0


#----------------------------------------------
#Analytical dataset (ADS-2) creation : 
#----------------------------------------------

#******Segregating FIFA 2018 and FIFA 2007 ADS for young and experienced players first (so that their data don't interfere in our analysis first)*******
#Note: Young players are the ones with age <=21 and experienced are the ones with age > 21
fifa_18_ADS_yng<-subset(fifa_18_ADS,age<=21)  #Data that has to be eventually scored
fifa_18_ADS_exp<-subset(fifa_18_ADS,age>21)

fifa_07_ADS_yng<-subset(fifa_07_ADS,age<=21)
fifa_07_ADS_exp<-subset(fifa_07_ADS,age>21)

#******Segregating FIFA 2018 experienced players data into good and average players (based on their current FIFA ratings)**** 
#Note: Average collectively denotes average and bad players
fifa_18_ADS_exp_good<-subset(fifa_18_ADS_exp,overall>=80)
fifa_18_ADS_exp_avrg<-subset(fifa_18_ADS_exp,overall<80)


#******Extracting 2007 FIFA performance data for experienced players of 2018**** 
#Note: The resulting data is going to be a subset of experienced players whose age is greater than 26 in 2018 (as FIFA maintains data for players with age >15, hence we dont have data for players whose age is less than 27 in 2018 ) 
fifa_07_ADS_yng_18_exp_good<-sqldf("SELECT *
                                    FROM fifa_07_ADS_yng
                                    WHERE ID in (select distinct ID from fifa_18_ADS_exp_good)")

fifa_07_ADS_yng_18_exp_avrg<-sqldf("SELECT *
                                    FROM fifa_07_ADS_yng
                                    WHERE ID in (select distinct ID from fifa_18_ADS_exp_avrg)")


#*******Creating flag variable for experienced good and avrg players of 2018, and then Appending their FIFA 2007 data**
#Good_flag creation
fifa_07_ADS_yng_18_exp_good<-mutate(fifa_07_ADS_yng_18_exp_good,good_flag=1)
fifa_07_ADS_yng_18_exp_avrg<-mutate(fifa_07_ADS_yng_18_exp_avrg,good_flag=0)

#Appending FIFA 2007 data for experienced good and avrg players of 2018
fifa_07_ADS_yng_18_exp<-rbind(fifa_07_ADS_yng_18_exp_good,fifa_07_ADS_yng_18_exp_avrg)
fifa_07_ADS_yng_18_exp<-arrange(fifa_07_ADS_yng_18_exp,desc(overall))

#----------------------------------------------
#Analytical dataset (ADS-3) creation: 
#----------------------------------------------

#******Clustering: Based on domain knowledge, clustering data into 4 clusters using player position traits in data - Attackers, Midfielders, Defenders, Goalkeepers*****
#Note: it also turns out the elbow comes up at this point where number of cluster = 4

#Checking row_ID of variables which are required for clustering; Subseting; Imputing NA in position data with 0 
names(fifa_07_ADS_yng_18_exp)   #col_nbr-> 59:85 pertain to player's position data

#Elbow curve to get optimal number of clusters (turns out to be 4 here too)
set.seed(11222)
wss <- (nrow(fifa_07_ADS_yng_18_exp[,59:85])-1)*sum(apply(fifa_07_ADS_yng_18_exp[,59:85],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(fifa_07_ADS_yng_18_exp[,59:85], iter.max = 1000, centers=i)$withinss)
par(mfrow=c(1,1))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

#Clustering players and checking cluster results with reality that I know for certain players
set.seed(123)
km_out<-kmeans(fifa_07_ADS_yng_18_exp[,59:85], centers=4, nstart=100, iter.max = 1000)
km_out
km_out$cluster
table(km_out$cluster)

#Plotting cluster results
library(cluster)
par(mfrow=c(1,1))
clusplot(fifa_07_ADS_yng_18_exp[,59:85], 
         km_out$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0,
         cex=0.5)

#Checking assigned clusters w.r.t preferred player positions (REALITY CHECK)
x<-cbind(fifa_07_ADS_yng_18_exp, clus=km_out$cluster)
Reality_Check<-as.data.frame(filter(x,ID %in% c("20801", "158023", "155862","168651","181872","138956","184344"))  
                             %>% select(name, clus, contains(c("pref"))))

#Note: The clustering results do seem promising statistically, but when I checked the consistency of clusters for certain players for their real preferred playing positions randomly, they didnt seem to be that cogent


#******Segregating ADS data based on preferred position into 4 clusters using player position traits in data - Attackers, Midfielders, Defenders, Goalkeepers***
#First, checking where does the player mostly play (sum of flags for preferred positions for defence, midfield, and attack)
gkp<-fifa_07_ADS_yng_18_exp %>% select(ID,starts_with("prefers")) %>% select(ID,ends_with("gk")) %>% mutate(gkp=rowSums(.[-1]))
def<-fifa_07_ADS_yng_18_exp %>% select(ID,starts_with("prefers")) %>% select(ID,ends_with("b")) %>% mutate(def=rowSums(.[-1]))
mid<-fifa_07_ADS_yng_18_exp %>% select(ID,starts_with("prefers")) %>% select(ID,ends_with("m")) %>% mutate(mid=rowSums(.[-1]))
atk<-fifa_07_ADS_yng_18_exp %>% select(ID,starts_with("prefers")) %>% select(ID,ends_with("s"), ends_with("w"), ends_with("t"), ends_with("f")) %>% mutate(atk=rowSums(.[-1]))
all<-sqldf("select z.ID, gkp, def, mid, atk
           from gkp as z
           inner join def as a
           on z.ID=a.ID
           inner join mid as b
           on a.ID=b.ID
           inner join atk as c
           on a.ID=c.ID") 
all_ply_pos<-all%>%
             mutate(position=ifelse(gkp>mid & gkp>def & gkp>atk, "gkp",
                           ifelse(mid>atk & mid>def, "mid", 
                           ifelse(atk>mid & atk>def, "atk",
                           ifelse(def>mid & def>atk, "def", "mid")))))

#Joining the aggregated position (preferred) data with ADS; Also removing the used columns for segregation in previous step
fifa_07_ADS_yng_18_exp<-sqldf("select a.*, b.position
                               from fifa_07_ADS_yng_18_exp as a
                               inner join all_ply_pos as b
                               on a.ID=b.ID") %>% select (-contains("prefers"))

#Segregating data based on preferred positions to model each set of positions independently; Also, removing columns that summarize their positional play % (as variables are proxy for position variable created in previous step)
fifa_07_ADS_yng_18_exp_gkp<-fifa_07_ADS_yng_18_exp%>%
                            filter(position=="gkp")
fifa_07_ADS_yng_18_exp_def<-fifa_07_ADS_yng_18_exp%>%
                            filter(position=="def")
fifa_07_ADS_yng_18_exp_mid<-fifa_07_ADS_yng_18_exp%>%
                            filter(position=="mid")
fifa_07_ADS_yng_18_exp_atk<-fifa_07_ADS_yng_18_exp%>%
                            filter(position=="atk")


#******Segregating data based on preferred position into 4 clusters using player position traits in data - Attackers, Midfielders, Defenders, Goalkeepers***
New_players<-fifa_18_ADS_yng

#First, checking where does the player mostly play (sum of flags for preferred positions for defence, midfield, and attack)
gkp_new<-New_players %>% select(ID,starts_with("prefers")) %>% select(ID,ends_with("gk")) %>% mutate(gkp=rowSums(.[-1]))
def_new<-New_players %>% select(ID,starts_with("prefers")) %>% select(ID,ends_with("b")) %>% mutate(def=rowSums(.[-1]))
mid_new<-New_players %>% select(ID,starts_with("prefers")) %>% select(ID,ends_with("m")) %>% mutate(mid=rowSums(.[-1]))
atk_new<-New_players %>% select(ID,starts_with("prefers")) %>% select(ID,ends_with("s"), ends_with("w"), ends_with("t"), ends_with("f")) %>% mutate(atk=rowSums(.[-1]))
all_new<-sqldf("select z.ID, gkp, def, mid, atk
               from gkp_new as z
               inner join def_new as a
               on z.ID=a.ID
               inner join mid_new as b
               on a.ID=b.ID
               inner join atk_new as c
               on a.ID=c.ID") 
all_new_ply_pos<-all_new%>%
  mutate(position=ifelse(gkp>mid & gkp>def & gkp>atk, "gkp",
                  ifelse(mid>atk & mid>def, "mid", 
                  ifelse(atk>mid & atk>def, "atk",
                  ifelse(def>mid & def>atk, "def", "mid")))))

#Joining the aggregated position (preferred) data with ADS; Also removing the used columns for segregation in previous step
New_players<-sqldf("select a.*, b.position
                   from New_players as a
                   inner join all_new_ply_pos as b
                   on a.ID=b.ID") %>% select (-contains("prefers"))

#Segregating data based on preferred positions to model each set of positions independently; Also, removing columns that summarize their positional play % (as variables are proxy for position variable created in previous step)
New_players_gkp<-New_players%>%
  filter(position=="gkp")%>%
  arrange(name)
New_players_def<-New_players%>%
  filter(position=="def")%>%
  arrange(name)
New_players_mid<-New_players%>%
  filter(position=="mid")%>%
  arrange(name)
New_players_atk<-New_players%>%
  filter(position=="atk")%>%
  arrange(name)




