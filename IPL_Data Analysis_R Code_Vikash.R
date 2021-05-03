# Loading the required packages

library(ggplot2) #  Data visualization
library(lubridate)
library(ggthemes)
library(tidyverse)
library(readr)
library(tidyverse)
library(dplyr)

#Importing the dataset into R Studio, I have imported 5 dataset as per below:- 

Batsman_Data1 <- read_csv("Batsman_Data.csv")

Player_information <- read_csv("WC_players.csv")

Bowler <- read_csv("Bowler_data.csv")

ODI_Match_Statistics <- read_csv("ODI_Match_Totals.csv")

Ground_information <- read_csv("Ground_Averages.csv")

# Data Cleaning 
# Data Cleaned for ODI_Match_Statistics.Also I have extracted Teams and No of Matches per team for further analysis

options(repr.plot.width=11.5, repr.plot.height=5.5)
Total_Matches_Played_by_all_Teams <- ODI_Match_Statistics %>% 
  mutate(Match_ID = str_replace(Match_ID, 'ODI # ', '')) %>% 
  group_by(Country) %>% count(Match_ID) %>% 
  distinct() %>% 
  count(Country)

names(Total_Matches_Played_by_all_Teams)[2] <- 'No_Of_Matches'
view(Total_Matches_Played_by_all_Teams)

## Data Cleaned for Batsman Data after merging with Player_information

names(Player_information)[2] <- 'Player_ID'
Player_performance<- inner_join(Player_information,Batsman_Data1,by = "Player_ID")
view(Batsman_Data1)
View(Player_performance)


# Clean Batsman_data, remove DNB,TDNB,- from joined table player_performance
Batsman_stats1 <- Player_performance %>%
  filter(Bat1 != 'DNB', Bat1 != 'TDNB', SR != '-') %>%
  mutate_at(c('Runs', 'BF', '4s', '6s'), as.integer) %>% mutate(SR = as.double(SR), `Start Date` = lubridate::dmy(`Start Date`)) %>%
  mutate(out_or_not = ifelse(str_detect(string = Bat1, pattern = '\\*'), 0, 1))

view(Batsman_stats1)

#  Renaming column 10, 6s to Sixes
names(Batsman_stats1)[10] <- 'Sixes'
View(Batsman_stats1)

#  Renaming column 9, 4s to fours
names(Batsman_stats1)[9] <- 'Fours'
View(Batsman_stats1)

## Data Cleaned for Bowler's Data after merging with Player_information

Player_performance1<- inner_join(Player_information,Bowler,by = "Player_ID")
View(Player_performance1)

# Data Cleaned Data
# Changed Wkts datatype to integer
# Replacing na with 0
# Displaying all Columns after editing the names

apply(is.na(Bowler),2,sum) #  to check all NA
Bowler$Wkts[is.na(Bowler$Wkts)]=0
names(Bowler)[13]="bowler"
  
view(Bowler)

# Questions to be answered

# 1     Display all the Match Details that have been played only in South africa
SA<- Ground_information %>%
  filter(grepl("South Africa",Ground))
View(SA)

# 2     Display only Stadium and Ground Name from all the matches played in South Africa since 2013
SA1 <- separate(SA,col = Ground, into = c("Stadium","Ground"),sep =',')
SA3 <- separate(SA1,col = Ground, into = c("GroundName","Country"),sep ='-')
Stadium_and_ground_information_in_south_africa<- SA3[ , c("Stadium","GroundName")]
view(Stadium_and_ground_information_in_south_africa)

# 3    Display all matches played by each team since 2013 through Barplot
ggplot(Total_Matches_Played_by_all_Teams, aes(x=Country, y=No_Of_Matches, fill=Country, label=No_Of_Matches)) + 
  # theme_economist() + 
  #scale_fill_economist() +
  geom_bar(stat='identity') +
  labs( title='All Matches Played since 2013')+
  guides(fill=FALSE) + 
  geom_text(nudge_y = 2) +
  theme(plot.margin = margin(10, 20, 20, 10), axis.title.y = element_text(size=11, margin = margin(0,10,0,0)), axis.title.x = element_text(size=11, margin = margin(10,0,0,0)))

# 4 Display top 10 Player with maximum runs scored since 2013 ODI matches 
Total_Runs_Scored_per_Batsman=Batsman_stats1%>%
  select(Player,Runs)%>%
  group_by(Player)%>%
  summarise(Runs=sum(Runs))%>%
  slice_max(Runs,n=10)
view(Total_Runs_Scored_per_Batsman)

# 5 Display Top 10 batsman with Maximum number of runs scored in the year 2019
# firstly , I have extracted year from date column 'start date'
Batsman_Stats_year<-Batsman_stats1 
Batsman_Stats_year$`Start Date` <- format(Batsman_Stats_year$`Start Date`, format = "%Y")
names(Batsman_Stats_year)[13] <- 'Year'

Top_Scorer=Batsman_Stats_year%>%
  filter(Year %in% 2019)%>%
  select(Player,Runs)%>%
  group_by(Player)%>%
  summarise(Runs=sum(Runs))%>%
  slice_max(Runs,n=10)
View(Top_Scorer)

# 6 Display Top 5 batsman with maximum number of sixes hit since 2013
Top_Six_scored_per_Batsman=Batsman_stats1%>%
  select(Player,Sixes)%>%
  group_by(Player)%>%
  summarise(Sixes=sum(Sixes))%>%
  slice_max(Sixes,n=5)
View(Top_Six_scored_per_Batsman)

# 7 Display Top 5 batsman with maximum number of Fours hit since 2013
Top_four_scored_per_Batsman=Batsman_stats1%>%
  select(Player,Fours)%>%
  group_by(Player)%>%
  summarise(Fours=sum(Fours))%>%
  slice_max(Fours,n=5)
view(Top_four_scored_per_Batsman)

View(Top_Scorer)

# 8 Display Average of each Batsman 
a=Batsman_stats1 %>%
  select(Batsman,Runs) %>%
  group_by(Batsman)%>%
  summarise(Runs=sum(Runs))
b=Batsman_stats1 %>%
  select(Batsman,out_or_not) %>%
  group_by(Batsman)%>%
  summarise(out_or_not=sum(out_or_not))
c=merge(a,b,by=c("Batsman","Batsman"),all.x = T)

c=mutate(c,Average=Runs/out_or_not)
Average<- c
view(Average)

# 9 Display Statistics of all matches played against australia
Australia_Ground_Stats<- Batsman_stats1 %>%
  filter(grepl("Australia",Opposition))
View(Australia_Ground_Stats)

# 10 Display top 5 Batsman with maximum runs scored against australia since 2013
Total_Runs_Scored_per_Batsman_in_Australia=Australia_Ground_Stats%>%
  select(Player,Runs)%>%
  group_by(Player)%>%
  summarise(Runs=sum(Runs))%>%
  slice_max(Runs,n=5)
view(Total_Runs_Scored_per_Batsman_in_Australia)



# 11 Using barplot, Display Top 5 batsman with maximum runs scored against australia since 2013
library(ggplot2)
ggplot(Total_Runs_Scored_per_Batsman_in_Australia,aes(x=Player,y=Runs, color = Player))+geom_point(size = 3)

# 12 Using Barplot, Display Top 5 batsman with highest Fours scored 
ggplot(Top_four_scored_per_Batsman, aes(x=Player, y=Fours, fill=Player, label=Fours)) + 
  geom_bar(stat='identity') +
  labs( title='Top 5 batsman with highest Fours')+  
  geom_text(nudge_y = 2) +
  theme(plot.margin = margin(10, 20, 20, 10), axis.title.y = element_text(size=11, margin = margin(0,10,0,0)), axis.title.x = element_text(size=11, margin = margin(10,0,0,0)))

# 13 Using Bar plot, Display Top 5 batsman with highest number of Sixes scored
ggplot(Top_Six_scored_per_Batsman, aes(x=Player, y=Sixes, fill=Player, label=Sixes)) + 
  geom_bar(stat='identity') +
  labs( title='Top 5 batsman with highest Sixes')+  
  geom_text(nudge_y = 2) +
  theme(plot.margin = margin(10, 20, 20, 10), axis.title.y = element_text(size=11, margin = margin(0,10,0,0)), axis.title.x = element_text(size=11, margin = margin(10,0,0,0)))


# 14 Display Top 5 Bowlers who took maximum wickets since 2013
Bowler$Wkts=as.numeric(Bowler$Wkts)
Bowler$Wkts[is.na(Bowler$Wkts)]=0
Total_Wickets_taken_per_bowler=Bowler%>%
  select(Wkts,bowler)%>%
  group_by(bowler)%>%
  summarise_if(is.numeric,sum)
colnames(Total_Wickets_taken_per_bowler)=c('bowler','wickets')
Total_Wickets_taken_per_bowler=arrange(Total_Wickets_taken_per_bowler, desc(wickets))
Total_Wickets_taken_per_bowler=data.frame(Total_Wickets_taken_per_bowler[1:5,])
view(Total_Wickets_taken_per_bowler)



# 15 Using Bar plot, Display Top 5 bowlers who took maximum wickets since 2013
ggplot(Total_Wickets_taken_per_bowler, aes(x=bowler, y=wickets, fill=bowler, label=wickets)) + 
  geom_bar(stat='identity') +
  labs( title='Top 5 bowlers with maximum wickets')+
  guides(fill=FALSE) + 
  geom_text(nudge_y = 2) +
  theme(plot.margin = margin(10, 20, 20, 10), axis.title.y = element_text(size=11, margin = margin(0,10,0,0)), axis.title.x = element_text(size=11, margin = margin(10,0,0,0)))


