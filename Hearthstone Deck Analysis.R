# Load the data
library(sqldf)
library(ggplot2)
library(lubridate)
data <- read.csv("~/Desktop/Blizzard/history-of-hearthstone/data.csv", stringsAsFactors=FALSE)
#Data Description
library(readxl)
Description <- read_excel("~/Desktop/Description.xlsx")
Description

#######################################
####### Number of Decks by Year #######
#######################################

#The number of uploaded decks increased over year.
data$year<-year(as.POSIXct(data$date))
ggplot(sqldf('select count(deck_id) as number, year from data where year !=2017 group by year'),aes(year,number)) + geom_bar(stat = "identity")+ ylab("Number of Decks")+ xlab("Year")

#In the standard format, the number of uploaded decks had a rocket increase in 2016.
standard_deck <- data[data$deck_format=='S',]
standard_deck$year<-year(as.POSIXct(standard_deck$date))
sdeck<-sqldf('select count(deck_id) as number, year from standard_deck where year != 2017 group by year')
ggplot(sdeck,aes(year,number)) + geom_bar(stat = "identity")+ ylab("Number of Decks")+ xlab("Year")

#In the wild format, the number of uploaded decks had a drastic increase in 2015 and decreased in 2016.
wild_deck <- data[data$deck_format=='W',]
wild_deck$year<-year(as.POSIXct(wild_deck$date))
wdeck<-sqldf('select count(deck_id) as number, year from wild_deck where year != 2017 group by year')
ggplot(wdeck,aes(year,number)) + geom_bar(stat = "identity")+ ylab("Number of Decks")+ xlab("Year")

#######################################
### Ratings by Deck Class Over Time ###
#######################################

#There is a weak relationship between the dust cost and the popularity of decks.
summary(lm(rating~craft_cost, data=standard_deck))
summary(lm(rating~craft_cost, data=wild_deck))

#Standard format: Rogue had the most ratings in 2013 but went all the way to bottom in the following one year. Ratings of class 'Warrior' are higher than that of other classes between mid 2014 and 2016. After 2016, all deck classes equalized.
standard_deck <- data[data$deck_format=='S',]
standard_deck$date <- as.POSIXct(standard_deck$date)
ggplot(standard_deck, aes(date, rating, col=deck_class)) + geom_smooth(se=F) + ylab("Ratings")+ xlab("Year") + labs(col="Deck Class")

#Wild format: Apart from Paladin and Druid, other deck classes had the highest ratings when released and decreased in the following one year. Ratings converged after 2015.
wild_deck <- data[data$deck_format=='W',]
wild_deck$date <- as.POSIXct(wild_deck $date)
ggplot(wild_deck, aes(date, rating, col=deck_class)) + geom_smooth(se=F) + ylab("Ratings")+ xlab("Year") + labs(col="Deck Class")

#######################################
### Average Dust Cost by Deck Class ###
#######################################

#Except class "Warrior", average dust cost is larger in standard format than in wild format. Mean craft cost for hunter is the smallest.
savg<-sqldf('select avg(craft_cost) as Standard, deck_class from standard_deck group by deck_class')

wavg<-sqldf('select avg(craft_cost) as Wild, deck_class from wild_deck group by deck_class')
avgcost<-merge(savg,wavg)
library(reshape2)
cost_class<-melt(avgcost,id.vars='deck_class',variable.name="Format",value.name="cost")
ggplot(cost_class,aes(deck_class,cost,fill=Format))+geom_bar(stat='identity',position='dodge')+
  ggtitle("Average dust cost by deck class")+ ylab("Dust Cost")+ xlab("Deck Class")

#################################################
## Most Popular Cards for Different Deck Class ##
#################################################

#(1) Standard Format
## Druid
s_druid<-subset(standard_deck, deck_class=="Druid")
s_druid_card<-as.data.frame(table(as.vector(as.matrix(s_druid[12:41]))))
colnames(s_druid_card)<-c("Card_number","Freq")
s_druid_card<-s_druid_card[order(s_druid_card[,2],decreasing=T),]
s_druid_card[1:5,]

## Hunter
s_hunter<-subset(standard_deck, deck_class=="Hunter")
s_hunter_card<-as.data.frame(table(as.vector(as.matrix(s_hunter[12:41]))))
s_hunter_card<-s_hunter_card[order(s_hunter_card[,2],decreasing=T),]
s_hunter_card[1:5,]

## Mage
s_mage<-subset(standard_deck, deck_class=="Mage")
s_mage_card<-as.data.frame(table(as.vector(as.matrix(s_mage[12:41]))))
s_mage_card<-s_mage_card[order(s_mage_card[,2],decreasing=T),]
s_mage_card[1:5,]

## Paladin
s_paladin<-subset(standard_deck, deck_class=="Paladin")
s_paladin_card<-as.data.frame(table(as.vector(as.matrix(s_paladin[12:41]))))
s_paladin_card<-s_paladin_card[order(s_paladin_card[,2],decreasing=T),]
s_paladin_card[1:5,]

## Priest
s_priest<-subset(standard_deck, deck_class=="Priest")
s_priest_card<-as.data.frame(table(as.vector(as.matrix(s_priest[12:41]))))
s_priest_card<-s_priest_card[order(s_priest_card[,2],decreasing=T),]
s_priest_card[1:5,]

## Rogue
s_rogue<-subset(standard_deck, deck_class=="Rogue")
s_rogue_card<-as.data.frame(table(as.vector(as.matrix(s_rogue[12:41]))))
s_rogue_card<-s_rogue_card[order(s_rogue_card[,2],decreasing=T),]
s_rogue_card[1:5,]

## Shaman
s_shaman<-subset(standard_deck, deck_class=="Shaman")
s_shaman_card<-as.data.frame(table(as.vector(as.matrix(s_shaman[12:41]))))
s_shaman_card<-s_shaman_card[order(s_shaman_card[,2],decreasing=T),]
s_shaman_card[1:5,]

## Warlock
s_warlock<-subset(standard_deck, deck_class=="Warlock")
s_warlock_card<-as.data.frame(table(as.vector(as.matrix(s_warlock[12:41]))))
s_warlock_card<-s_warlock_card[order(s_warlock_card[,2],decreasing=T),]
s_warlock_card[1:5,]

## Warrior
s_warrior<-subset(standard_deck, deck_class=="Warrior")
s_warrior_card<-as.data.frame(table(as.vector(as.matrix(s_warrior[12:41]))))
s_warrior_card<-s_warrior_card[order(s_warrior_card[,2],decreasing=T),]
s_warrior_card[1:5,]

## Most popular cards in the standard format:
s_popular_cards<-cbind(s_druid_card[1:5,1],s_hunter_card[1:5,1],s_mage_card[1:5,1],s_paladin_card[1:5,1],s_priest_card[1:5,1],s_rogue_card[1:5,1],s_shaman_card[1:5,1],s_warlock_card[1:5,1],s_warrior_card[1:5,1])
colnames(s_popular_cards)<-c('Druid','Hunter','Mage','Paladin','Priest','Rogue','Shaman','Warlock','Warrior')
s_popular_cards

#(2) Wild Format
## Most popular cards in the wild format:
w_popular_cards<-cbind(w_druid_card[1:5,1],w_hunter_card[1:5,1],w_mage_card[1:5,1],w_paladin_card[1:5,1],w_priest_card[1:5,1],w_rogue_card[1:5,1],w_shaman_card[1:5,1],w_warlock_card[1:5,1],w_warrior_card[1:5,1])
colnames(w_popular_cards)<-c('Druid','Hunter','Mage','Paladin','Priest','Rogue','Shaman','Warlock','Warrior')
w_popular_cards


