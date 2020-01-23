# EU Referendum results csv is from:
# https://data.cambridgeshireinsight.org.uk/dataset/eu-referendum-results
# But is for area, not constituency

# Constituency results were likely estimated from:
# https://medium.com/@chrishanretty/revised-estimates-of-leave-vote-share-in-westminster-constituencies-c4612f06319d
# This model has been shown to be pretty accurate:
# https://commonslibrary.parliament.uk/parliament-and-elections/elections-elections/brexit-votes-by-constituency/

library(readr)
library(tidyverse)

## path to folder ###
path <- 'Submission/'

mpdata <- data.frame(read_csv(paste(path,"data/raw_mpdata.csv",sep='')))
mpdata.second.votes <- data.frame(read_csv(paste(path,"data/raw_mpdata_secondvotes.csv",sep='')))
mpdata$ConstituencyPercent <- as.numeric(mpdata$Constituency.)
mpdata$LeavePercent <- rep(0, nrow(mpdata)) 

## Joining second and first votes
mpdata.second.votes <- select(mpdata.second.votes, Constituency, Vote1, Vote2, Vote3, Vote4)
mpdata.second.votes <- rename(mpdata.second.votes, "SecondVote1" = Vote1, "SecondVote2" = Vote2, "SecondVote3" = Vote3, "SecondVote4" = Vote4)
mpdata <- plyr::join(mpdata, mpdata.second.votes, "Constituency", type="left")
mpdata[mpdata$Name == "Albert Owen", 16:19] <- mpdata.second.votes[mpdata.second.votes$Constituency == "Ynys Môn",2:5]


# apply(mpdata, 1, function(x) x["LeavePercent"] <- ifelse(x["ConstituencyVote"] == "Leave" && !is.na(x["ConstituencyPercent"]), x["ConstituencyPercent"], 100-x["ConstituencyPercent"]))

for (i in 1:(nrow(mpdata))) {
  if (!is.na(mpdata[i,"ConstituencyVote"])) {
    mpdata[i, "LeavePercent"] <- ifelse(mpdata[i,"ConstituencyVote"] == "Leave", mpdata[i,"ConstituencyPercent"], 100-mpdata[i, "ConstituencyPercent"])
  }
}

# Classifying PartyGroup Column into Lab, Con and Other
mpdata$PartyGroup <- apply(mpdata, 1, function(x) ifelse(x["Party"] %in% c("Lab", "Con"), x["Party"], "Other"))


mpdata.no.irish <- filter(mpdata, ConstituencyVote != "None")
mpdata.only.irish <- filter(mpdata, ConstituencyVote == "None")

hanretty <- read_csv(paste(path,"data/hanretty estimates.csv",sep=''))

# Remove newport west
# Replace in mpdata weston-super-mare
hanretty <- filter(hanretty, Constituency != "Newport West")
hanretty$Constituency <- gsub('[[:punct:] ]+',' ',hanretty$Constituency)
mpdata.no.irish <- rbind(mpdata.no.irish, filter(mpdata, Constituency == "Weston-super-Mare"))
mpdata.only.irish <- filter(mpdata.only.irish, Constituency != "Weston-super-Mare")
mpdata.no.irish$Constituency <- gsub('[[:punct:] ]+',' ',mpdata.no.irish$Constituency)

# Names didn't quite match up, so instaed we sorted both by name, and then placed them in that order
mpdata.no.irish.sorted <- arrange(mpdata.no.irish, Constituency)
hanretty.sorted <- arrange(hanretty, Constituency)
mpdata.no.irish.sorted$BetterLeavePercent <- hanretty.sorted$`Figure to use`  

mpdata.only.irish$BetterLeavePercent <- NA
mpdata <- rbind(mpdata.no.irish.sorted, mpdata.only.irish)


#write_csv(mpdata,paste(path,"data/mpdata.csv",sep=''))


#### Producing Final dataset ####
library(readr)

# Import Data ----------
raw_data <- read_csv(paste(path,"data/mpdata.csv",sep=''), 
                     col_types = cols(Party = col_factor(levels = c("Con","Lab","DUP","SNP","Grn","SF","TIG","Ind","LD","PC")),
                                      PartyGroup = col_factor(levels = c("Con", "Lab", "Other")),
                                      ConstituencyVote = col_factor(levels = c("Remain", "Leave")), 
                                      Vote1 = col_factor(levels = c("For", "Against","Abstain")), 
                                      Vote2 = col_factor(levels = c("For", "Against","Abstain")), 
                                      Vote3 = col_factor(levels = c("For", "Against","Abstain")), 
                                      Vote4 = col_factor(levels = c("For", "Against","Abstain")), 
                                      Vote5 = col_factor(levels = c("For", "Against","Abstain")), 
                                      Vote6 = col_factor(levels = c("For", "Against","Abstain")), 
                                      Vote7 = col_factor(levels = c("For", "Against","Abstain")), 
                                      Vote8 = col_factor(levels = c("For", "Against","Abstain")),
                                      SecondVote1 = col_factor(levels = c("For", "Against","Abstain")),
                                      SecondVote2 = col_factor(levels = c("For", "Against","Abstain")),
                                      SecondVote3 = col_factor(levels = c("For", "Against","Abstain")),
                                      SecondVote4 = col_factor(levels = c("For", "Against","Abstain"))
                     )
)

# Prep data --------------

# Remove the Speaker
data <- raw_data[-which(raw_data[,2]=="John Bercow"),]


# remove constituency percent and constituency category number since they are useless
# reorder dataset to be more logical
data <- data[,c(1,20,21,4,6,7,8,9,10,11,12,13,16,17,18,19,2,3)]

# add grouped percent votes column
y <- data$BetterLeavePercent
hist(y)

# NOTICE: no constituency with leave vote above 75% !! (good). 
# will create roughly-balanced classes by splitting at 42%,50%,59%.
# GroupedPercent <- as.factor(ifelse(y <= 0.5, ifelse(y <= 0.42, 1, 2), ifelse(y <= 0.59, 3, 4)))

# This creates balanced classes by splitting at slightly messier quantiles
GroupedPercent <- as.factor(ifelse(y <= quantile(y, 0.50, na.rm=TRUE), ifelse (y <= quantile(y, 0.25, na.rm=TRUE), 1, 2), ifelse(y <= quantile(y, 0.75, na.rm=TRUE), 3, 4)))
quantiles <- round(quantile(y, na.rm=TRUE)*100, 1)

levels(GroupedPercent) <- c("First","Second","Third","Fourth")
# levels(GroupedPercent) <- c(paste(quantiles[1], "%-", quantiles[2], "%", sep=""),
#                             paste(quantiles[2], "%-", quantiles[3], "%", sep=""),
#                             paste(quantiles[3], "%-", quantiles[4], "%", sep=""),
#                             paste(quantiles[4], "%-", quantiles[5], "%", sep=""))
table(GroupedPercent)
data <- cbind(data,GroupedPercent)

over.50 <- y[y>0.500000]
GroupedPercent2 <- as.factor(ifelse(y < 0.50, "Remain",
                                    ifelse(y < quantile(over.50, 0.50, na.rm=TRUE), "Leave", "StrongLeave")))
table(GroupedPercent2)
data <- cbind(data, GroupedPercent2)
# Reorder
data <- data[,c(1,2,3,19,20, 4:18)]

# Fixing names
data$Name[data$Constituency == "West Tyrone"] <- "Orfhlaith Begley"
data$Name[data$Constituency == "Suffolk Coastal"] <- "Therese Coffey"
data$Constituency[data$Name == "Albert Owen"] <- "Ynys Mon"

#write.csv(data, file = paste(path,"data/mpdataFinal.csv",sep=''),row.names = FALSE)
