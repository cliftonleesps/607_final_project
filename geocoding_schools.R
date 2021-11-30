# Geocoding Schools from Kratika Patel
library(sf)
library(tidyverse)

url1 <- "https://raw.githubusercontent.com/cliftonleesps/607_final_project/master/Acct_Curricula2.csv"
AcctCurricula <- data.frame(read.csv(url1))
col <- colnames(AcctCurricula) 
col <- toupper(col)
col[1] <- "NAME"
colnames(AcctCurricula) <- col
Names <- AcctCurricula %>% select("NAME")

Names <- data.frame(NAME = unique(Names$NAME))

url2 <- "https://raw.githubusercontent.com/cliftonleesps/607_final_project/master/EDGE_GEOCODE_POSTSECSCH_2021.csv"
schools <- data.frame(read.csv(url2))
col <- colnames(schools)
col[1] <- "UNITID"
colnames(schools) <- col
#head(schools)


SchoolGeo <- schools %>%
  filter(NAME %in% Names$NAME)

#Correct typos and clean names of Universities not detected in schools dataframe
Names %>%
  filter(!(NAME %in% schools$NAME))
Names$NAME[Names$NAME == "Fitchberg State University"] <- "Fitchburg State University"
Names$NAME[Names$NAME == "Saint Joseph's University\n"] <- "Saint Joseph's University"
Names$NAME[Names$NAME == "Pennsylvania State University"] <- "Pennsylvania State University-Penn State Harrisburg"
Names$NAME[Names$NAME == "Strayer University - Delaware"] <- "Strayer University-Delaware"
Names$NAME[Names$NAME == "Strayer University-North Carolina (online, for-profit)"] <- "Strayer University-North Carolina"
Names$NAME[Names$NAME == "University of Massachussetts - Amherst"] <- "University of Massachusetts-Amherst"
Names$NAME[Names$NAME == "University of Massachussetts - Dartmouth"] <- "University of Massachusetts-Dartmouth"
Names$NAME[Names$NAME == "University of North Carolina Chapel Hill"] <- "University of North Carolina at Chapel Hill"

SchoolGeo <- schools %>%
  filter(NAME %in% Names$NAME)

#Remove Duplicate row for Pennsylvania State University-Penn State Harrisburg
SchoolGeo <- SchoolGeo[!(SchoolGeo$UNITID == 49576722),]
#glimpse(SchoolGeo)


# subset(SchoolGeo, NAME == "Ramapo College of New Jersey")
# 
# ?inner_join
# 
# t <- right_join(SchoolGeo, temp_schools, by = c("NAME"= "name"))
# subset(t, NAME == "Ramapo College of New Jersey")
