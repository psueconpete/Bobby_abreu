#Analyzing the Prime of Bobby Abreu's career





# Reading the Data into R -------------------------------------------------

#using Jim Alberts cheats
source_gist(8892981)
library(devtools)
parse.retrosheet2.pbp(2018)

#parsing all the files during Abreu's career

parse.retrosheet2.pbp(1998)
parse.retrosheet2.pbp(1999)
parse.retrosheet2.pbp(2000)
parse.retrosheet2.pbp(2001)
parse.retrosheet2.pbp(2002)
parse.retrosheet2.pbp(2003)
parse.retrosheet2.pbp(2004)
parse.retrosheet2.pbp(2005)
parse.retrosheet2.pbp(2006)
parse.retrosheet2.pbp(2007)
parse.retrosheet2.pbp(2008)
parse.retrosheet2.pbp(2009)
parse.retrosheet2.pbp(2010)
parse.retrosheet2.pbp(2011)

setwd("~/retrosheet/download.folder/unzipped")
#read them in
all1998 <- read_csv("all1998.csv", col_names = FALSE)
all1999 <- read_csv("all1999.csv", col_names = FALSE)
all2000 <- read_csv("all2000.csv", col_names = FALSE)
all2001 <- read_csv("all2001.csv", col_names = FALSE)
all2002 <- read_csv("all2002.csv", col_names = FALSE)
all2003 <- read_csv("all2003.csv", col_names = FALSE)
all2004 <- read_csv("all2004.csv", col_names = FALSE)
all2005 <- read_csv("all2005.csv", col_names = FALSE)
all2006 <- read_csv("all2006.csv", col_names = FALSE)
all2007 <- read_csv("all2007.csv", col_names = FALSE)
all2008 <- read_csv("all2008.csv", col_names = FALSE)
all2009 <- read_csv("all2009.csv", col_names = FALSE)
all2010 <- read_csv("all2010.csv", col_names = FALSE)
all2011 <- read_csv("all2011.csv", col_names = FALSE)





# Filtering for Bobby Abreu's Plate Appearances ---------------------------

#looking up Abreu's retrosheet name
# its is  "abreubo01"

#writing a function to filter by abreu
abreu <- function(x) {
  filter(x, X11 == "abreb001")
}
#testing the function
test <- abreu(all1998)
View(test)
#it works!

#Now lets do this for all years
abreu1998 <- abreu(all1998)
abreu1999 <- abreu(all1999)
abreu2000 <- abreu(all2000)
abreu2001 <- abreu(all2001)
abreu2002 <- abreu(all2002)
abreu2003 <- abreu(all2003)
abreu2004 <- abreu(all2004)
abreu2005 <- abreu(all2005)
abreu2006 <- abreu(all2006)
abreu2007 <- abreu(all2007)
abreu2008 <- abreu(all2008)
abreu2009 <- abreu(all2009)
abreu2010 <- abreu(all2010)
abreu2011 <- abreu(all2011)


#now lets combine them all into a career log
career <- rbind(abreu1998, abreu1999, abreu2000, abreu2001, abreu2002, abreu2003, abreu2004, abreu2005, abreu2006, abreu2007, abreu2008, abreu2009, abreu2010, abreu2011)





# The column names for Retrosheet -----------------------------------------

col <- c("GAME_ID","AWAY_TEAM_ID","INN_CT","BAT_HOME_ID","OUTS_CT","BALLS_CT","STRIKES_CT","PITCH_SEQ_TX","AWAY_SCORE_CT","HOME_SCORE_CT","BAT_ID","BAT_HAND_CD",'RESP_BAT_ID',"RESP_BAT_HAND_CD","PIT_ID","PIT_HAND_CD","RESP_PIT_ID","RESP_PIT_HAND_CD","POS2_FLD_ID","POS3_FLD_ID","POS4_FLD_ID","POS5_FLD_ID","POS6_FLD_ID","POS7_FLD_ID","POS8_FLD_ID","POS9_FLD_ID","BASE1_RUN_ID","BASE2_RUN_ID","BASE3_RUN_ID","EVENT_TX","LEADOFF_FL","PH_FL","BAT_FLD_CD","BAT_LINEUP_ID","EVENT_CD","BAT_EVENT_FL","AB_FL","H_FL","SH_FL","SF_FL","EVENT_OUTS_CT","DP_FL","TP_FL","RBI_CT","WP_FL","PB_FL","FLD_CD","BATTEDBALL_CD","BUNT_FL","FOUL_FL","BATTEDBALL_LOC_TX","ERR_CT","ERR1_FLD_CD","ERR1_CD","ERR2_FLD_CD","ERR2_CD","ERR3_FLD_CD","ERR3_CD","BAT_DEST_ID","RUN1_DEST_ID","RUN2_DEST_ID","RUN3_DEST_ID","BAT_PLAY_TX","RUN1_PLAY_TX","RUN2_PLAY_TX","RUN3_PLAY_TX","RUN1_SB_FL","RUN2_SB_FL","RUN3_SB_FL","RUN1_CS_FL","RUN2_CS_FL","RUN3_CS_FL","RUN1_PK_FL","RUN2_PK_FL","RUN3_PK_FL","RUN1_RESP_PIT_ID","RUN2_RESP_PIT_ID","RUN3_RESP_PIT_ID","GAME_NEW_FL","GAME_END_FL","PR_RUN1_FL","PR_RUN2_FL","PR_RUN3_FL","REMOVED_FOR_PR_RUN1_ID","REMOVED_FOR_PR_RUN2_ID","REMOVED_FOR_PR_RUN3_ID","REMOVED_FOR_PH_BAT_ID","REMOVED_FOR_PH_BAT_FLD_CD","PO1_FLD_CD","PO2_FLD_CD","PO3_FLD_CD","ASS1_FLD_CD","ASS2_FLD_CD","ASS3_FLD_CD","ASS4_FLD_CD","ASS5_FLD_CD","EVENT_ID" )





# Taking Bobby Abreu's career plate appearances and filtering them --------

#adding names to the columns
names(career) <- col

#now lets take the original dataframes and filter them
#for clutchness

#these functions create a variable that creates the score differential
# of the team abreu was on.
phifunc <- function(x) {
  mutate(x, score=if_else(X2 == "PHI", (X9 - X10), (X10- X9)))
}
nyyfunc <- function(x) {
  mutate(x, score=if_else(X2 == "NYY", (X9 - X10), (X10- X9)))
}
laafunc <- function(x){
  mutate(x, score=if_else(X2 == "LAA", (X9 - X10), (X10- X9)))
}

#I apply that function to all of the data sets
#double for 2005 because Abreu played on both teams
abreu1998 <- phifunc(abreu1998)
abreu1999 <- phifunc(abreu1999)
abreu2000 <- phifunc(abreu2000)
abreu2001 <- phifunc(abreu2001)
abreu2002 <- phifunc(abreu2002)
abreu2003 <- phifunc(abreu2003)
abreu2004 <- phifunc(abreu2004)
abreu2005 <- phifunc(abreu2005)
abreu2005ny <- nyyfunc(abreu2005)
abreu2006 <- nyyfunc(abreu2006)
abreu2007 <- nyyfunc(abreu2007)
abreu2008 <- nyyfunc(abreu2008)
abreu2009 <- laafunc(abreu2009)
abreu2010 <- laafunc(abreu2010)
abreu2011 <- laafunc(abreu2011)

#combine all the datasets for further filtering
career <- rbind(abreu1998, abreu1999, abreu2000, abreu2001, abreu2002, abreu2003, abreu2004, abreu2005, abreu2005ny, abreu2006, abreu2007, abreu2008, abreu2009, abreu2010, abreu2011)
#name the score column
names(career) = c(col, "score")





# Tribble for the Retrosheet event codes ----------------------------------
# Event codes table to be joined with count tables ------------
event_codes <- tribble(
  ~EVENT_CD, ~event,  
  0,    "Unknown event", 
  1,    "No event",
  2,    "Generic out",
  3,    "Strikeout",
  4,    "Stolen base",
  5,    "Defensive indifference",
  6,    "Caught stealing",
  7,    "Pickoff error",
  8,    "Pickoff",
  9,    "Wild pitch",
  10,   "Passed ball",
  11,   "Balk",
  12,   "Other advance",
  13,   "Foul error",
  14,   "Walk",
  15,   "Intentional walk",
  16,   "Hit by pitch",
  17,   "Interference",
  18,   "Error",
  19,   "Fielder's choice",
  20,   "Single",
  21,   "Double",
  22,   "Triple",
  23,   "Home run",
  24,   "Missing play"
)





# Non Clutch Situations ---------------------------------------------------

#make a dataframe for not clutch situations
not_clutch <- career %>%
  filter((INN_CT < 7 & BAT_EVENT_FL == TRUE) | ((score < -4 & BAT_EVENT_FL == TRUE) | score > 1 & INN_CT > 6 & BAT_EVENT_FL == TRUE))

#how did Abreu perform in nonclutch situations
cnt_not_clutch <- not_clutch %>%
  group_by(EVENT_CD) %>%
  count()

#join with the event codes table
cnt_not_clutch <- right_join(event_codes, cnt_not_clutch, by= "EVENT_CD")
cnt_not_clutch <- select(cnt_not_clutch, EVENT_CD, event, n)

#making not clutch variables
not_clutch_AB <- 3478 + 1459 + 65 + 1278 + 485 + 43 +253
not_clutch_PA <- sum(cnt_not_clutch$n)
not_clutch_hits <- 1278 + 485 + 43 +253
not_clutch_BB <- 1122+62
not_clutch_ba <- not_clutch_hits/not_clutch_AB
not_clutch_obp <- (not_clutch_hits + not_clutch_BB+ 24)/not_clutch_PA
not_clutch_slug <- (1278 + (485*2) + (43*3) + (253*4))/not_clutch_AB
not_clutch_ops <- not_clutch_obp + not_clutch_slug
not_clutch_ob <- not_clutch_hits+not_clutch_BB+26+16
not_clutch_unint_BB <- 1122
not_clutch_int_BB <- 62
not_clutch_1B <- 1278
not_clutch_2B <- 485
not_clutch_3B <- 43
not_clutch_HR <- 253
not_clutch_SO <- 1459
not_clutch_field_out <- 3478
not_clutch_misc <- 26+65+16

not_clutch_total <- tribble(
  ~Event, ~Count,
  "unint_BB", not_clutch_unint_BB,
  "int_BB", not_clutch_int_BB,
  "1B", not_clutch_1B,
  "2B", not_clutch_2B,
  "3B", not_clutch_3B,
  "HR", not_clutch_HR,
  "SO", not_clutch_SO,
  "field out", not_clutch_field_out,
  "misc", not_clutch_misc
)





# Clutch Situations -------------------------------------------------------

#make a dataframe for clutch situations
clutchcareer <- career %>%
  filter((score > -4 & score < 1 & INN_CT > 6 & BAT_EVENT_FL == TRUE) | (score == -4 & !is.na(BASE1_RUN_ID) & !is.na(BASE2_RUN_ID) & !is.na(BASE3_RUN_ID) & INN_CT > 6 & BAT_EVENT_FL == TRUE))

#How did Abreu perform in clutch situations
cnt_clutch_career <- clutchcareer %>%
  group_by(EVENT_CD) %>%
  count() 

#Joining the event codes tribble with the count table
cnt_clutch_career <- right_join(event_codes, cnt_clutch_career, by= "EVENT_CD")
cnt_clutch_career <- select(cnt_clutch_career, EVENT_CD, event, n)

#making clutch variables
clutch_AB <- 496+282+219+73+10+40+1+9 
clutch_PA <- sum(cnt_clutch_career$n) 
clutch_hits <- 219+73+10+40
clutch_BB <- 208+47
clutch_ba <- clutch_hits/clutch_AB
clutch_obp <- (clutch_hits+clutch_BB+8)/clutch_PA
clutch_slug <- (219+(73*2)+(10*3)+(40*4))/clutch_AB
clutch_ops <- clutch_slug+clutch_obp
clutch_ob <- clutch_hits+clutch_BB+39
clutch_unint_BB <- 208
clutch_int_BB <- 47
clutch_1B <- 219
clutch_2B <- 73
clutch_3B <- 10
clutch_HR <- 40
clutch_SO <- 282
clutch_field_out <- 496
clutch_misc <- 8+9+1

clutch_total <- tribble(
  ~Event, ~Count,
  "unint_BB", clutch_unint_BB,
  "int_BB", clutch_int_BB,
  "1B", clutch_1B,
  "2B", clutch_2B,
  "3B", clutch_3B,
  "HR", clutch_HR,
  "SO", clutch_SO,
  "field out", clutch_field_out,
  "misc", clutch_misc
)
clutch_total %>%
  View()





# RISP -----------------------------------------------


#lets look at clutch plays when men were on base
clutch_risp <- clutchcareer %>%
  filter(!is.na(BASE2_RUN_ID) | !is.na(BASE3_RUN_ID))

#count how he did
cnt_clutch_risp <- clutch_risp %>%
  group_by(EVENT_CD) %>%
  count() 
#join
cnt_clutch_risp <- right_join(event_codes, cnt_clutch_risp, by= "EVENT_CD")

#create risp variables
risp_BB <- 48+47
risp_hits <- 54+15+2+8
risp_ab <- 124+65+4+54+15+2+8
risp_ba <- risp_hits/risp_ab
risp_obp <- (risp_BB+risp_hits+5)/369
risp_slug <- (54+30+6+32)/risp_ab
risp_ops <- risp_slug+risp_obp
risp_pa <- sum(cnt_clutch_risp$n)
risp_ob <- sum(risp_BB+risp_hits+2)
risp_unint_BB <- 48
risp_int_BB <- 47
risp_1B <- 54
risp_2B <- 15
risp_3B <- 2
risp_HR <- 8
risp_SO <- 65
risp_field_out <- 124
risp_misc <- 1+4+1

#table of at bat
risp_total <- tribble(
  ~Event, ~Count,
  "unint_BB", risp_unint_BB,
  "int_BB", risp_int_BB,
  "1B", risp_1B,
  "2B", risp_2B,
  "3B", risp_3B,
  "HR", risp_HR,
  "SO", risp_SO,
  "field out", risp_field_out,
  "risp_misc", risp_misc
)
risp_total %>%
  View()
  

# Total Variables ---------------------------------------------------------
total_hits <- not_clutch_hits+clutch_hits
total_bb <- not_clutch_BB+clutch_BB
total_ob <- not_clutch_ob+clutch_ob
total_pa <- not_clutch_PA+clutch_PA
total_ab <- not_clutch_AB+clutch_AB





# Tabulating his carreer --------------------------------------------------


#Table comparing him situationally
#alternatively I could have created lists for each row but
#I wanted to be clearer.
#also I realized I forgot to create PA for RISP when I got here
Abreu_stats <- tribble(
  ~Situation, ~PA, ~AB, ~BA, ~all_BB, ~H, ~OBP, ~SLUG, ~OPS, ~unint_BB,
  "Non-Clutch", not_clutch_PA, not_clutch_AB, not_clutch_ba, not_clutch_BB, not_clutch_hits, not_clutch_obp, not_clutch_slug, not_clutch_ops, (not_clutch_BB-62),
  
  "Clutch", clutch_PA, clutch_AB, clutch_ba, clutch_BB, clutch_hits, clutch_obp, clutch_slug, clutch_ops, (clutch_BB-47),
  
  "RISP", sum(cnt_clutch_risp$n), risp_ab, risp_ba, risp_BB, risp_hits, risp_obp, risp_slug, risp_ops, (risp_BB-47),
  
  "Total", (not_clutch_PA+clutch_PA), (not_clutch_AB+clutch_AB), ((not_clutch_hits+clutch_hits)/(not_clutch_AB+clutch_AB)), (not_clutch_BB+clutch_BB), (not_clutch_hits+clutch_hits), (total_ob/total_pa), (((219+(73*2)+(10*3)+(40*4))+(1278 + (485*2) + (43*3) + (253*4)))/total_ab), ((total_ob/total_pa) +((((219+(73*2)+(10*3)+(40*4))+(1278 + (485*2) + (43*3) + (253*4)))/total_ab))), ((clutch_BB-47)+(not_clutch_BB-62))
)


#adding walk rate column
Abreu_stats <- Abreu_stats %>%
  mutate('BB%' = all_BB/PA)





# Visualizations ----------------------------------------------------------
library(ggplot2)
library(ggthemes)

#non log example
ggplot(data=not_clutch_total) +
  geom_col(mapping=aes(x=reorder(Event, -Count), y=(Count), fill=Event)) +
  coord_polar() +
  theme_economist() +
  xlab(label = "Event") +
  theme(legend.position = "None") +
  ggtitle(label = "Non-Clutch Events") 


#notclutch plot
ggplot(data=not_clutch_total) +
  geom_col(mapping=aes(x=reorder(Event, -Count), y=log(Count), fill=Event)) +
  coord_polar() +
  theme_economist() +
  xlab(label = "Event") +
  theme(legend.position = "None") +
  ggtitle(label = "Non-Clutch Events") 

#clutch plot
clutch_plot <- ggplot(data=clutch_total) +
  geom_col(mapping=aes(x=reorder(Event, -Count), y=log(Count), fill=Event)) +
  coord_polar() +
  theme_economist() +
  xlab(label = "Event") +
  theme(legend.position = "None") +
  ggtitle(label = "Clutch Events") 

#risp plot
risp_plot <- ggplot(data=risp_total) +
  geom_col(mapping=aes(x=reorder(Event, -Count), y=log(Count), fill=Event)) +
  coord_polar() +
  theme_economist() +
  xlab(label = "Event") +
  theme(legend.position = "None") +
  ggtitle(label = "RISP Events") 
risp_plot

setwd("~/blog/Bobby abreu")
#moving the plot in Excel for some reshaping
write_csv(Abreu_stats, "bobby_stats")
#and back in
bobby_stats <- read_excel("bobby_stats.xlsx")
#it now looks like this
# A tibble: 40 x 3
"Situation  Type        Value
<chr>      <chr>       <dbl>
  1 Non-Clutch PA       8287    
2 Non-Clutch AB       7061    
3 Non-Clutch BA          0.292
4 Non-Clutch all_BB   1184    
5 Non-Clutch H        2059    
6 Non-Clutch OBP         0.394
7 Non-Clutch SLUG        0.480
8 Non-Clutch OPS         0.874
9 Non-Clutch unint_BB 1122    
10 Non-Clutch BB%         0.143
# ... with 30 more rows"

total_plot <- bobby_stats %>%
  filter(Type == "BA" | Type == "OBP" | Type == "OPS" | Type == "SLUG") %>%
  ggplot() +
  geom_col(mapping=aes(x=reorder(Type, Value), y=Value, fill=Situation), position = "dodge") +
  theme_fivethirtyeight() +
  ylim(0,1) +
  ggtitle(label = "Hitting Statistics across different Situations")
