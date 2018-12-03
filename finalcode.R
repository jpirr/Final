library(tidyverse)
library(dplyr)
library(stringr)

plays.09 <- read_csv("pbpdata/reg_pbp_2009.csv")
plays.10 <- read_csv("pbpdata/reg_pbp_2010.csv")
plays.11 <- read_csv("pbpdata/reg_pbp_2011.csv")
plays.12 <- read_csv("pbpdata/reg_pbp_2012.csv")
plays.13 <- read_csv("pbpdata/reg_pbp_2013.csv")
plays.14 <- read_csv("pbpdata/reg_pbp_2014.csv")
plays.15 <- read_csv("pbpdata/reg_pbp_2015.csv")
plays.16 <- read_csv("pbpdata/reg_pbp_2016.csv")
plays.17 <- read_csv("pbpdata/reg_pbp_2017.csv")

#data is huge so need to only get the variables we need across all the data frame

plays.09 <- plays.09 %>%  
  select(game_id, passer_player_name, qb_dropback, 
         qb_scramble, sack, pass_attempt, penalty_type, 
         interception, qb_hit, touchdown, complete_pass, ydsnet) %>% 
  filter(passer_player_name == "B.Roethlisberger"|
         passer_player_name =="A.Rodgers"| 
         passer_player_name =="T.Brady"|
         passer_player_name =="E.Manning") %>% 
  mutate(year = str_sub(game_id, start = 1, end = 4)) 

#Need to get the relivant stats for each player by year

#2009 completion percentage
comp_pct09 <- plays.09 %>% 
  group_by(passer_player_name) %>% 
  count(complete_pass) %>% 
  mutate(comp_pct = round(n/sum(n)*100)) %>% 
  filter(complete_pass == "1") %>% 
  select(passer_player_name, comp_pct)

#2009 Touchdowns 
tds09 <- plays.09 %>% 
  group_by(passer_player_name) %>% 
  count(touchdown) %>% 
  filter(touchdown =="1") %>% 
  mutate(touchdowns = n) %>% 
  select(passer_player_name, touchdowns)
  
#2009 interceptions 
int09 <- plays.09 %>%  
  group_by(passer_player_name) %>% 
  count(interception) %>% 
  filter(interception =="1") %>% 
  mutate(interceptions = n) %>% 
  select(passer_player_name, interceptions)

#2009 passing yards
#py09 <- plays.09 %>% 
  #group_by(passer_player_name) %>% 
  #count(ydsnet) %>%
  #mutate(passingyards = sum(ydsnet)) %>%  
  #select(passer_player_name, passingyards) %>% 

#2009 hits
hits09 <- plays.09 %>% 
  group_by(passer_player_name) %>% 
  count(qb_hit) %>% 
  filter(qb_hit =="1") %>% 
  mutate(Hits = n) %>% 
  select(passer_player_name, Hits)
  
#2009 sack
sack09 <- plays.09 %>%  
  group_by(passer_player_name) %>% 
  count(sack) %>% 
  filter(sack =="1") %>% 
  mutate(Sacks = n) %>% 
  select(passer_player_name, Sacks)

stats09a <- left_join(comp_pct09, tds09, by = "passer_player_name")
stats09b <- left_join(stats09a, int09, by = "passer_player_name")
stats09c <- left_join(stats09b, hits09, by = "passer_player_name")
stats09d <- left_join(stats09c, sack09, by = "passer_player_name")

stats09 <- stats09d %>% 
  mutate(year = "2009")

#2010
  
plays.10 <- plays.10 %>%  
  select(game_id, passer_player_name, qb_dropback, 
         qb_scramble, sack, pass_attempt, penalty_type, 
         interception, qb_hit, touchdown, complete_pass, ydsnet) %>% 
  filter(passer_player_name == "B.Roethlisberger"|
           passer_player_name =="A.Rodgers"| 
           passer_player_name =="T.Brady"|
           passer_player_name =="E.Manning") %>% 
  mutate(year = str_sub(game_id, start = 1, end = 4))

#completion percentage
comp_pct10 <- plays.10 %>% 
  group_by(passer_player_name) %>% 
  count(complete_pass) %>% 
  mutate(comp_pct = round(n/sum(n)*100)) %>% 
  filter(complete_pass == "1") %>% 
  select(passer_player_name, comp_pct)

#Touchdowns 
tds10 <- plays.10 %>% 
  group_by(passer_player_name) %>% 
  count(touchdown) %>% 
  filter(touchdown =="1") %>% 
  mutate(touchdowns = n) %>% 
  select(passer_player_name, touchdowns)

#interceptions 
int10 <- plays.10 %>%  
  group_by(passer_player_name) %>% 
  count(interception) %>% 
  filter(interception =="1") %>% 
  mutate(interceptions = n) %>% 
  select(passer_player_name, interceptions)

#passing yards
#py09 <- plays.09 %>% 
#group_by(passer_player_name) %>% 
#count(ydsnet) %>%
#mutate(passingyards = sum(ydsnet)) %>%  
#select(passer_player_name, passingyards) %>% 

#hits
hits10 <- plays.10 %>% 
  group_by(passer_player_name) %>% 
  count(qb_hit) %>% 
  filter(qb_hit =="1") %>% 
  mutate(Hits = n) %>% 
  select(passer_player_name, Hits)

#sack
sack10 <- plays.10 %>%  
  group_by(passer_player_name) %>% 
  count(sack) %>% 
  filter(sack =="1") %>% 
  mutate(Sacks = n) %>% 
  select(passer_player_name, Sacks)

stats10a <- left_join(comp_pct10, tds10, by = "passer_player_name")
stats10b <- left_join(stats10a, int10, by = "passer_player_name")
stats10c <- left_join(stats10b, hits10, by = "passer_player_name")
stats10d <- left_join(stats10c, sack10, by = "passer_player_name")

stats10 <- stats10d %>% 
  mutate(year = "2010")

#2011
plays.11 <- plays.11 %>%  
  select(game_id, passer_player_name, qb_dropback, 
         qb_scramble, sack, pass_attempt, penalty_type, 
         interception, qb_hit, touchdown, complete_pass, ydsnet) %>% 
  filter(passer_player_name == "B.Roethlisberger"|
           passer_player_name =="A.Rodgers"| 
           passer_player_name =="T.Brady"|
           passer_player_name =="E.Manning") %>% 
  mutate(year = str_sub(game_id, start = 1, end = 4))

#completion percentage
comp_pct11 <- plays.11 %>% 
  group_by(passer_player_name) %>% 
  count(complete_pass) %>% 
  mutate(comp_pct = round(n/sum(n)*100)) %>% 
  filter(complete_pass == "1") %>% 
  select(passer_player_name, comp_pct)

#Touchdowns 
tds11 <- plays.11 %>% 
  group_by(passer_player_name) %>% 
  count(touchdown) %>% 
  filter(touchdown =="1") %>% 
  mutate(touchdowns = n) %>% 
  select(passer_player_name, touchdowns)

#interceptions 
int11 <- plays.11 %>%  
  group_by(passer_player_name) %>% 
  count(interception) %>% 
  filter(interception =="1") %>% 
  mutate(interceptions = n) %>% 
  select(passer_player_name, interceptions)

#passing yards
#py09 <- plays.09 %>% 
#group_by(passer_player_name) %>% 
#count(ydsnet) %>%
#mutate(passingyards = sum(ydsnet)) %>%  
#select(passer_player_name, passingyards) %>% 

#hits
hits11 <- plays.11 %>% 
  group_by(passer_player_name) %>% 
  count(qb_hit) %>% 
  filter(qb_hit =="1") %>% 
  mutate(Hits = n) %>% 
  select(passer_player_name, Hits)

#sack
sack11 <- plays.11 %>%  
  group_by(passer_player_name) %>% 
  count(sack) %>% 
  filter(sack =="1") %>% 
  mutate(Sacks = n) %>% 
  select(passer_player_name, Sacks)

stats11a <- left_join(comp_pct11, tds11, by = "passer_player_name")
stats11b <- left_join(stats11a, int11, by = "passer_player_name")
stats11c <- left_join(stats11b, hits11, by = "passer_player_name")
stats11d <- left_join(stats11c, sack11, by = "passer_player_name")

stats11 <- stats11d %>% 
  mutate(year = "2011")

plays.12 <- plays.12 %>%  
  select(game_id, passer_player_name, qb_dropback, 
         qb_scramble, sack, pass_attempt, penalty_type, 
         interception, qb_hit, touchdown, complete_pass, ydsnet) %>% 
  filter(passer_player_name == "B.Roethlisberger"|
           passer_player_name =="A.Rodgers"| 
           passer_player_name =="T.Brady"|
           passer_player_name =="E.Manning") %>% 
  mutate(year = str_sub(game_id, start = 1, end = 4))

plays.13 <- plays.13 %>%  
  select(game_id, passer_player_name, qb_dropback, 
         qb_scramble, sack, pass_attempt, penalty_type, 
         interception, qb_hit, touchdown, complete_pass, ydsnet) %>% 
  filter(passer_player_name == "B.Roethlisberger"|
           passer_player_name =="A.Rodgers"| 
           passer_player_name =="T.Brady"|
           passer_player_name =="E.Manning") %>% 
  mutate(year = str_sub(game_id, start = 1, end = 4))

plays.14 <- plays.14 %>%  
  select(game_id, passer_player_name, qb_dropback, 
         qb_scramble, sack, pass_attempt, penalty_type, 
         interception, qb_hit, touchdown, complete_pass, ydsnet) %>% 
  filter(passer_player_name == "B.Roethlisberger"|
           passer_player_name =="A.Rodgers"| 
           passer_player_name =="T.Brady"|
           passer_player_name =="E.Manning") %>% 
  mutate(year = str_sub(game_id, start = 1, end = 4))

plays.15 <- plays.15 %>%  
  select(game_id, passer_player_name, qb_dropback, 
         qb_scramble, sack, pass_attempt, penalty_type, 
         interception, qb_hit, touchdown, complete_pass, ydsnet) %>% 
  filter(passer_player_name == "B.Roethlisberger"|
           passer_player_name =="A.Rodgers"| 
           passer_player_name =="T.Brady"|
           passer_player_name =="E.Manning") %>% 
  mutate(year = str_sub(game_id, start = 1, end = 4))

plays.16 <- plays.16 %>%  
  select(game_id, passer_player_name, qb_dropback, 
         qb_scramble, sack, pass_attempt, penalty_type, 
         interception, qb_hit, touchdown, complete_pass, ydsnet) %>% 
  filter(passer_player_name == "B.Roethlisberger"|
           passer_player_name =="A.Rodgers"| 
           passer_player_name =="T.Brady"|
           passer_player_name =="E.Manning") %>% 
  mutate(year = str_sub(game_id, start = 1, end = 4))

plays.17 <- plays.17 %>%  
  select(game_id, passer_player_name, qb_dropback, 
         qb_scramble, sack, pass_attempt, penalty_type, 
         interception, qb_hit, touchdown, complete_pass, ydsnet) %>% 
  filter(passer_player_name == "B.Roethlisberger"|
           passer_player_name =="A.Rodgers"| 
           passer_player_name =="T.Brady"|
           passer_player_name =="E.Manning") %>% 
  mutate(year = str_sub(game_id, start = 1, end = 4))

#Now that I have all the variables and observations I need to combine all the dfs into one. 

all1 <- rbind(plays.09, plays.10)
all2 <- rbind(all1, plays.11)
all3 <- rbind(all2, plays.12)
all4 <- rbind(all3, plays.13)
all5 <- rbind(all4, plays.14)
all6 <- rbind(all5, plays.15)
all7 <- rbind(all6, plays.16)
all8 <- rbind(all7, plays.17)

#data on the qbs from 2009-2017




 # rename(all8, c(passer_player_name = QB, 
                # ydsnet = Yards, 
                 #touchdown = Touchdown, 
                 #penalty_type = Penalty, 
                 #qb_scramble = Scramble, 
                 #qb_hit = Hit))

write_rds(all8, "QBs.rds", compress = "none")


  