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

#data is huge so need to only get the variables we need

plays.09 <- plays.09 %>%  
  select(passer_player_name, qb_dropback, qb_scramble, sack, pass_attempt, penalty_type, interception, qb_hit, touchdown, complete_pass) %>% 
  filter(passer_player_name == "B.Roethlisberger"|
         passer_player_name =="A.Rodgers"| 
         passer_player_name =="T.Brady"|
         passer_player_name =="E.Manning") 

plays.10 <- plays.10 %>%  
  select(passer_player_name, qb_dropback, qb_scramble, sack, pass_attempt, penalty_type, interception, qb_hit, touchdown, complete_pass) %>% 
  filter(passer_player_name == "B.Roethlisberger"|
           passer_player_name =="A.Rodgers"| 
           passer_player_name =="T.Brady"|
           passer_player_name =="E.Manning") 

plays.11 <- plays.11 %>%  
  select(passer_player_name, qb_dropback, qb_scramble, sack, pass_attempt, penalty_type, interception, qb_hit, touchdown, complete_pass) %>% 
  filter(passer_player_name == "B.Roethlisberger"|
           passer_player_name =="A.Rodgers"| 
           passer_player_name =="T.Brady"|
           passer_player_name =="E.Manning") 

plays.12 <- plays.12 %>%  
  select(passer_player_name, qb_dropback, qb_scramble, sack, pass_attempt, penalty_type, interception, qb_hit, touchdown, complete_pass) %>% 
  filter(passer_player_name == "B.Roethlisberger"|
           passer_player_name =="A.Rodgers"| 
           passer_player_name =="T.Brady"|
           passer_player_name =="E.Manning") 

plays.13 <- plays.13 %>%  
  select(passer_player_name, qb_dropback, qb_scramble, sack, pass_attempt, penalty_type, interception, qb_hit, touchdown, complete_pass) %>% 
  filter(passer_player_name == "B.Roethlisberger"|
           passer_player_name =="A.Rodgers"| 
           passer_player_name =="T.Brady"|
           passer_player_name =="E.Manning") 

plays.14 <- plays.14 %>%  
  select(passer_player_name, qb_dropback, qb_scramble, sack, pass_attempt, penalty_type, interception, qb_hit, touchdown, complete_pass) %>% 
  filter(passer_player_name == "B.Roethlisberger"|
           passer_player_name =="A.Rodgers"| 
           passer_player_name =="T.Brady"|
           passer_player_name =="E.Manning") 

plays.15 <- plays.15 %>%  
  select(passer_player_name, qb_dropback, qb_scramble, sack, pass_attempt, penalty_type, interception, qb_hit, touchdown, complete_pass) %>% 
  filter(passer_player_name == "B.Roethlisberger"|
           passer_player_name =="A.Rodgers"| 
           passer_player_name =="T.Brady"|
           passer_player_name =="E.Manning") 

plays.16 <- plays.16 %>%  
  select(passer_player_name, qb_dropback, qb_scramble, sack, pass_attempt, penalty_type, interception, qb_hit, touchdown, complete_pass) %>% 
  filter(passer_player_name == "B.Roethlisberger"|
           passer_player_name =="A.Rodgers"| 
           passer_player_name =="T.Brady"|
           passer_player_name =="E.Manning") 

plays.17 <- plays.17 %>%  
  select(passer_player_name, qb_dropback, qb_scramble, sack, pass_attempt, penalty_type, interception, qb_hit, touchdown, complete_pass) %>% 
  filter(passer_player_name == "B.Roethlisberger"|
           passer_player_name =="A.Rodgers"| 
           passer_player_name =="T.Brady"|
           passer_player_name =="E.Manning") 

all1 <- rbind(plays.09, plays.10)
all2 <- rbind(all1, plays.11)
all3 <- rbind(all2, plays.12)
all4 <- rbind(all3, plays.13)
all5 <- rbind(all4, plays.14)
all6 <- rbind(all5, plays.15)
all7 <- rbind(all6, plays.16)
all8 <- rbind(all7, plays.17)

  