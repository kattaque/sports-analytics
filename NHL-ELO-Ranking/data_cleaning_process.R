#libraries to load
library(tidyverse)
library(lubridate)
library(eeptools)
library(reshape2)
library(plotly)
library(glmnet)

#read in data files
player_info <- readRDS('player_information.RData')
View(player_info)
season_stat <- readRDS('player_season_statistics.RData')
View(season_stat)
draft_data <- readRDS('NHL_draft_data.RData')

#merge the data together
merged.data <- merge(player_info, season_stat, by="player_id", all=TRUE)

#create function that transforms season to an usable date
season_to_date <- function(season) {
  year <- substr(season, start = 5, stop = 8)
  newdate <- paste(year,"-01-01", collapse = NULL)
  return(as.Date(ymd(newdate)))
}

# function for finding modes
Mode <- function(x) {
  u_x <- unique(x)
  u_x[which.max(tabulate(match(x, u_x)))]
}

#create age-at-season-start variable
merged.data %>% mutate(season_date = ymd(season_to_date(merged.data$season))) %>% mutate(age = (as.Date(season_date) - as.Date(birth_date))/365.25) %>% head
merged.data %>% mutate(season_date = ymd(season_to_date(merged.data$season))) %>% dplyr::filter(!is.na(birth_date)) %>% mutate(age = age_calc(as.Date(birth_date), season_date, units = "years", precise = FALSE)) %>% head

#new data frame w/ new variables (age, season date)
m_cleaned <- merged.data %>% mutate(season_date = ymd(season_to_date(merged.data$season))) %>% dplyr::filter(!is.na(birth_date)) %>% dplyr::mutate(age = age_calc(as.Date(birth_date), season_date, units = "years", precise = FALSE))

#add draft information to data set with stats and info and age if you wish
draft <- readRDS("NHL_draft_data.Rdata")
m_cleaned2 = merge(m_cleaned,draft , by="player_id")

#create goal/game and assist/game variable
m_cleaned$GPG = round(m_cleaned$G/m_cleaned$GP, digits = 2)
m_cleaned$APG = round(m_cleaned$A/m_cleaned$GP, digits = 2)

#creating new ppg meterics for defense and offense: for forwards, goals are weighted more heavily than assists, and vice versa for defensemen
forward_stat <- m_cleaned[ which(m_cleaned$position=='F'), ]
defense_stat <- m_cleaned[ which(m_cleaned$position=='D'), ]

forward_stat2 = forward_stat %>% dplyr::mutate(Fppg = (1.25*G + .75*A)/GP,Dppg = NA)
forward_stat3 = forward_stat2 %>% dplyr::mutate(logpg = log(PPG))
defense_stat2 = defense_stat %>% dplyr::mutate(Dppg = (.75*G + 1.25*A)/GP,Fppg = NA)

# info by age
m_cleaned %>% group_by(age) %>% summarise(n(),common_league = Mode(league_id),mean_ppg = mean(PPG,na.rm = TRUE)) %>% print(n = Inf)

# info by league 
m_cleaned %>% group_by(league_id) %>% summarise(n(),mean_age = mean(age, na.rm = TRUE),mean_ppg = mean(PPG,na.rm = TRUE)) %>% arrange(-mean_ppg) %>% filter(`n()` > 100) %>% print(n = Inf)

# info by position 
m_cleaned %>% group_by(league_id, position) %>% summarise(n(),mean_age = mean(age, na.rm = TRUE),mean_ppg = mean(PPG,na.rm = TRUE)) %>% arrange(-mean_ppg) %>% filter(`n()` > 100) %>% print(n = Inf)

#percentiles for position in your league subset NHL by position
NHL =m_cleaned[ which(m_cleaned$league_id=='nhl' ), ]
NHLF =m_cleaned[ which(m_cleaned$league_id=='nhl' & m_cleaned$position == 'F'), ]
pctNHL = m_cleaned %>% dplyr::group_by(position) %>% dplyr::mutate(percrank=rank(PPG)/length(PPG) * 100)
pctnhlf = m_cleaned %>% dplyr::mutate(percrank=rank(PPG)/length(PPG) * 100)

#percentiles for all datasets by position and league?
pctfull = m_cleaned %>% dplyr::group_by(league_id,position) %>% dplyr::mutate(percrank=rank(PPG)/length(PPG) * 100)

#percentiles with age position and league
pctage = m_cleaned %>% dplyr::group_by(league_id,position,age) %>% dplyr::mutate(percrank=rank(PPG)/length(PPG) * 100)
modeldf = pctfull %>% dplyr:: select(player_name.x,player_id,league_id,season,percrank,position)


#########
# CREATING ACTUAL RESPONSE VARIABLE WE USE

#getting leagues with enough observations
someleagues  = pctfull %>% dplyr::select(-percrank) 
someleagues2 = someleagues[ which(someleagues$league_id =='nhl'|someleagues$league_id =="ushs.prep"|someleagues$league_id =="cisaa"|someleagues$league_id =="j18-elit"|someleagues$league_id =="mjhl"|someleagues$league_id == "czech.u18"| someleagues$league_id=="cchl"|someleagues$league_id =="mphl"| someleagues$league_id =="ojhl"|someleagues$league_id =="ajhl"| someleagues$league_id =="j18-allsvenskan"| someleagues$league_id =="nahl"| someleagues$league_id =="bchl"| someleagues$league_id =="jr.-a-sm-liiga"|someleagues$league_id =="superelit"|someleagues$league_id =="qmjhl"| someleagues$league_id =="ohl"|someleagues$league_id =="ushl"| someleagues$league_id =="whl" | someleagues$league_id =="usdp"| someleagues$league_id =="eihl"| someleagues$league_id =="denmark" | someleagues$league_id =="mhl"| someleagues$league_id =="wjc-18" | someleagues$league_id =="division-1" | someleagues$league_id =="mestis"|someleagues$league_id =="ncaa" |someleagues$league_id =="czech2"| someleagues$league_id =="echl"| someleagues$league_id =="nlb"| someleagues$league_id =="wjc-20"| someleagues$league_id =="vhl"|someleagues$league_id =="allsvenskan"|someleagues$league_id =="del"| someleagues$league_id =="liiga"|someleagues$league_id =="czech"|someleagues$league_id =="nla" | someleagues$league_id =="ahl"| someleagues$league_id =="shl"|someleagues$league_id =="khl"| someleagues$league_id =="wc" |someleagues$league_id == "czech-u18"|someleagues$league_id=="ushs-mn" | someleagues$league_id=="ushs-prep") , ]

#getting the log of ppg
logppg = someleagues2 %>% dplyr:: filter(!is.na(PPG))%>% dplyr::group_by(age,position,league_id,season) %>% dplyr::mutate(logval = log(PPG+1)) %>% dplyr:: mutate(mean = mean(logval),standard_dev = sd(logval))
#notgroup = someleagues2 %>% dplyr::mutate(logval = log(PPG+1))

#finding zscores of data

#meanlog = mean(logppg$logval,na.rm=TRUE)
#stdvlog = sd(logppg$logval,na.rm=TRUE)
logppg2 = logppg %>% dplyr::mutate(zscore = (logval - mean)/standard_dev)