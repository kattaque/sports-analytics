#preliminary plots
plot(m_cleaned$age,m_cleaned$PPG)
hist(m_cleaned$age)
hist(log(m_cleaned$PPG), breaks = 60)

#Plots of the response variable. 
# Make a few different versions of this plot, e.g. split by league, position, season, etc.

#For PPG
plot(m_cleaned$age,m_cleaned$PPG,main = "Points per Game distribution over different Ages",xlab = "Age",ylab = "Points per Game")
hist(m_cleaned$age)
hist(log(m_cleaned$PPG), breaks = 60)

#For Percentile (accounting for age,league,position)
plot(pctfull$league_id, pctNHL$percrank,main = "Percentile distribution over different Leagues")
barplot(pctNHL$percrank)
hist(pctage$percrank)
h1 <- ggplot(pctNHL) + geom_point(aes(x = league_id, y = percrank,colour = league_id)) + theme(legend.position = "none") 

library(reshape2)
library(plotly)

hp <- ggplot(pctage, aes(x=percrank)) + geom_histogram(binwidth=0.1,colour="red")

# Histogram of total_bill, divided by sex and smoker
hp + facet_grid(. ~ position)

ggplotly()

#plot 
g <- ggplot(df1, aes(year)) + geom_hline(yintercept = 0)
g <- g + geom_line(aes(y=shl), colour="red")
g <- g + geom_line(aes(y=nhl), colour="green")
g <- g + geom_line(aes(y=ohl), colour="blue")
g <- g + geom_line(aes(y=whl), colour="yellow") + labs(title = "NHL/SHL/WHL/OHL",x = "Years",y="Strength Coefficient")
g

#creating a dataframe for all the coefficients in order of year
library(plyr)
temp <- rbind.fill.matrix(t(centered_coefficients98),t(centered_coefficients99),t(centered_coefficients00),t(centered_coefficients01),t(centered_coefficients02),t(centered_coefficients03),t(centered_coefficients04),t(centered_coefficients05),t(centered_coefficients06),t(centered_coefficients07),t(centered_coefficients08), t(centered_coefficients09),t(centered_coefficients10),t(centered_coefficients11),t(centered_coefficients12),t(centered_coefficients13),t(centered_coefficients14),t(centered_coefficients15),t(centered_coefficients16),t(centered_coefficients17))
#temp[is.na(temp)] <- 0
year <- c(2008:2018)
new_m <- cbind(year,temp)

# Create Line Chart as time series
histzscore = hist(logppg2$zscore,main = "Histogram of Zscores of Log(PPG)",xlab = "Zscore")
dfnew2 <- dfnew1 %>% gather(League, Coefficient, 2:46)
TimeSeriesData <- ts(aa[,-9], start = 1998, end = 2017)
plot.ts(TimeSeriesData,type= "o",main ="League Strength over time using Adjusted PPG", xlab = "Years", las=0, ylab = "Coefficient")



#Plotting a small subset of leagues on same graph (preliminary)
g <- ggplot(df_btm, aes(year)) + geom_hline(yintercept = 0)
g <- g + geom_line(aes(y=shl), colour="red")
g <- g + geom_line(aes(y=nhl), colour="green")
g <- g + geom_line(aes(y=ohl), colour="blue")
g <- g + geom_line(aes(y=whl), colour="yellow") + labs(title = "NHL/SHL/WHL/OHL",x = "Years",y="Strength Coefficient")
g



#creating a wide data frame of coefficient matrix
#df3 <- df1 %>% gather(League, Coefficient, 2:46) #no NA
df1 <- df_btm %>% gather(League, Coefficient, 2:43) #keeps NA
df_zscore <- hockey_zscores %>% gather(League, Coefficient, 2:43)
df_zscore2 <- df_zscore %>% filter(League != "usdp")
df_btm1 <- df_btm %>% filter(League %in% group1)
df_zscore1 <- df_zscore %>% filter(League %in% group1)

#new subsets with certain leagues 
library(ggrepel)
plot1 <- df_btm1 %>% mutate(label = if_else(year == max(year), as.character(League), NA_character_)) %>%
  ggplot(aes(x = year, y = Coefficient, group = League, colour = League)) + 
  geom_line() + theme(legend.position = "none") + geom_point() +
  geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE) +
  ggtitle("Strength Coefficient of Leagues Over 20 Seasons (Using BTM)")

ccc <- merge(hockey_zscores,df1,by="year", all = T)
#Plotting BTM and ELO together
g1 <- ggplot(ccc, aes(year)) + geom_line(aes(y=ohl.x), colour="salmon") + geom_point(aes(y=ohl.x,x=year), color = "salmon3")
g1 <- g1 + geom_line(aes(y=ohl.y), colour="turquoise") + labs(title = "Bradley Terry vs Self-Built ELO",x = "Years",y="Strength Coefficient")
g1 <- g1 + geom_point(aes(y=ohl.y,x=year), color = "turquoise4")


#subset graph
df5 <- aa %>% gather(League, Coefficient, 2:17)
h <- ggplot(df4) + geom_line(aes(x = year, y = Coefficient, group = League, colour = League)) + xlim(2005,2017) + ylim(-5,5)
#Another plot with ALL leagues and labels
Plot_df <- df5 %>% mutate_if(is.factor, as.character) %>% mutate(year = as.numeric(year))
messy <- ggplot() + geom_point() + geom_line(data = Plot_df, aes(year, Coefficient, color = League)) 
messy + geom_hline(yintercept = 0) 

# currently: OLS
plot1 <- df1 %>% mutate(label = if_else(year == max(year), as.character(League), NA_character_)) %>%
  ggplot(aes(x = year, y = Coefficient, group = League, colour = League)) + 
  geom_line() + theme(legend.position = "none") + geom_point() +
  geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE) + labs(title = "Self-Built ELO using OLS linear regression",x = "Years",y="Strength Coefficient")

### currently: Log
plot2 <- df_zscore2 %>% mutate(label = if_else(year == max(year), as.character(League), NA_character_)) %>%
  ggplot(aes(x = year, y = Coefficient, group = League, colour = League)) + 
  geom_line() + theme(legend.position = "none") + geom_point() + xlim(2008, 2017) + ylim(-5, 5) +
  geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE) + labs(title = "Self-Built ELO using Logistic Regression",x = "Years",y="Strength Coefficient")

# plots with only PPG instead of percentiles
dfnew1 <- data.frame(co_eff2)
dfnew2 <- dfnew1 %>% gather(League, Coefficient, 2:46)
TimeSeriesData <- ts(dfnew1[,2:11], start = 1998, end = 2017)
plot.ts(TimeSeriesData,type= "o",main ="League Strength over time using PPG", xlab = "Years", las=0, ylab = "Coefficient")

# plots with percentiles
TimeSeriesData1 <- ts(df2[,2:11], start = 1998, end = 2017)
plot.ts(TimeSeriesData1,type= "o",main ="League Strength over time using percentiles", xlab = "Years", las=0, ylab = "Coefficient")

#bar plot of average # of goals per league
scoring1 <- merged.data %>% group_by(league_id) %>% summarise(avg_goals = mean(G,na.rm = TRUE)) %>% arrange(-avg_goals)

# Horizontal 
ggplot(scoring1, aes(x=league_id, y=avg_goals)) +
  geom_segment( aes(xend=league_id, y=0, yend=avg_goals), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) + theme_light() + coord_flip() +
  theme( panel.grid.major.y = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank()) + 
  xlab("Leagues") + ylab("Scoring Strength")

# Reorder
scoring %>%
  mutate(league_id = fct_reorder(league_id, avg_goals)) %>% ggplot( aes(x=league_id, y=avg_goals)) +
  geom_segment( aes(x=league_id, xend=league_id, y=0, yend=avg_goals), color="skyblue", size=1) +
  geom_point( color="blue", size=4, alpha=0.6) + theme_light() + coord_flip() +
  theme( panel.grid.major.y = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank()) +
  xlab("Leagues") +ylab("Scoring Strength") +ggtitle("Scoring Strength by League")

#if you have specific leagues u wanna choose...
library(dplyr, warn.conflicts = FALSE)
imptLeague <- c("nhl","shl","ahl","khl","czech","liiga","echl","vhl")
sam <- c("whl","ushl")
najunior <- c("ohl","qmjhl","whl","bchl","ushl","nahl","ojhl","ncaa")
d_filtered <- ols_long %>% group_by(League) %>% filter(League %in% najunior) %>% ungroup()
d_filtered2 <- ols_long %>% group_by(League) %>% filter(League %in% sam) %>% ungroup()


p2 <- ggplot() +
  geom_line(aes(year, Coefficient, group = League), data = ols_long, colour = alpha("grey", 0.7))  +
  geom_line(aes(year, Coefficient, colour = League), data = d_filtered) +  geom_point()+ ggtitle("USHL and WHL compared to all leagues") + ylab("Ranks") +
  scale_x_continuous(name = "Years", breaks = scales::pretty_breaks(n = 10)) + theme(plot.title = element_text(size=22)) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16))

p3 <- ggplot() +
  geom_line(aes(year, Coefficient, group = League), data = d_filtered, colour = alpha("grey", 0.7))  +
  geom_line(aes(year, Coefficient, colour = League), data = d_filtered2) +  geom_point()+ ggtitle("USHL and WHL compared to NA Junior Leagues") + ylab("Ranks") +
  scale_x_continuous(name = "Years", breaks = scales::pretty_breaks(n = 10)) + theme(plot.title = element_text(size=22)) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16))



plot3 <- d_filtered %>% mutate(label = if_else(year == max(year), as.character(League), NA_character_)) %>%
  ggplot(aes(x = year, y = Coefficient, group = League, colour = League)) + 
  geom_line(aes(year,Coefficient, colour = League), data = d_filtered, colour = alpha("grey", 0.7)) + theme_minimal() +theme(legend.position = "none") + geom_point() +
  geom_line(aes(year, Coefficient, colour = League), data = d_filtered2, size=1.5) + geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE) +  ggtitle("USHL and WHL compared to NA Junior Leagues") +  ylab("Ranks") +
  scale_x_continuous(name = "Years", breaks = scales::pretty_breaks(n = 10)) + theme(plot.title = element_text(size=22)) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16))
# just general highlighting 
library(scales)
gghighlight_line(ols_long, aes(year, Coefficient, colour = League), max(Coefficient) > 0 )+ geom_point() +theme_minimal() + ggtitle("Strength Coefficient of Leagues Over 10 Seasons (Using OLS)")+ 
  scale_x_continuous(name = "Years", breaks = scales::pretty_breaks(n = 10)) + theme(plot.title = element_text(size=22)) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16))



gghighlight_line(ranking_leagues, aes(year, rank, colour = league), max(rank) < 16 )+  scale_y_reverse() + geom_point() +theme_minimal() +  ylab("Ranks") +
  labs(title = "League Rankings over Time")+ scale_x_continuous(name = "Years", breaks = scales::pretty_breaks(n = 10)) + theme(plot.title = element_text(size=22)) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16))


#BTM

year <- c(2008:2018)
df_btm1 <- cbind(year,data_btm)
df_btm <- data.frame(df_btm1)
btm_long <- df_btm %>% gather(League, Coefficient, 2:43)
gghighlight_line(btm_long, aes(year, Coefficient, colour = League), max(Coefficient) > -5 )+ geom_point() +theme_minimal() + ggtitle("Strength Coefficient of Leagues Over 10 Seasons (Using BTM)")+ scale_x_continuous(name = "Years", breaks = scales::pretty_breaks(n = 10))


#logis
df_log <- data.frame(hockey_zscores[10:20,])
log_long <- df_log %>% gather(League, Coefficient, 2:43)
gghighlight_line(log_long, aes(year, Coefficient, colour = League), max(Coefficient) > 0.5 )+ geom_point() +theme_minimal() + ggtitle("Strength Coefficient of Leagues Over 10 Seasons (Using Logistic Regression)")+ scale_x_continuous(name = "Years", breaks = scales::pretty_breaks(n = 10)) + ylim(-5,5)



#bump chart
ranking_leagues <-readRDS('~/Desktop/CMSAC/CMU_project/linear_ranks.RData')
library(dplyr, warn.conflicts = FALSE)
imptLeague <- c("nhl","ohl","shl","ahl","khl","qmjhl","ncaa","mestis","del","czech","liiga")
d_filtered <- ranking_leagues %>% group_by(league) %>% filter(league %in% imptLeague) %>% ungroup()
ranking_leagues1 <- ranking_leagues %>% mutate(label = if_else(year == max(year), as.character(league), NA_character_))
p2 <- ggplot() + geom_line(aes(year, rank, group = league), data = ranking_leagues, colour = alpha("grey", 0.7)) + 
  geom_line(aes(year,rank, colour = league), data = d_filtered) + geom_point() + theme(legend.position = "none") +
  labs(title = "League Rankings over Time") + scale_y_reverse() + xlab("Year") + ylab("Rank") +theme_minimal() + 
  scale_x_continuous(name = "Years", breaks = scales::pretty_breaks(n = 10)) 
p2

library(ggrepel)
plot2 <- ranking_leagues %>% mutate(label = if_else(year == max(year), as.character(league), NA_character_)) %>%
  ggplot(aes(x = year, y = rank, group = league, colour = League)) + geom_line(aes(year, rank, group = league), data = ranking_leagues, colour = alpha("grey", 0.7)) + geom_line(aes(year,rank, colour = league), data = d_filtered) + theme(legend.position = "none") + geom_point() +
  geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE) +  ggtitle("League Rankings over Time")
plot2


