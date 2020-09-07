library(rpart)
library(tidyverse)
library(ggplot2)
library(ggmap)



FL <- read_csv('https://raw.githubusercontent.com/gitcnk/Data/master/ElectionData/Florida_before2016.csv')
covid_UScounties <- read.csv("https://raw.githubusercontent.com/DOUZIZIZIII/Stats1/master/all-states-history.csv")
covid_UScounties <- covid_UScounties %>%
  rename(State = state)
election <- read.csv(file = 'https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-16/master/US_County_Level_Presidential_Results_08-16.csv')

election$R_to_D_08 <-election$gop_2008/election$dem_2008
election$R_to_D_12 <-election$gop_2012/election$dem_2012
election$R_to_D_16 <-election$gop_2016/election$dem_2016

#how many days since covid started till today
days <- data.frame(date="2020/09/05",covid_start="2020/01/21")

days$date_diff <- as.Date(as.character(days$date), format="%Y/%m/%d")-
  as.Date(as.character(days$covid_start), format="%Y/%m/%d")
days

#FIXME
StdDev
volatility(covid_UScounties, n = 10, calc = "deathIncrease", N = days$date_diff, mean0 = FALSE, by = state)


#reopen date
reopen2 <- read.csv("https://raw.githubusercontent.com/DOUZIZIZIII/Stats1/master/reopen2.csv")

abbreviation<- read.csv("https://raw.githubusercontent.com/DOUZIZIZIII/Stats1/master/state.abbreviation.csv")

covid_reopen <- inner_join(abbreviation, reopen, by = 'State')
#need to slice and dice
covid_stability <- 
  
  ggplot(covid_UScounties,aes(y = deathIncrease, x = date, colour = state, shape = state)) +
  geom_point() + geom_smooth(method = "lm", fill = NA)

#plot to see the overall trend of covid stability of each states.. this is a little massive right now so there might be a better way
covid_stability = ggplot()+
  geom_point(data = covid_UScounties, mapping = aes(x = date, y = deathincrease))+
  geom_line(data = covid_UScounties, mapping = aes(x = date, y = ma(covid_UScounties$deathincrease, order = 7)),col = 'red')




#FL_subset <- all.states.history %>%
#  select(male, female, education_index, income, unemployment_rate, white, county, votes_d_received, votes_r_received, votes_d_received_2012, votes_r_received_2012)

FL_health_subset <- all.states.history %>%
  filter(state == 'FL')

CO_health_subset <- all.states.history %>%
  filter(state == 'CO')


FL_full <- inner_join(FL_subset, FL_covid_subset, by = 'county')



FL_subset$D <- (FL_subset$votes_d_received+FL_subset$votes_d_received_2012)/2
FL_subset$R <- (FL_subset$votes_r_received+FL_subset$votes_r_received_2012)/2

FL_subset$wa_result <- ifelse(FL_subset$D > FL_subset$R, 'D', 'R')

FL_subset$M_to_F <- FL_subset$male/FL_subset$female

plot (FL_subset$education_index, FL_subset$income)

plot (FL_subset$income, FL_subset$unemployment_rate)

FLmodel <- rpart(
  wa_result ~ M_to_F + education_index + white + unemployment_rate, 
  data = FL_subset, 
  control = rpart.control(minsplit = 2))

summary(FL_subset$M_to_F)
par(xpd = NA, mar = rep(0.7, 4)) 
plot(FLmodel, compress = TRUE)
text(FLmodel, cex = 0.7, use.n = TRUE, fancy = FALSE, all = TRUE)
