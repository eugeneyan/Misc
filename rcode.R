# load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

#####################################################################
# import and clean data
#####################################################################
# import data
sup <- read.csv('support3.csv')
sup$assignee_id <- as.factor(sup$assignee_id)
tran <- read.csv('transactions3.csv')
tran$X <- NULL  # drop blank column that arose due to extra ','

# clean up dates and create additional features
tran$date <- as.POSIXct(strptime(x = tran$transactions, format = '%d/%m/%Y'))  # fix tran date
tran$yearmon <- strftime(tran$date, format = '%Y/%m')

# add date to tran in date format
tran$date_fmt <- as.Date.POSIXct(tran$date) + 1  # + 1 due to timezone difference

sup$created <- as.POSIXct(strptime(x = sup$created_time, format = '%d/%m/%Y %H:%M'))
sup$created_date <- as.Date.POSIXct(sup$created)
sup$created_day <- weekdays(sup$created_date)
sup$solved <- as.POSIXct(strptime(x = sup$solved_time, format = '%d/%m/%Y %H:%M'))
sup$created_month <- month(sup$created)
sup$yearmon <- strftime(sup$created, format = '%Y/%m')

# remove rows with missing assignee_id and solved_time and select french queries only
sup <- sup %>%
  na.omit(sup) %>%  # remove missing cases
  filter(requester_language == 'french')  %>%  # select french queries only
  filter(assignee_id != '655155207')  # remove agent with all queries taking about 123 hrs

# create additional features
sup <- sup %>%
  mutate(time_min = as.integer((sup$solved - sup$created) / 60)) %>%  # time taken in minutes
  mutate(time_hr = time_min/60)  %>%  # time taken in hrs
  mutate(time_day = time_hr/24)  # time taken in days

# create new dataset that excludes outliers 
sup <- sup %>%
  filter(time_hr < (mean(time_hr) + 3 * sd(time_hr)))

### Exploratory code
# are there outliers for time taken?
ggplot(data = sup, aes(x = time_hr)) + geom_histogram(binwidth = 24)  
# time taken in hrs ranges from 0 to 4321. how shall we deal with the outliers?

# using mean + 3sd to exclude outliers, how many outliers are there?
nrow(sup[sup$time_hr > mean(sup$time_hr) + 3*sd(sup$time_hr), ])  
# there are 662 outlier (approx 1.4% of the data)

#####################################################################
### examining transaction data
#####################################################################
# split transaction data into past and present
tran_past <- tran %>%
  filter(date < ymd(20150531))

tran_fut <- tran %>%
  filter(date >= ymd(20150601))

# mean booking and transaction / month
tran_past_month <- tran_past %>%
  group_by(month) %>%
  summarise(bookings = sum(bookings), checkins = sum(checkins))

# bookings/checkins across time
View(tran_past)
tran$yearmon <- strftime(tran$date, format = '%Y/%m')
tran_past$yearmon <- strftime(tran_past$date, format = '%Y/%m')
tran_fut$yearmon <- strftime(tran_fut$date, format = '%Y/%m')

tran_yearmon <- tran %>%
  group_by(yearmon) %>%
  summarise(bookings = sum(bookings), checkins = sum(checkins))

tran_past_yearmon <- tran_past %>%
  group_by(yearmon) %>%
  summarise(bookings = sum(bookings), checkins = sum(checkins))

tran_fut_yearmon <- tran_fut %>%
  group_by(yearmon) %>%
  summarise(bookings = sum(bookings), checkins = sum(checkins))

View(tran_past_yearmon)
View(tran_fut_yearmon)

#####################################################################
# number of french queries/queryhr over months/day/day week
# does it fluncuate with time? Is there a seasonal trend?  
#####################################################################
# find proportion of french queries across the year
# examine query numbers by day
query_per_day <- sup %>%
  group_by(created_date) %>%
  summarise(queries = n())
ggplot(data = query_per_day, aes(x = created_date, y = queries)) + geom_bar(stat = 'identity')

# queries by day of the week
queries_day_week <- sup %>%
  group_by(created_day) %>%
  summarise(queries = n())

ggplot(data = queries_day_week, aes(x = created_day, y = queries)) + geom_bar(stat = 'identity')  # mon - wed has highest volume,

# add month of query data
sup$created_month <- month(sup$created)

# queries by month
queries_month <- sup %>%
  group_by(yearmon) %>%
  summarise(queries = n())

ggplot(data = queries_month, aes(x = yearmon, y = queries)) + 
  geom_bar(stat = 'identity')  # peaks in Mar, then again in Aug

# examine query hours day
queryhr_per_day <- sup %>%
  group_by(created_date) %>%
  summarise(queryhr = sum(time_hr))
ggplot(data = queryhr_per_day, aes(x = created_date, y = queryhr)) + geom_bar(stat = 'identity')

queryhr_day_week <- sup %>%
  group_by(created_day) %>%
  summarise(queryhr = sum(time_hr))
ggplot(data = queryhr_day_week, aes(x = created_day, y = queryhr)) + geom_bar(stat = 'identity')

queryhr_month <- sup %>%
  group_by(yearmon) %>%
  summarise(queries = sum(time_hr))

ggplot(data = queryhr_month, aes(x = yearmon, y = queries)) + 
  geom_bar(stat = 'identity') # peaks in Mar, then again in Aug

queryhr_avg_month <- sup %>%
  group_by(yearmon) %>%
  summarise(time_taken = mean(time_hr))

# strong evidence that time taken for query differs with month
ggplot(data = queryhr_avg_month, aes(x = yearmon, y = time_taken)) +  geom_bar(stat = 'identity') + labs(title = 'Average time per query (hr) across months', y = 'Time per query (hr)', x = 'Month')  # peaks in Mar, then again in Aug

#####################################################################
# how many queries/query hours per agent each month
#####################################################################
# query hrs per agent per month
queryhr_agent_month <- sup %>%
  group_by(assignee_id, yearmon) %>%
  summarise(queryhr = sum(time_hr))

summary(queryhr_agent_month$queryhr)
View(queryhr_agent_month[queryhr_agent_month$queryhr > 500, ])
View(queryhr_agent_month[queryhr_agent_month$assignee_id == '11109977', ])

# queries per agent per month
query_agent_month <- sup %>%
  group_by(assignee_id, yearmon) %>%
  summarise(query = n())

summary(query_agent_month)

#####################################################################
# create simple lm
#####################################################################
# add date to tran in date format
tran$date_fmt <- as.Date.POSIXct(tran$date) + 1

# join on sup.created_day = tran.date_fmt
sup_tran <- left_join(queryhr_per_day, tran, by = c('created_date' = 'date_fmt'))

# remove Mar 2014 data
sup_tran <- sup_tran %>%
  filter(date > ymd(20140331))

# create linear model with query hr
lm.fit <- lm(queryhr ~ bookings + checkins, data = sup_tran)
summary(lm.fit)
# R-sq of 0.1

# join on sup.created_day = tran.date_fmt
sup_tran2 <- left_join(query_per_day, tran, by = c('created_date' = 'date_fmt'))

# remove Mar 2014 data
sup_tran2 <- sup_tran2 %>%
  filter(date > ymd(20140331))

# create linear model with query
lm.fit2 <- lm(query ~  bookings + checkins, data = sup_tran2)
summary(lm.fit2)
# R-sq 0.24

# Select forecasted bookings and checkin from Jun - Dec 2015
tran_pred <- tran %>%
  filter(date >= ymd(20150601))

# create predictions for Jun - Dec 2015
queries <- predict(lm.fit2, newdata = tran_pred)
tran_pred <- cbind(tran_pred, queries)

# create month variable
tran_pred$yearmon <- strftime(tran_pred$date, format = '%Y/%m')

# number of queryhrs expected per month
tran_pred_month <- tran_pred %>%
  group_by(yearmon) %>%
  summarise(queries = sum(queries))

ggplot(data = tran_pred_month, aes(x = yearmon, y = queries)) + geom_bar(stat = 'identity')
View(tran_pred_month)

#####################################################################
### Plots for slides
#####################################################################
# past and predicted bookings/check-ins
ggplot(data = tran_yearmon, aes(x = yearmon)) + 
  # geom_line(aes(y = 0, alpha = 1)) + 
  geom_line(data = tran_past_yearmon, aes(x = yearmon, y = bookings, group = 1, color = 'Past Bookings'), stat = 'identity', size = 1) + 
  geom_line(data = tran_past_yearmon, aes(x = yearmon, y = checkins, group = 1, color = 'Past Check-ins'), stat = 'identity', size = 1) + 
  geom_line(data = tran_fut_yearmon, aes(x = yearmon, y = bookings, group = 1, color = 'Predicted Bookings'), stat = 'identity', linetype = 'dashed', size = 1) + 
  geom_line(data = tran_fut_yearmon, aes(x = yearmon, y = checkins, group = 1, color = 'Predicted Check-ins'), stat = 'identity', linetype = 'dashed', size = 1) + 
  labs(color = 'Legend', title = 'Past and Predicted Bookings/Check-ins') + 
  labs(x = 'Year-Month') + 
  labs(y = 'No. of Bookings/Check-ins') +
  scale_y_continuous(breaks = c(100000, 250000, 500000, 750000, 1000000, 1250000, 1500000)) +
  scale_colour_manual(name = 'Legend', values = c('Past Bookings' = '#fc6c85', 'Predicted Bookings' = '#fc6c85', 'Past Check-ins' = '#37c3e0', 'Predicted Check-ins' = '#37c3e0')) +
  scale_linetype_manual(name = 'Legend', values = c('Past Bookings' = 'solid', 'Predicted Bookings' = 'dashed', 'Past Check-ins' = 'solid', 'Predicted Check-ins' = 'dashed')) +
  theme(legend.position = 'bottom') +
  theme(plot.title = element_text(lineheight=1, face= 'bold', size = 22)) +
  theme(legend.text = element_text(lineheight=1, size = 15)) + 
  theme(legend.title = element_text(lineheight=1, size = 15, face = 'bold')) + 
  theme(axis.title = element_text(lineheight=1, size = 15, face = 'bold')) + 
  theme(axis.text = element_text(colour = 'black', size = 10))

# Time spent on tickets/month
ggplot(data = queryhr_month_full, aes(x = created_month, y = queries)) + 
  geom_bar(stat = 'identity', fill = '#fc6c85') + 
  scale_x_continuous(breaks = 1:12, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) + 
  scale_y_continuous(breaks = c(0, 1000000, 2000000, 3000000), labels = c('0', '1,000,000', '2,000,000', '3,000,000')) + 
  labs(title = 'Hours spent on tickets per Month (Apr 2014 - Mar 2015') + 
  labs(x = 'Month') + 
  labs(y = 'Hours spent on tickets') +
  theme(plot.title = element_text(lineheight=1, face= 'bold', size = 22)) +
  theme(axis.title = element_text(lineheight=1, size = 15, face = 'bold'))

# no of tickets/month
ggplot(data = query_month_full, aes(x = yearmon, y = queries)) + 
  geom_bar(stat = 'identity', fill = '#ff5a5e') + 
  geom_text(aes(x = yearmon, label = queries), size = 5, vjust = -0.5) + 
  scale_y_continuous(breaks = c(0, 10000, 20000, 30000, 40000, 50000), labels = c('0', '10,000', '20,000', '30,000', '40,000', '50,000')) + 
  labs(title = 'No. of tickets per Month (Apr 2014 - Mar 2015)') + 
  labs(x = 'Year-Month') + 
  labs(y = 'No. of tickets') +
  theme(plot.title = element_text(lineheight=1, face= 'bold', size = 22)) +
  theme(axis.title = element_text(lineheight=1, size = 15, face = 'bold')) +
  theme(axis.text = element_text(colour = 'black', size = 10))

# Time spent on tickets/month (french)
ggplot(data = queryhr_month, aes(x = yearmon, y = queries)) + 
  geom_bar(stat = 'identity', fill = '#ff5a5e') + 
  labs(title = 'Time spent on Tickets in French per Month (Apr 2014 - Mar 2015)') + 
  geom_text(aes(x = yearmon, label = round(queries)), size = 5, vjust = -0.5) + 
  scale_y_continuous(breaks = c(0, 100000, 200000, 300000, 400000, 500000), labels = c('0', '100,000', '200,000', '300,000', '400,000', '500,000'), limits = c(0, 310000)) + 
  labs(x = 'Year-Month') + 
  labs(y = 'Time spent on tickets (hrs)') +
  theme(plot.title = element_text(lineheight=1, face= 'bold', size = 22)) +
  theme(axis.title = element_text(lineheight=1, size = 15, face = 'bold')) +
  theme(axis.text = element_text(colour = 'black', size = 10))

# Average time spent on tickets/month (french)
ggplot(data = queryhr_avg_month, aes(x = yearmon, y = time_taken)) + 
  geom_bar(stat = 'identity', fill = '#ff5a5e') + 
  ylim(0, 75) +
  geom_text(aes(x = yearmon, label = round(time_taken, 1)), size = 5, vjust = -0.5) + 
  labs(title = 'Average time spent per French Ticket per Month (Apr 2014 - Mar 2015)') + 
  labs(x = 'Year-Month') + 
  labs(y = 'Average time spent on each ticket') +
  theme(plot.title = element_text(lineheight=1, face= 'bold', size = 17)) +
  theme(axis.title = element_text(lineheight=1, size = 15, face = 'bold')) +
  theme(axis.text = element_text(colour = 'black', size = 10))

# No of queries/month (french)
ggplot(data = queries_month, aes(x = yearmon, y = queries)) + 
  geom_bar(stat = 'identity', fill = '#ff5a5e') + 
  ylim(0, 5100) + 
  geom_text(aes(x = yearmon, label = queries), size = 5, vjust = -0.5) + 
  labs(title = 'No of Tickets in French per Month (Apr 2014 - Mar 2015)') + 
  labs(x = 'Year-Month') + 
  labs(y = 'No of tickets') +
  theme(plot.title = element_text(lineheight=1, face= 'bold', size = 17)) +
  theme(axis.title = element_text(lineheight=1, size = 15, face = 'bold')) +
  theme(axis.text = element_text(colour = 'black', size = 10))

# Plot of bookings against tickets with lm fit2
ggplot(data = sup_tran2, aes(x = bookings, y = query)) + 
  geom_point(colour = '#ff5a5e') + 
  ylim(0, 250) +
  geom_abline(intercept = 5.25e+01, slope = 3.55e-03, size = 1) +
  labs(title = 'No. of French Tickets vs No. of Bookings (daily)') + 
  labs(x = 'No. of Bookings (daily)') + 
  labs(y = 'No. of French tickets (daily)') +
  theme(plot.title = element_text(lineheight=1, face= 'bold', size = 22)) +
  theme(axis.title = element_text(lineheight=1, size = 15, face = 'bold'))

# Predicted No. of queries
ggplot(data = tran_pred_month, aes(x = yearmon, y = queries)) + 
  geom_bar(stat = 'identity', fill = '#ff5a5e') +
  geom_text(aes(x = yearmon, label = round(queries)), size = 5, vjust = -0.5) + 
  labs(title = 'Forecasted No. of French tickets per month (Jun 2015 - Dec 2015)') + 
  labs(x = 'Year-Month (Jun 2015 - Dec 2015)') + 
  labs(y = 'Forecasted No. of tickets') +
  theme(plot.title = element_text(lineheight=1, face= 'bold', size = 17)) +
  theme(axis.title = element_text(lineheight=1, size = 15, face = 'bold')) +
  theme(axis.text = element_text(colour = 'black', size = 10))

# no of agents needed with 6 tickets / month and 34 tickets / month
View(tran_pred_month)
tran_pred_month <- tran_pred_month %>%
  mutate(agent1 = queries/6) %>%
  mutate(agent2 = queries/34)