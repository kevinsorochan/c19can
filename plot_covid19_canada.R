rm(list = ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(ggrepel)

covid_raw <- fread('https://health-infobase.canada.ca/src/data/covidLive/covid19.csv')
latest_covid_raw <- write.csv("latest_covid_data_canada.csv", row.names = F)

#head(covid_raw)
#str(covid_raw)

west_can <- c("British Columbia",
              "Alberta",
              "Manitoba", 
              "Saskatchewan")

east_can <- c("Quebec",
              "Ontario")

mar_can <- c("New Brunswick",
             "Nova Scotia",
             "Prince Edward Island",
             "Newfoundland and Labrador")

nor_can <- c("Yukon", 
             "Northwest Territories",
             "Nunavut")

covid_1 <- covid_raw %>%
  dplyr::mutate(., Date = as.POSIXct(date, format = "%d-%m-%Y")) %>%
  dplyr::mutate(., day = format(Date, format = '%d')) %>%
  dplyr::mutate(., month = format(Date, format = '%m')) %>%
  dplyr::mutate(., year = format(Date, format = '%Y')) %>%
  dplyr::mutate(., doy = yday(Date)) %>%
  dplyr::mutate(., seconds = as.numeric(Date)) %>%
  dplyr::filter(., !(prname %in% c("Repatriated Travellers", "Repatriated travellers", "Canada"))) %>%
  dplyr::mutate(., region = ifelse(prname %in% west_can, 'West', 'no_region')) %>%
  dplyr::mutate(., region = ifelse(prname %in% east_can, 'East', region)) %>%
  dplyr::mutate(., region = ifelse(prname %in% mar_can, 'Maritimes', region)) %>%
  dplyr::mutate(., region = ifelse(prname %in% nor_can, 'North', region)) 

case_thresh <- 1
covid_cases_cnf <- covid_1 %>%
  dplyr::filter(., numconf >= case_thresh) %>%
  dplyr::group_by(., region, prname) %>%
  dplyr::summarise(., doy0_case = min(doy)) %>%
  dplyr::full_join(., covid_1) %>%
  dplyr::mutate(., new_day_case = doy-doy0_case) %>%
  dplyr::mutate(., numconf = numconf - case_thresh) %>%
  dplyr::ungroup(.) %>%
  dplyr::mutate(., region = factor(region, levels = c("West", "East", "Maritimes", "North")))

death_thresh <- 1
covid_death <- covid_1 %>%
  dplyr::filter(numdeaths > 0) %>%
  dplyr::group_by(., region, prname) %>%
  dplyr::summarise(., doy0_death = min(doy)) %>%
  dplyr::full_join(., covid_cases_cnf) %>%
  dplyr::mutate(., new_day_death = doy-doy0_death) %>%
  dplyr::mutate(., numdeaths = numdeaths - death_thresh) %>%
  dplyr::ungroup(.) %>%
  dplyr::mutate(., region = factor(region, levels = c("West", "East", "Maritimes", "North")))

#Plot raw case data with minimum of 10 cases

covid_case <- covid_1 %>%
  dplyr::filter(., numconf >0)

latest_time <- max(covid_1$seconds)

latest_date_plus1day <- max(covid_1$Date) + (60*60*24*3)

earliest_date_case <- min(covid_case$Date)

covid_case_latest <- covid_case %>%
  dplyr::filter(., seconds == latest_time)

p_cases <- ggplot() +
  geom_point(data = covid_case, aes(x = Date, y = numconf, group = prname), alpha = 0.2) +
  geom_line(data = covid_case, aes(x = Date, y = numconf, colour = prname)) +
  geom_label_repel(data = covid_case_latest, aes(x = Date, y = numconf, label = prname), size = 2, box.padding = 0.7, alpha = 0.5) +
  scale_y_continuous(trans = 'log2', limits = c(1, max(covid_case$numconf))) +
  scale_x_datetime(limits = c(earliest_date_case, latest_date_plus1day)) +
  ylab('Cumulative number of cases')

gg_cases_reg <- p_cases +
  scale_y_continuous(trans = 'log2') +
  facet_wrap(~region, scales = 'free')

##Get similar plot for deaths

covid_death <- covid_1 %>%
  dplyr::filter(numdeaths > 0)

covid_death_latest <- covid_death %>%
  dplyr::filter(., seconds == latest_time)

latest_date_plus1day_death <- max(covid_death$Date) + (60*60*24*3)

earliest_date_death <- min(covid_death$Date)

p_deaths <- ggplot() +
  geom_point(data = covid_death, aes(x = Date, y = numdeaths, group = prname), alpha = 0.2) +
  geom_line(data = covid_death, aes(x = Date, y = numdeaths, colour = prname)) +
  geom_label_repel(data = covid_death_latest, aes(x = Date, y = numdeaths, label = prname), size = 2, box.padding = 0.7, alpha = 0.5) +
  scale_y_continuous(trans = 'log2', limits = c(1, max(covid_death$numdeaths))) +
  scale_x_datetime(limits = c(earliest_date_death, latest_date_plus1day_death)) +
  ylab('Cumulative number of cases')


##Make plots since first case

case_label <- covid_cases_cnf %>%
  dplyr::group_by(., region, prname) %>%
  dplyr::summarise(., label_day = max(new_day_case),
                   label_case = max(numconf)) %>%
  dplyr::mutate(., label_day = ifelse(is.na(label_day), -100, label_day))

days_case <- seq(from = 0, to = max(case_label$label_day), by = 0.1)
db1 <- 2^(days_case)
db2<- 2^((days_case)/2)
db3 <- 2^((days_case)/4)

db_case <- data.frame(days_case, db1, db2, db3)

p_new_cases <- ggplot() +
  geom_line(data = covid_cases_cnf, aes(x = new_day_case, numconf, colour = prname)) +
  geom_point(data = covid_cases_cnf, aes(x = new_day_case, y = numconf, group = prname), alpha = 0.2) +
  geom_line(data = covid_cases_cnf, aes(x = new_day_case, y = numconf, colour = prname)) +
  geom_label_repel(data = case_label, aes(x = label_day, y = label_case, label = prname), size = 2, box.padding = 0.7, alpha = 0.5) +
  scale_y_continuous(trans = 'log2', limits = c(1, max(covid_case$numconf))) +
  scale_x_continuous(limits = c(-1, max(covid_cases_cnf$new_day_case))) +
  xlab(paste('Day since', case_thresh, 'cases', sep = " ")) +
  ylab('Cumulative number of cases') +
  geom_line(data = db_case, aes(x = days_case + 1, y = db1), size = 2, alpha = 0.2) +
  geom_line(data = db_case, aes(x = days_case + 1, y = db2), size = 2, alpha = 0.2) +
  geom_line(data = db_case, aes(x = days_case + 1, y = db3), size = 2, alpha = 0.2)

p_new_cases_r <- p_new_cases +
  facet_wrap(~region)

death_label <- covid_death %>%
  dplyr::group_by(., region, prname) %>%
  dplyr::summarise(., label_day = max(new_day_death),
                   label_death = max(numdeaths)) %>%
  dplyr::mutate(., label_day = ifelse(is.na(label_day), -100, label_day))

days_death <- seq(from = 0, to = max(death_label$label_day), by = 0.1)
db1 <- 2^(days_death)
db2<- 2^(days_death/2)
db3 <- 2^(days_death/4)

db_death <- data.frame(days_death, db1, db2, db3)

p_new_deaths <- ggplot() +
  geom_line(data = covid_death, aes(x = new_day_death, numdeaths, colour = prname)) +
  geom_point(data = covid_death, aes(x = new_day_death, y = numdeaths, group = prname), alpha = 0.2) +
  geom_line(data = covid_death, aes(x = new_day_death, y = numdeaths, colour = prname)) +
  geom_label_repel(data = death_label, aes(x = label_day, y = label_death, label = prname), size = 2, box.padding = 0.7, alpha = 0.5) +
  scale_y_continuous(trans = 'log2', limits = c(1, max(covid_death$numdeaths))) +
  scale_x_continuous(limits = c(-1, max(death_label$label_day) + 2)) +
  xlab('Day since 1st death') +
  ylab('Cumulative number of deaths') +
  geom_line(data = db_death, aes(x = days_death, y = db1), size = 2, alpha = 0.2) +
  geom_line(data = db_death, aes(x = days_death, y = db2), size = 2, alpha = 0.2) +
  geom_line(data = db_death, aes(x = days_death, y = db3), size = 2, alpha = 0.2)

p_new_deaths_r <- p_new_deaths +
  facet_wrap(~region)
