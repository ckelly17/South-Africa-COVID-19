library(tidyverse)
library(data.table)
library(plotly)
library(lubridate)
library(scales)
library(pracma)
library(RColorBrewer)


#setwd("/Users/conorkelly/Documents/COVID")



omcr_annc_date <- ymd("2021-11-25")

raw <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv") %>%
  as_tibble()

afr <- raw %>%
  filter(continent %in% "Africa")

s_afr <- raw %>%
  filter(location %in% "South Africa") %>%
  select(date, new_cases, new_cases_smoothed, new_deaths, new_deaths_smoothed,
         positive_rate, new_tests_smoothed) %>%
  mutate(new_cases = ifelse(is.na(new_cases), 0 , new_cases),
         temp = 0)

if(add_row == TRUE){
s_afr <- s_afr %>%
  add_row(new_cases = cases_temp,
          new_deaths = deaths_temp,
          date = date_temp,
          temp = override) %>%
  group_by(date) %>%
  filter(temp == min(temp, na.rm = TRUE)) %>%
  ungroup()
  
}

s_afr <- s_afr %>%
  mutate(new_cases_smoothed = movavg(new_cases, 7),
         new_deaths_smoothed = movavg(new_deaths, 7))

# lags
case_peak_date <- ymd("2021-07-08")
death_peak_date <- ymd("2021-07-25")
death_lag <- as.integer(death_peak_date - case_peak_date)

max_case <- s_afr$new_cases_smoothed[s_afr$date == '2021-07-08']
max_death <- s_afr$new_deaths_smoothed[s_afr$date == '2021-07-25']

# lagged cases/deaths
s_afr <- s_afr %>%
  arrange(date) %>%
  mutate(deaths_lag = lead(new_deaths_smoothed, n = death_lag),
         case_pct_pk = new_cases_smoothed / max_case,
         death_fwd_pk = deaths_lag / max_death,
         cfr = deaths_lag / new_cases_smoothed)

# pct plot
pct_pk_plot <- s_afr %>%
  filter(date >= "2021-01-01") %>%
  mutate(fill = "Cases",
         color = "Deaths (17-Day Lag)") %>%
  ggplot +
  geom_area(aes(x = date, y = case_pct_pk, fill = fill), alpha = 0.4) +
  #geom_line(aes(x = date, y = case_pct_pk), color = "coral1", size = 0.9) +
  geom_line(aes(x = date, y = death_fwd_pk, color = color), size = 0.8) +
  scale_y_continuous(labels = percent_format(accuracy = 2),
                     breaks = seq(0, 100, 0.1)) +
  theme_minimal() +
  labs(title = "COVID-19 Cases and Lagged Deaths as Percentage of Peak Values",
       caption = "Cases in orange, deaths in black. Deaths lagged 17 days to reflect lag between cases peak and deaths peak during 2021 Delta wave.") +
  xlab("") +
  ylab("Percentage of Summer 2021 (Delta) Peak") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values = c("Cases" = "coral1")) +
  scale_color_manual(values = c("Deaths (17-Day Lag)" = "black"))

# CFR
cfr_plot <- s_afr %>%
  filter(date >= "2021-01-01") %>%
  ggplot +
  geom_point(aes(x = date, y = cfr), color = "navy", shape = 1, size = 1) +
  geom_line(aes(x = date, y = cfr), color = "navy", alpha = 0.5 , size = 0.75) +
  geom_line(aes(x = date, y = cfr), 
            stat = "smooth", se = FALSE, color = "navy", alpha = 0.75) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     #expand = c(0, .13),
                     limits = c(0, NA),
                     breaks = seq(0, .13, 0.01)) +
  theme_minimal() +
  labs(title = "South Africa: Case Fatality Rate",
       subtitle = "Deaths lagged 17 days") +
  xlab("") +
  ylab("") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
cfr_plot

# pos rate

# assign ratios for axis transformation
s_afr$positive_rate_int = s_afr$positive_rate * 100
max_tests <- max(s_afr$new_tests_smoothed, na.rm = TRUE)
max_pp <- max(s_afr$positive_rate_int, na.rm = TRUE)

pp_test_ratio <- max_tests / max_pp

s_afr_pp <- s_afr %>%
  filter(date >= "2021-01-01") %>%
  mutate(tests_axis2 = new_tests_smoothed / pp_test_ratio / 100)

pos_rate_plot <- s_afr_pp %>%

  ggplot +
  geom_point(aes(x = date, y = positive_rate), 
             color = "forestgreen", size = 1, shape = 1, alpha = 0.75) +
  geom_line(aes(x = date, y = positive_rate), 
             color = "forestgreen", size = 0.2, alpha = 0.5) +
  geom_area(aes(x = date, y = tests_axis2, label = new_tests_smoothed), 
            fill = "forestgreen", alpha = 0.15) +
  scale_y_continuous(labels = percent_format(accuracy = 2),
                     sec.axis = sec_axis(~.*pp_test_ratio, name = "Daily Tests (7-Day Avg)")) +
  theme_minimal() +
  labs(title = "Positivity Rate (Lines) and 7-Day Avg Daily Testing (Area)") +
  xlab("") +
  ylab("Positive Rate (7-Day)") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

pos_rate_plot

# ratios
sa_ratio <- s_afr %>%
  mutate(case_ratio_smoothed = new_cases_smoothed / lag(new_cases_smoothed, n = 7)) %>%
  mutate(case_ratio = new_cases / lag(new_cases, n = 7)) %>%
  mutate(case_decline = ifelse(case_ratio > 1, 'Growth', 'Decline'),
         case_decline = factor(case_decline, levels = c('Growth', 'Decline'))) %>%
  mutate(case_max_pct = new_cases_smoothed / max(new_cases_smoothed, na.rm = TRUE)) %>%
  mutate(one = 1)

abv_one <- max(sa_ratio$case_ratio[which(sa_ratio$case_ratio < Inf)], na.rm = TRUE)
  
sa_ratio <- sa_ratio %>%
  mutate(abv_one = 8)

# log cases
sa_ratio_log <-
  sa_ratio %>%
  filter(date >= "2021-01-01") %>%
  ggplot() +
  geom_point(aes(x = date, y = new_cases_smoothed), 
             color = "coral1", shape = 1, alpha = 0.75, size = 1) +
  geom_line(aes(x = date, y = new_cases_smoothed), 
             color = "coral1", alpha = 0.5, size = 0.25) +
  labs(title = "Log Growth of COVID-19 Cases in South Africa",
       x = '') +
  scale_y_continuous(trans='log2',
                     label = comma,
                     breaks=c(0, 500, 1000, 2000, 5000, 10000, 20000, 40000)) +
  xlab("") +
  ylab("") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplotly(sa_ratio_log)

# ratios
sa_ratio_plot <-

  sa_ratio %>%
  filter(date >= "2021-01-01") %>%
  ggplot() +
  geom_area(aes(x = date, y = abv_one), fill = '#f2947f', alpha = 0.2) +
  geom_area(aes(x = date, y = one), fill = '#99e5e7', alpha = 0.5) + 
  geom_point(aes(x = date, y = case_ratio, color = case_decline), 
             alpha = 0.5, size = 0.7, show.legend = FALSE) +
  geom_line(aes(x = date, y = case_ratio_smoothed), size = 0.8) +
  geom_hline(yintercept = 1, linetype = 'dashed', size = 0.7, alpha = 0.6) +
  theme(legend.position='none') +
  ggtitle("Growth Multiplier of COVID-19 Cases Week-Over-Week") +
  xlab("") +
  ylab("") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  scale_y_continuous(breaks = c(0, 0.01, 0.1, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5, 6, 7, 8),
                     #labels = percent_format(accuracy = 1),
                     trans = "log2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

sa_ratio_plot
ggplotly(sa_ratio_plot)

# cases/average
sa_case_plot <-
  
  sa_ratio %>%
  filter(date >= "2021-01-01") %>%
  ggplot() +
  geom_col(aes(x = date, y = new_cases), alpha = 0.4, fill = "coral1") +
  geom_line(aes(x = date, y = new_cases_smoothed), color = "coral1", size = 1) +

  ggtitle("COVID-19 Cases in South Africa") +
  xlab("") +
  ylab("") +
  scale_y_continuous(label=comma, breaks = seq(0, 100000, 5000)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

sa_case_plot
ggplotly(sa_case_plot)

# deaths/average
sa_deaths_plot <-
  
  sa_ratio %>%
  filter(date >= "2021-01-01") %>%
  ggplot() +
  geom_col(aes(x = date, y = new_deaths), alpha = 0.4) +
  geom_line(aes(x = date, y = new_deaths_smoothed), size = 1) +
  ylim(0,600) +
  ggtitle("COVID-19 Deaths in South Africa") +
  xlab("") +
  ylab("") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  scale_y_continuous(label = comma, breaks = seq(0, 1000, 100)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

sa_deaths_plot
ggplotly(sa_deaths_plot)

# deaths/log
sa_deaths_log <-
  
  sa_ratio %>%
  filter(date >= "2021-01-01") %>%
  ggplot() +
  geom_point(aes(x = date, y = new_deaths_smoothed), size = 1,
            shape = 1, alpha = 0.75) +
  geom_line(aes(x = date, y = new_deaths_smoothed), size = 0.2,
             alpha = 0.75) +
  scale_y_continuous(trans='log2', 
                     breaks=c(0, 25, 50, 100, 200, 400, 800, 1600))+
  ggtitle("Log Growth of COVID-19 Deaths in South Africa") +
  xlab("") +
  ylab("") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

sa_deaths_log
ggplotly(sa_deaths_log)

### Hospitalization

url <- "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_raw_hospitalization.csv"
hosp_raw <- fread(url) %>%
  as_tibble()

vars <- c("CurrentlyAdmitted", "CurrentlyinICU", "CurrentlyVentilated", "FacilitiesReporting")

hosp <- hosp_raw %>%
  filter(variable %in% vars,
         Owner %in% "Total",
         Province %in% c("Gauteng", "Total")) %>%
  mutate(Date = ymd(Date),
         value = as.numeric(value)) %>%
  group_by(variable, Owner, Province) %>%
  mutate(seven_day_avg = movavg(value, 7)) %>%
  ungroup()

sa_hosp <- hosp %>%
  filter(Date >= ymd("2021-01-01"),
         Province %in% "Total") %>%
  
  ggplot()+
  geom_area(aes(x = Date, y = value), alpha = 0.5, fill = "#AF460A") +
  geom_line(aes(x = Date, y = seven_day_avg), size = 0.8, color = "#AF460A") +
  geom_vline(xintercept = as.numeric(omcr_annc_date), linetype = "dashed") +
  facet_wrap(~variable, scales = "free") + 
  ggtitle("South Africa Hospitalization Data") +
  xlab("") +
  ylab("") +
  scale_y_continuous(label=comma) +
  theme_minimal() +
  theme(legend.position='none') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")


ggplotly(sa_hosp)

gt_hosp <- hosp %>%
  filter(Date >= ymd("2021-01-01"),
         Province %in% "Gauteng") %>%
  
  ggplot()+
  geom_area(aes(x = Date, y = value), alpha = 0.5, fill = "#B45C7A") +
  geom_line(aes(x = Date, y = seven_day_avg), size = 0.8, color = "#B45C7A") +
  geom_vline(xintercept = as.numeric(omcr_annc_date), linetype = "dashed") +
  facet_wrap(~variable, scales = "free") + 
  ggtitle("Gauteng Hospitalization Data") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  scale_y_continuous(label=comma) +
  theme(legend.position='none') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
  # scale_fill_brewer(palette = "#479257") +
  # scale_color_brewer(palette = "#479257")

ggplotly(gt_hosp)

## Hosp/Case Comparison
cases_append <- s_afr %>%
  select(Date = date, seven_day_avg = new_cases_smoothed) %>%
  mutate(variable = "Cases")

comb <- hosp %>%
  filter(Province %in% "Total",
         !variable %in% "FacilitiesReporting") %>%
  
  mutate(variable = ifelse(variable %in% "CurrentlyAdmitted", "Total Admissions", variable)) %>%
  mutate(variable = ifelse(variable %in% "CurrentlyAdmitted", "Total Admissions", variable)) %>%
  mutate(variable = ifelse(variable %in% "CurrentlyinICU", "ICU", variable)) %>%
  mutate(variable = ifelse(variable %in% "CurrentlyVentilated", "Ventilated", variable)) %>%
  mutate(variable = ifelse(variable %in% "NewCases", "New Cases", variable)) %>%
  
  select(Date, variable, seven_day_avg) %>%
  bind_rows(cases_append) %>%
  arrange(Date) %>%
  group_by(variable) %>%
  mutate(pct_val = seven_day_avg / max(seven_day_avg, na.rm = TRUE)) %>%
  ungroup()

## percent chart
pct_hosp_plot <- comb %>%
  filter(Date >= ymd("2021-01-01")) %>%
  mutate(Type = ifelse(variable %in% "Cases", "Cases", "Hosp"),
         Type = factor(Type, levels = c("Hosp", "Cases")),
         
         variable = factor(variable, levels = c("Cases", "Total Admissions",
                                                "ICU", "Ventilated")),
         
         area = ifelse(variable %in% "Cases", pct_val, NA),
         pct_val = ifelse(variable %in% "Cases", NA, pct_val)) %>%
  
  ggplot() +
  geom_area(aes(x = Date, y = area), fill = 'lightgray', alpha = 0.7) +
  geom_line(aes(x = Date, y = pct_val, color = variable), 
            size = 0.8) +
  geom_vline(xintercept = as.numeric(omcr_annc_date), linetype = "dashed") +
  
  scale_y_continuous(labels = percent_format(accuracy = 2),
                     breaks = seq(0, 100, 0.1)) +
  ylab("Percentage of Peak Value") +
  xlab("") +
  ggtitle("South Africa Hospitalization/Cases as Percentage of Peak Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  labs(variable = "" ) +
  scale_color_manual(values = c("Cases" = "lightgray",
                                "Total Admissions" = "#6F8FAF", # denim blue
                                "ICU" = "#6495ED", # cornflower (medium)
                                "Ventilated" = "#00008B")) # dark blue
  
pct_hosp_plot
ggplotly(pct_hosp_plot)

## ICU percentage
icu <- hosp %>%
  filter(variable %in% c("CurrentlyAdmitted", "CurrentlyVentilated", "CurrentlyinICU")) %>%
  group_by(Province, Owner, Date)%>%
  mutate(admitted = max(seven_day_avg, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct_of_admitted = seven_day_avg / admitted)

icu_pct_plot <- icu %>%
  filter(Date >= ymd("2021-01-01")) %>%
  filter(variable %in% c("CurrentlyVentilated", "CurrentlyinICU")) %>%
  mutate(Province = factor(Province, levels = c("Total", "Gauteng"))) %>%
  ggplot() +
  geom_line(aes(x = Date, y = pct_of_admitted, color = variable), size = 0.8) +
  geom_vline(xintercept = as.numeric(omcr_annc_date), linetype = "dashed") +
  facet_wrap(~Province) +
  
  ylim(0, .3) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, .3),
                     breaks = seq(0, .3, 0.05)) +
  ylab("") +
  xlab("") +
  
  ggtitle("ICU and Ventilated as Percentage of Admissions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  labs(variable = "" ) +
  scale_color_manual(values = c("CurrentlyinICU" = "#6495ED",
                                "CurrentlyVentilated" = "#00008B")) +
  theme(strip.text.x = element_text(size = 11, face = 'bold'))
icu_pct_plot
ggplotly(icu_pct_plot)

##########################################
## gauteng
gauteng <- read_csv("/Users/conorkelly/Documents/COVID/gauteng_cases.csv") %>%
  mutate(new_cases_smoothed = movavg(new_cases, 7),
         ratio_smooth = new_cases_smoothed / lag(new_cases_smoothed, 7),
         ratio = new_cases / lag(new_cases, 7),
         date = mdy(date))

### Gauteng cases base
gt_cases_plot <- 
  gauteng %>%
  filter(date >= "2021-01-01") %>%
  ggplot() +
  geom_col(aes(x = date, y = new_cases), 
             fill = "coral1", alpha = 0.4, size = 1) +
  geom_line(aes(x = date, y = new_cases_smoothed), 
            color = "coral1", alpha = 0.5, size = 0.8) +
  xlab("") +
  ylab("") +
  scale_x_date(date_breaks = "1 week") +
  labs(title = "COVID-19 Cases in Gauteng") +
  scale_y_continuous(label = comma) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## Gauteng log
gt_log_cases <- 
  gauteng %>%
  filter(date >= "2021-01-01") %>%
  ggplot() +
  geom_point(aes(x = date, y = new_cases_smoothed), 
           color = "coral1", shape = 1, alpha = 0.75, size = 1) +
  geom_line(aes(x = date, y = new_cases_smoothed), 
            color = "coral1", alpha = 0.5, size = 0.2) +
  xlab("") +
  ylab("") +
  scale_x_date(date_breaks = "1 week") +
  labs(title = "COVID-19 Cases in Gauteng (Log Scale)") +
  scale_y_continuous(label = comma) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(trans='log2',
                     label = comma,
                     breaks=c(0, 25,  50, 100, 250, 500, 1000, 2000, 5000, 10000, 20000, 40000))

## Gauteng ratio
gt_ratio <-
  gauteng %>%
  mutate(one = 1,
         abv_one = 8) %>%
  filter(date >= "2021-01-01") %>%
  ggplot() +
  geom_area(aes(x = date, y = abv_one), fill = '#f2947f', alpha = 0.2) +
  geom_area(aes(x = date, y = one), fill = '#99e5e7', alpha = 0.5) + 
  geom_point(aes(x = date, y = ratio), 
             color = "black", shape = 1, alpha = 0.75, size = 1) +
  geom_line(aes(x = date, y = ratio_smooth), 
            color = "black", alpha = 0.9, size = 0.75) +
  xlab("") +
  ylab("") +
  scale_x_date(date_breaks = "1 week") +
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 3, 4, 5, 6, 7, 8),
                     labels = percent_format(accuracy = 1)) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Gauteng Growth Multiplier")

# show
ggplotly(gt_cases_plot)
ggplotly(gt_log_cases)
ggplotly(gt_ratio)

           
           






    