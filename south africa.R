library(tidyverse)
library(data.table)
library(plotly)
library(lubridate)
library(scales)
library(pracma)
library(RColorBrewer)

omcr_annc_date <- ymd("2021-11-25")
start <- lubridate::now()

raw <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv") %>%
  as_tibble()

afr <- raw %>%
  filter(continent %in% "Africa")

s_afr <- raw %>%
  filter(location %in% "South Africa") %>%
  mutate(Region = "South Africa") %>%
  select(date, Region, new_cases, new_cases_smoothed, new_deaths, new_deaths_smoothed,
         positive_rate, new_tests_smoothed) %>%
  mutate(new_cases = ifelse(is.na(new_cases), 0 , new_cases),
         date = ymd(date),
         temp = 0)

if(add_row == TRUE){
s_afr <- s_afr %>%
  add_row(new_cases = cases_temp,
          new_deaths = deaths_temp,
          date = date_temp,
          temp = override,
          Region = region_temp) %>%
  group_by(date) %>%
  filter(temp == min(temp, na.rm = TRUE)) %>%
  ungroup()
  
}

s_afr <- s_afr %>%
  mutate(new_cases_smoothed = movavg(new_cases, 7),
         new_deaths_smoothed = movavg(new_deaths, 7))

# gauteng
gt_cases_url <- "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv"
gt_deaths_url <- "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_deaths.csv"
gt_cases <- fread(gt_cases_url) %>%
  as_tibble() %>%
  select(date = YYYYMMDD,
         total_cases = GP) %>%
  mutate(Region = "Gauteng Province",
         new_cases = total_cases - lag(total_cases),
         new_cases_smoothed = movavg(new_cases, 7),
         date = ymd(date))

gt_deaths <- fread(gt_deaths_url) %>%
  as_tibble() %>%
  select(date = YYYYMMDD,
         total_deaths = GP) %>%
  mutate(Region = "Gauteng Province",
         new_deaths = total_deaths - lag(total_deaths),
         new_deaths_smoothed = movavg(new_deaths, 7),
         date = ymd(date))

gt <- gt_cases %>%
  left_join(gt_deaths, by = c("date", "Region"))

s_afr <- s_afr %>%
  bind_rows(gt) %>%
  mutate(Region = factor(Region, levels = c( "South Africa", "Gauteng Province")))

#tests
test_url <- "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_timeline_testing.csv"
test_sa_raw <- fread(test_url) %>%
  as_tibble()

owid_tests <- s_afr %>%
  filter(!is.na(new_tests_smoothed))

test_sa <- test_sa_raw %>%
  select(date = YYYYMMDD,
         cumulative_tests) %>%
  mutate(new_tests = cumulative_tests - lag(cumulative_tests),
         date = ymd(date),
         new_tests_smoothed = movavg(new_tests, 7),
         Region = "South Africa") %>%
  anti_join(owid_tests, "date")

s_afr <- s_afr %>%
  bind_rows(test_sa) %>%
  group_by(date, Region) %>%
  mutate(new_tests_smoothed = max(new_tests_smoothed, na.rm = TRUE)) %>%
  filter(!is.na(new_cases)) %>%
  ungroup() %>%
  mutate(positive_rate = new_cases_smoothed / new_tests_smoothed)

# lags
case_peak_date <- ymd("2021-07-08")
death_peak_date <- ymd("2021-07-25")
death_lag <- as.integer(death_peak_date - case_peak_date)

max_case_sa <- s_afr$new_cases_smoothed[s_afr$date == '2021-07-08' & s_afr$Region %in% "South Africa"]
max_death_sa <- s_afr$new_deaths_smoothed[s_afr$date == '2021-07-25'  & s_afr$Region %in% "South Africa"]

max_case_gt <- s_afr$new_cases_smoothed[s_afr$date == '2021-07-03' & s_afr$Region %in% "Gauteng Province"]
max_death_gt <- s_afr$new_deaths_smoothed[s_afr$date == '2021-07-10' & s_afr$Region %in% "Gauteng Province"]

# lagged cases/deaths
s_afr <- s_afr %>%
  mutate(Region = factor(Region, levels = c( "South Africa", "Gauteng Province"))) %>%
  arrange(date) %>%
  group_by(Region) %>%
  mutate(max_case = ifelse(Region %in% "South Africa", max_case_sa, max_case_gt),
         max_death = ifelse(Region %in% "South Africa", max_death_sa, max_death_gt)) %>%
  mutate(deaths_lag = lead(new_deaths_smoothed, n = death_lag),
         case_pct_pk = new_cases_smoothed / max_case,
         death_pct_pk = deaths_lag / max_death,
         cfr = deaths_lag / new_cases_smoothed,
         death_report_date = date + 17,
         case_report_date = date) %>%
  ungroup()

# provincial testing
prov_testing <- "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_timeline_testing_positivityrate.csv"
ptest <- fread(prov_testing) %>%
  as_tibble() %>%
  select(date = YYYYMMDD,
         Gauteng, Total) %>%
  mutate(date = ymd(date)) %>%
  pivot_longer(cols = -date,
               values_to = "pos_rate",
               names_to = "Region") %>%
  mutate(Region = ifelse(Region %in% "Total", "South Africa", Region),
         Region = ifelse(Region %in% "Gauteng", "Gauteng Province", Region))

reg_pp_plot <- ptest %>%
  mutate(Region = factor(Region, levels = c( "South Africa", "Gauteng Province"))) %>%
  filter(date >= "2021-01-01") %>%
  ggplot() +
  geom_line(aes(x = date, y = pos_rate, color = Region)) +
  geom_point(aes(x = date, y = pos_rate, color = Region), size = 0.5) +
  scale_color_brewer(palette = "Dark2") + 
  scale_y_continuous(labels = percent_format(accuracy = 2),
                     sec.axis = sec_axis(~.*pp_test_ratio, name = "Daily Tests (7-Day Avg)")) +
  theme_minimal() +
  labs(title = "Weekly Positivity Rate") +
  xlab("") +
  ylab("Positive Rate (7-Day)") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#ggplotly(reg_pp_plot)

# pct plot
pct_df <- s_afr %>%
  mutate(Region = factor(Region, levels = c( "South Africa", "Gauteng Province"))) %>%
  filter(date >= "2021-01-01") %>%
  select(case_pct_pk,
         death_pct_pk,
         case_report_date,
         death_report_date,
         Region) %>%
  rename(`Deaths (17-Day Lag)` = death_pct_pk,
         Cases = case_pct_pk) %>%
  pivot_longer(cols = c(`Deaths (17-Day Lag)`, Cases),
               values_to = "% of Delta Peak")

pct_pk_plot <- s_afr %>%
  mutate(Region = factor(Region, levels = c( "South Africa", "Gauteng Province"))) %>%
  filter(date >= "2021-01-01") %>%
  mutate(fill = "Cases",
         color = "Deaths (17-Day Lag)") %>%
  ggplot +
  geom_area(aes(x = case_report_date, y = case_pct_pk), fill = 'coral1', alpha = 0.4) +
  geom_vline(xintercept = as.numeric(omcr_annc_date), linetype = "dashed") +
  geom_line(aes(x = case_report_date, y = death_pct_pk, color = fill), 
            size = 0.8, alpha = 0.4) +
  #geom_line(aes(x = case_report_date, y = case_pct_pk), color = 'coral1', size = 0.8) +
  geom_line(aes(x = case_report_date, y = death_pct_pk, color = color, label = death_report_date), 
            size = 0.8) +
  geom_line(aes(x = case_report_date, y = death_pct_pk, color = fill), 
            size = 0.8, alpha = 0.4) +
  scale_y_continuous(labels = percent_format(accuracy = 2),
                     breaks = seq(0, 100, 0.1)) +
  theme_minimal() +
  labs(title = "COVID-19 Cases and Lagged Deaths as Percentage of Peak Values",
       caption = "Cases in orange, deaths in black. Deaths lagged 17 days to reflect lag between cases peak and deaths peak during 2021 Delta wave.") +
  xlab("") +
  ylab("Percentage of Summer 2021 (Delta) Peak") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #scale_fill_manual(values = c("Cases" = "coral1")) +
  scale_color_manual(values = c("Deaths (17-Day Lag)" = "black",
                                "Cases" = "coral1")) +
  guides(fill=FALSE) +
  facet_wrap(.~Region, scales = "free", nrow = 2)

# CFR
cfr_plot <- s_afr %>%
  mutate(Region = factor(Region, levels = c( "South Africa", "Gauteng Province"))) %>%
  mutate(CFR = paste0(round(cfr, digits = 4)*100, "%")) %>%
  mutate(case_report_date = date,
         death_report_date = date + 17) %>%
  filter(date >= "2021-01-01") %>%
  ggplot +
  geom_hline(yintercept = 0.0, alpha = 0.5, color = "gray", size = 0.2) +
  geom_point(aes(x = case_report_date, y = cfr, label = death_report_date, label2 = CFR), 
             color = "navy", shape = 1, size = 1) +
  geom_line(aes(x = case_report_date, y = cfr, label = death_report_date), 
            color = "navy", alpha = 0.5 , size = 0.75) +
  geom_line(aes(x = case_report_date, y = cfr, label = death_report_date), 
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
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~Region, scales = "free", nrow = 2)

# pos rate

# assign ratios for axis transformation
s_afr$positive_rate_int = s_afr$positive_rate * 100
max_tests <- max(s_afr$new_tests_smoothed, na.rm = TRUE)
max_pp <- max(s_afr$positive_rate_int, na.rm = TRUE)

pp_test_ratio <- max_tests / max_pp

s_afr_pp <- s_afr %>%
  filter(date >= "2021-01-01") %>%
  filter(Region %in% "South Africa") %>%
  mutate(tests_axis2 = new_tests_smoothed / pp_test_ratio / 100)

pos_rate_plot <- s_afr_pp %>%
  mutate(Region = factor(Region, levels = c( "South Africa", "Gauteng Province"))) %>%

  ggplot +
  geom_vline(xintercept = as.numeric(omcr_annc_date), linetype = "dashed") +
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

# ratios
sa_ratio <- s_afr %>%
  mutate(Region = factor(Region, levels = c( "South Africa", "Gauteng Province"))) %>%
  group_by(Region) %>%
  mutate(case_ratio_smoothed = new_cases_smoothed / lag(new_cases_smoothed, n = 7)) %>%
  mutate(case_ratio = new_cases / lag(new_cases, n = 7)) %>%
  mutate(case_decline = ifelse(case_ratio > 1, 'Growth', 'Decline'),
         case_decline = factor(case_decline, levels = c('Growth', 'Decline'))) %>%
  mutate(case_max_pct = new_cases_smoothed / max(new_cases_smoothed, na.rm = TRUE)) %>%
  mutate(one = 1) %>%
  ungroup()

abv_one <- max(sa_ratio$case_ratio[which(sa_ratio$case_ratio < Inf)], na.rm = TRUE)
  
sa_ratio <- sa_ratio %>%
  mutate(abv_one = 8)

# log cases
sa_ratio_log <-
  sa_ratio %>%
  mutate(Region = factor(Region, levels = c( "South Africa", "Gauteng Province"))) %>%
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
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~Region, nrow = 2)

# ratios
sa_ratio_plot <-

  sa_ratio %>%
  mutate(Region = factor(Region, levels = c( "South Africa", "Gauteng Province"))) %>%
  filter(date >= "2021-01-01") %>%
  mutate(case_decline = factor(case_decline)) %>%
  ggplot() +
  
  geom_ribbon(aes(x = date, ymin=1, ymax=pmax(case_ratio_smoothed, 1)), fill="darksalmon", col="white", alpha=0.3) +
  geom_ribbon(aes(x = date, ymax=1, ymin=pmin(case_ratio_smoothed, 1)), fill="darkseagreen2", col="white", alpha=0.3) +
  geom_hline(yintercept = 1, linetype = 'dashed', size = 0.7, alpha = 0.6) +
  geom_hline(yintercept = .45, linetype = 'dashed', size = 0.7, alpha = 0.01) +
  geom_point(aes(x = date, y = case_ratio, color = case_decline), 
             alpha = 0.7, size = 0.8, show.legend = FALSE) +
  geom_line(aes(x = date, y = case_ratio_smoothed), size = 0.8) +
  theme(legend.position='none') +
  ggtitle("Growth Multiplier of COVID-19 Cases Week-Over-Week") +
  xlab("") +
  ylab("") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  scale_y_continuous(breaks = c(0, 0.5, 1, 2, 4, 8, 16, 32),
                     trans = "log2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_color_manual(values = c("Growth" = "darksalmon",
                                "Decline" = "darkseagreen3"))+
  facet_wrap(~Region, scales = "free", nrow = 2)

# cases/average
sa_case_plot <-
  
  sa_ratio %>%
  mutate(Region = factor(Region, levels = c( "South Africa", "Gauteng Province"))) %>%
  filter(date >= "2021-01-01",
         !is.na(Region)) %>%
  ggplot() +
  geom_vline(xintercept = as.numeric(omcr_annc_date), linetype = "dashed") +
  geom_col(aes(x = date, y = new_cases), alpha = 0.4, fill = "coral1") +
  geom_line(aes(x = date, y = new_cases_smoothed), color = "coral1", size = 1) +
  

  ggtitle("COVID-19 Cases in South Africa") +
  xlab("") +
  ylab("") +
  scale_y_continuous(label=comma, breaks = seq(0, 100000, 5000)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~Region, scales = "free", nrow = 2)

ggplotly(sa_case_plot)

# deaths/average
sa_deaths_plot <-
  
  sa_ratio %>%
  filter(date >= "2021-01-01") %>%
  ggplot() +
  geom_vline(xintercept = as.numeric(omcr_annc_date), linetype = "dashed") +
  geom_col(aes(x = date, y = new_deaths), alpha = 0.4) +
  geom_line(aes(x = date, y = new_deaths_smoothed), size = 1) +
  ylim(0,600) +
  ggtitle("COVID-19 Deaths in South Africa") +
  xlab("") +
  ylab("") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  scale_y_continuous(label = comma, breaks = seq(0, 1000, 100)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~Region, scales = "free", nrow = 2)

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
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~Region, scales = "free", nrow = 2)

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

## Hosp/Case Comparison
cases_append <- s_afr %>%
  #filter(Region %in% "South Africa") %>%
  select(Date = date, seven_day_avg = new_cases_smoothed, Region) %>%
  mutate(variable = "Cases")

comb <- hosp %>%
  mutate(Region = Province) %>%
  mutate(Region = ifelse(Region %in% "Total", "South Africa", Region),
         Region = ifelse(Region %in% "Gauteng", "Gauteng Province", Region)) %>%
  filter(#Province %in% "Total",
         !variable %in% "FacilitiesReporting") %>%
  
  mutate(variable = ifelse(variable %in% "CurrentlyAdmitted", "Total Admissions", variable)) %>%
  mutate(variable = ifelse(variable %in% "CurrentlyAdmitted", "Total Admissions", variable)) %>%
  mutate(variable = ifelse(variable %in% "CurrentlyinICU", "ICU", variable)) %>%
  mutate(variable = ifelse(variable %in% "CurrentlyVentilated", "Ventilated", variable)) %>%
  #mutate(variable = ifelse(variable %in% "NewCases", "New Cases", variable)) %>%
  
  select(Date, Region, variable, seven_day_avg) %>%
  bind_rows(cases_append)

#max_cases_sa <- max(comb$seven_day_avg[comb$variable %in% "Cases" & comb$Date <= "2021-09-01" & comb$Region %in% "South Africa"], na.rm = TRUE)
#max_cases_gt <- max(comb$seven_day_avg[comb$variable %in% "Cases" & comb$Date <= "2021-09-01" & comb$Region %in% "Gauteng Province"], na.rm = TRUE)


comb <- comb %>%
  arrange(Date) %>%
  group_by(variable, Region) %>%
  mutate(pct_val = ifelse(variable %in% "Cases" & Region %in% "South Africa",
                          seven_day_avg / max_case_sa,
                          NA),
         pct_val = ifelse(variable %in% "Cases" & Region %in% "Gauteng Province",
                          seven_day_avg / max_case_gt,
                          pct_val),
         pct_val = ifelse(!variable %in% "Cases",
                          seven_day_avg / max(seven_day_avg, na.rm = TRUE),
                          pct_val)) %>%
  ungroup()

## percent chart
pct_hosp_plot <- comb %>%
  #filter(Region %in% "South Africa") %>%
  mutate(Region = factor(Region, levels = c( "South Africa", "Gauteng Province"))) %>%
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
  ggtitle("Hospitalization/Cases as Percentage of Peak Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  labs(variable = "" ) +
  scale_color_manual(values = c("Cases" = "lightgray",
                                "Total Admissions" = "#6F8FAF", # denim blue
                                "ICU" = "#6495ED", # cornflower (medium)
                                "Ventilated" = "#00008B")) + # dark blue
  facet_wrap(.~Region, scales = "free", nrow = 2)

## ICU percentage
icu <- hosp %>%
  filter(variable %in% c("CurrentlyAdmitted", "CurrentlyVentilated", "CurrentlyinICU")) %>%
  group_by(Province, Owner, Date) %>%
  mutate(admitted = max(seven_day_avg, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct_of_admitted = seven_day_avg / admitted)

icu_pct_plot <- icu %>%
  mutate(Region = Province) %>%
  mutate(Region = ifelse(Region %in% "Total", "South Africa", Region),
         Region = ifelse(Region %in% "Gauteng", "Gauteng Province", Region)) %>%
  filter(Date >= ymd("2021-01-01")) %>%
  filter(variable %in% c("CurrentlyVentilated", "CurrentlyinICU")) %>%
  mutate(Province = factor(Province, levels = c("Total", "Gauteng"))) %>%
  ggplot() +
  geom_line(aes(x = Date, y = pct_of_admitted, color = variable), size = 0.8) +
  geom_vline(xintercept = as.numeric(omcr_annc_date), linetype = "dashed") +
  facet_wrap(~Region) +
  
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


