library(dplyr)
library(stringr)
library(lubridate)
library(tidyverse)
library(lubridate)
library(hms)
library(patchwork)
library(lmerTest)
library(broom.mixed)
library(knitr)
library(kableExtra)
library(ggplot2)
library(lattice)
library(sjPlot)

ema_data <- read.csv("")
sleep_file <- read_csv('')
bout_data_daily <- read_csv('')


## Reading Sleep file and creating additional weekday variables 
sleep_file_processed <- sleep_file %>%
  dplyr::select(ID, contains('PTZ'), contains('minutes'), efficiency) %>%
  dplyr::mutate(
    day_of_week = wday(start_date_PTZ, label = TRUE, abbr = FALSE),
    day_type = ifelse(wday(start_date_PTZ) %in% c(1, 7), "Weekend", "Weekday"),
    day_type = factor(day_type, levels = c("Weekday", "Weekend")) # Set Weekday as baseline
  ) 


## Reading EMA file filter to only include rows where Intend.to.engage.MVPA.Day == 'Yes' and Survey_Type == 'Morning', 
## this ensures that there is a single row per participant.
## Join this file with the bout_summary file by using ID and Date.Prompt = Date_PTZ.
## Create a new variable called Day.MVPA.Lapse where Day.MVPA.Lapse == 0 when Intend.to.engage.MVPA.Day == "Yes" & (fairly_active_mins + vigorous_active_mins) > 0, 
## and Day.MVPA.Lapse == 1 when Intend.to.engage.MVPA.Day == "Yes" & (fairly_active_mins + vigorous_active_mins) == 0. 

ema_data_processed <- ema_data %>%
  dplyr::mutate(Date.Prompt = as.Date(Date.Prompt)) %>%
  dplyr::filter(Intend.to.engage.MVPA.Day == 'Yes', Survey_Type == 'Morning') %>%
  dplyr::inner_join(bout_data_daily,
                    by = c('ID' = 'user_id', 'Date.Prompt' = 'Date_PTZ')) %>%
  dplyr::mutate(
    Day.MVPA.Lapse = case_when(
      Intend.to.engage.MVPA.Day == "Yes" &
        (fairly_active_mins + vigorous_active_mins) > 0 ~ 0,
      Intend.to.engage.MVPA.Day == "Yes" &
        (fairly_active_mins + vigorous_active_mins) == 0 ~ 1
    )
  ) %>%
  dplyr::mutate(Day.MVPA.Lapse = as.factor(Day.MVPA.Lapse)) %>%
  dplyr::select(
    ID,
    Date.Prompt,
    Intend.to.engage.MVPA.Day,
    Day.MVPA.Lapse,
    fairly_active_mins,
    vigorous_active_mins,
    Day.in.Program,
    Intend.to.engage.MVPA
  ) %>%
  data.frame()

## Merging with Sleep Data : mvpa_frame with sleep data by using ID and Date.Prompt == end_date_PTZ. 
## Sleep predictors such as 'minutes_after_wakeup','minutes_asleep','minutes_awake' are scaled.

merged_data_sleep_ema <- ema_data_processed %>%
  dplyr::inner_join(
    dplyr::select(
      sleep_file_processed,
      ID,
      end_date_PTZ,
      contains('minutes'),
      day_type,
      efficiency
    ),
    by = c('ID', 'Date.Prompt' = 'end_date_PTZ')
  ) %>%
  dplyr::select(-minutes_to_fall_sleep) %>%
  dplyr::mutate(
    scaled_minutes_after_wakeup = scale(minutes_after_wakeup),
    scaled_minutes_asleep = scale(minutes_asleep),
    scaled_minutes_awake = scale(minutes_awake),
    scaled_minutes_in_bed = scale(minutes_in_bed)
  ) %>%
  group_by(ID) %>%
  mutate(
    minutes_in_bed_between       = mean(minutes_in_bed, na.rm = TRUE),
    minutes_asleep_between       = mean(minutes_asleep, na.rm = TRUE),
    minutes_after_wakeup_between = mean(minutes_after_wakeup, na.rm = TRUE),
    minutes_awake_between = mean(minutes_awake, na.rm = TRUE),
    
    minutes_in_bed_within       = minutes_in_bed - minutes_in_bed_between,
    minutes_asleep_within       = minutes_asleep - minutes_asleep_between,
    minutes_after_wakeup_within = minutes_after_wakeup - minutes_after_wakeup_between,
    minutes_awake_within = minutes_awake - minutes_awake_between
  ) %>%
  dplyr::mutate(Intend.to.engage.MVPA = as.factor(Intend.to.engage.MVPA)) %>%
  ungroup()





