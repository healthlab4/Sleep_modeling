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


### Function to process models ###
## Function expects models in the for of list ##
## Specify the engine with: glmer or lmer ##

run_lmerTest_pipeline <- function(formula_list, data, engine) {
  
  # Standardize engine input right away
  engine_choice <- tolower(engine)
  if (!engine_choice %in% c("lmer", "glmer")) {
    stop("Engine must be explicitly set to either 'lmer' or 'glmer'.")
  }
  
  process_model <- function(formula_input, index) {
    formula_obj <- as.formula(formula_input)
    outcome_var <- all.vars(formula_obj)[1]
    
    cat(sprintf("Processing Model %d [%s]\n", index, engine_choice))
    
    # Listwise deletion of missing values for variables in this specific formula
    model_vars   <- all.vars(formula_obj)
    clean_data   <- data %>% dplyr::select(all_of(model_vars)) %>% na.omit()
    
    # ---- Execute based on user's engine choice ----
    results <- tryCatch({
      if (engine_choice == "glmer") {
        
        if (!is.factor(clean_data[[outcome_var]])) {
          clean_data[[outcome_var]] <- as.factor(clean_data[[outcome_var]])
        }
        
        model_fit <- lme4::glmer(
          formula = formula_obj, 
          data    = clean_data, 
          family  = 'binomial',
          control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
        )
        
        broom.mixed::tidy(model_fit, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) %>%
          dplyr::rename(estimate_val = estimate) %>%
          dplyr::mutate(metric_type = "Odds Ratio (OR)")
        
      } else if (engine_choice == "lmer") {
        
        model_fit <- lmerTest::lmer(formula = formula_obj, data = clean_data)
        
        broom.mixed::tidy(model_fit, effects = "fixed", conf.int = TRUE) %>%
          dplyr::rename(estimate_val = estimate) %>%
          dplyr::mutate(metric_type = "Beta (b)")
      }
    }, error = function(e) {
      cat(sprintf("  !! Model %d failed: %s\n", index, e$message))
      return(NULL)
    })
    
    # ---- Format Output Row ----
    if (!is.null(results)) {
      results <- results %>%
        dplyr::mutate(
          model_id     = index,
          engine_used  = engine_choice
        ) %>%
        dplyr::select(model_id, engine_used, term, metric_type, estimate_val, std.error, p.value, conf.low, conf.high)
    }
    
    return(results)
  }
  # Map over formulas and stack results
  all_results <- purrr::imap_dfr(formula_list, ~process_model(.x, .y))
  return(all_results)
}
  
all_models <- list(
  Day.MVPA.Lapse ~ scaled_minutes_after_wakeup + scaled_minutes_asleep + day_type + Day.in.Program + (1 |
                                                                                                        ID),
  Day.MVPA.Lapse ~ minutes_after_wakeup_within + minutes_after_wakeup_between + minutes_asleep_within + minutes_asleep_between +
    day_type + Day.in.Program + (1 | ID),
  
  Day.MVPA.Lapse ~ scaled_minutes_asleep + scaled_minutes_in_bed + day_type + Day.in.Program + (1 |
                                                                                                  ID),
  Day.MVPA.Lapse ~ minutes_asleep_within + minutes_asleep_between + minutes_in_bed_within + minutes_in_bed_between + day_type +
    Day.in.Program + (1 | ID),
  
  Day.MVPA.Lapse ~ scaled_minutes_awake + scaled_minutes_in_bed + day_type + Day.in.Program + (1 |
                                                                                                 ID),
  Day.MVPA.Lapse ~ minutes_awake_within + minutes_awake_between + minutes_in_bed_within + minutes_in_bed_between + day_type +
    Day.in.Program + (1 | ID)
)

### Processing all mvpa_lapse models ##
mvpa_model <- run_lmerTest_pipeline(
  formula_list    = all_models,
  data            = merged_data_sleep_ema,
  engine          = "glmer"
)




### Intention (Intend.to.engage.MVPA)

all_models <- list(
  Intend.to.engage.MVPA ~ scaled_minutes_after_wakeup + scaled_minutes_asleep + day_type + Day.in.Program + (1|ID),
  Intend.to.engage.MVPA ~ minutes_after_wakeup_within + minutes_after_wakeup_between + minutes_asleep_within +
    minutes_asleep_between + day_type + Day.in.Program + (1|ID),
  
  Intend.to.engage.MVPA ~ scaled_minutes_asleep + scaled_minutes_in_bed + day_type + Day.in.Program + (1|ID),
  Intend.to.engage.MVPA ~ minutes_asleep_within + minutes_asleep_between + minutes_in_bed_within + minutes_in_bed_between +
    day_type + Day.in.Program + (1|ID),
  
  Intend.to.engage.MVPA ~ scaled_minutes_awake + scaled_minutes_in_bed + day_type + Day.in.Program + (1|ID),
  Intend.to.engage.MVPA ~ minutes_awake_within + minutes_awake_between +  minutes_in_bed_within + minutes_in_bed_between +
    day_type + Day.in.Program + (1|ID),
  
  
  Intend.to.engage.MVPA ~ scaled_minutes_in_bed + scaled_minutes_asleep + day_type + Day.in.Program + (1|ID),
  Intend.to.engage.MVPA ~ minutes_in_bed_within + minutes_in_bed_between + minutes_asleep_within +  minutes_asleep_between +
    day_type + Day.in.Program + (1|ID),
  
  Intend.to.engage.MVPA ~ efficiency +  scaled_minutes_asleep + day_type + Day.in.Program + (1|ID)
)


### Processing all Intention models ##
Intention_model <- run_lmerTest_pipeline(
  formula_list    = all_models,
  data            = merged_data_sleep_ema,
  engine          = "glmer"
)



### MVPA minutes ###
merged_data_sleep_ema <- merged_data_sleep_ema %>%
  dplyr::mutate(mvpa_minutes = fairly_active_mins + vigorous_active_mins) ## mvpa outcome


all_models <- list(
  mvpa_minutes ~ scaled_minutes_after_wakeup + scaled_minutes_asleep + day_type + Day.in.Program + (1|ID),
  mvpa_minutes ~ minutes_after_wakeup_within + minutes_after_wakeup_between + minutes_asleep_within +
    minutes_asleep_between + day_type + Day.in.Program + (1|ID),
  
  mvpa_minutes ~ scaled_minutes_asleep + scaled_minutes_in_bed + day_type + Day.in.Program + (1|ID),
  mvpa_minutes ~ minutes_asleep_within + minutes_asleep_between + minutes_in_bed_within + minutes_in_bed_between +
    day_type + Day.in.Program + (1|ID),
  
  mvpa_minutes ~ scaled_minutes_awake + scaled_minutes_in_bed + day_type + Day.in.Program + (1|ID),
  mvpa_minutes ~ minutes_awake_within + minutes_awake_between +  minutes_in_bed_within + minutes_in_bed_between +
    day_type + Day.in.Program + (1|ID),
  
  
  mvpa_minutes ~ scaled_minutes_in_bed + scaled_minutes_asleep + day_type + Day.in.Program + (1|ID),
  mvpa_minutes ~ minutes_in_bed_within + minutes_in_bed_between + minutes_asleep_within +  minutes_asleep_between +
    day_type + Day.in.Program + (1|ID),
  
  mvpa_minutes ~ efficiency +  scaled_minutes_asleep + day_type + Day.in.Program + (1|ID)
)


### Processing all mvpa_minutes models ##
mvpa_minutes_model <- run_lmerTest_pipeline(
  formula_list    = all_models,
  data            = merged_data_sleep_ema,
  engine          = "lmer"
)





