# Data cleaning
# 1) Load data
surgery1 <- read.table("surgery1.txt", header = T, sep = ";")
surgery2 <- read.table("surgery2.txt", header = T, sep = ",")

# 2) Merge data
surgery <- merge(surgery1, surgery2, by = "id")

# 3)
library(VIM)
aggr(surgery, numbers = T)
# We see a big load of missing data in the severity variable, when calculating, we see that 15.9% is missing.

# 4)
library(dplyr)
library(naniar)
surgery %>% filter_all(any_vars(. %in% c('')))

surgery <- surgery %>%
  replace_with_na(replace = list(technique = ''
  ))
surgery %>% filter_all(any_vars(. %in% c('')))

surgery$technique <- droplevels(surgery$technique)

# 5)
aggr(surgery, numbers = T)
# On multiple rows, we have missing values on the variables severity, bmi, srh and technique. 
# There is one row where the variables bmi and severity contains a missing value at the same time. 
# There is one row where the variables srh and severity contains a missing value at the same time.
# There is one row where the variables 

# 6)
table(sapply(surgery[c(5,6,9,12:18)], class))

# 7)
surgery$T <- as.factor(surgery$T)
surgery$inflammation <- as.ordered(surgery$inflammation)
surgery$hospital <- as.factor(surgery$hospital)
surgery$prior_treatment <- as.factor(surgery$prior_treatment)
surgery$srh <- as.ordered(surgery$srh)
surgery$surgery_type <- as.factor(surgery$surgery_type)
surgery$severity <- as.ordered(surgery$severity)

# 8)
surgery$severity_ind <- ifelse(surgery$severity > 2, 1, 0)

# Analysis
# 9)
surgery$surgery_date <- as.Date(surgery$surgery_date)
surgery$event_date <- as.Date(surgery$event_date, format = "%m/%d/%Y")
surgery$Y <- surgery$event_date - surgery$surgery_date

# 10)
newsurgery <- surgery[c(4:17, 19, 20)]

# 11)
impute_fun <- function(data, type) {
  if (type == "cc") {
    completedata <- 
      na.omit(data)
    return(completedata)
  } else if (type == "hotdeck") {
    completedata <- 
      hotdeck(data)
    return(completedata)
  } else {
    print("This doesn't work, try something else!")
  }
}

# 12)
surgery_cc <- impute_fun(newsurgery, "cc")

# 13)
library(survival)
Surv(surgery_cc$Y, surgery_cc$event)[1:20,]

# 14)
library(survminer)
levels(surgery_cc$event)
surgery_cc$event <- as.numeric(surgery_cc$event)
surgery_cc$event[surgery_cc$event == 1] <- 0
surgery_cc$event[surgery_cc$event == 2] <- 1
survmodel <- survfit(Surv(Y, event) ~ T, data = surgery_cc) 
survplot <- survminer::ggsurvplot(survmodel, conf.int = T, legend.labs = c("Surgery B", "Surgery A"),
                                  data = surgery_cc)
survplot

# 15)
cox.model <- coxph(Surv(Y, event) ~ T + inflammation + srh + surgery_type + sex + bmi + age,
        data = surgery_cc)
summary(cox.model)
