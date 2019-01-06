library(dplyr)
library(here)
library(readr)


# Load data
customer <- read_csv(here("00_Data/raw", "WA_Fn-UseC_-Telco-Customer-Churn.csv"))

# 11 customers have TotalCharges missing, and all have tenure == 0, and also none of them have Churned,
# so deleting those records will not influence model. 
# drop cusotmerID, change SeniorCitizen to "Yes" / "No", Churn to factor
# rest of character columns to factors

customer_clean <- customer %>%
    filter(!is.na(TotalCharges)) %>%
    select(-c(customerID, TotalCharges, gender, PhoneService)) %>%
    mutate(SeniorCitizen = ifelse(SeniorCitizen == 1, "Yes", "No"),
           Churn = as.factor(Churn)) %>%
    mutate_if(is.character, as.factor)


write_csv(customer_clean, here("00_Data/clean/", "customer_clean.csv"))


