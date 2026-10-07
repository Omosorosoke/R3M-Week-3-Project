# Load packages ----------------------------------------------------------
library(tidyverse)
library(gt)
library(gtExtras)
library(ggthemes)
library(janitor)
library(hrbrthemes)
library(scales)
library(marquee)
library(ggrepel)
library(flextable)
library(gtExtras)
library(officer) # Work with flexatable


# Import data ------------------------------------------------------------
data_fraud_transactions <- read_csv("data-raw/ACCA_DSP_synthetic_data.csv")

data_fraud_transactions_raw <- data_fraud_transactions # keep a copy of the raw data

# Explore data -----------------------------------------------------------

glimpse(data_fraud_transactions)
View(data_fraud_transactions)
# Check columns for proper names, appropriate data types  and recode values
data_fraud_transactions <- data_fraud_transactions |>
  mutate(
    transaction_date = mdy(transaction_date),
    actual_fraud = factor(
      actual_fraud,
      levels = c(0, 1),
      labels = c("Legitimate", "Fraudulent") # convert to factors and label values
    )
  )


# Keep a copy of clean data in .rds file ---------------------------------

write_rds(
  x = data_fraud_transactions,
  file = "data/fraud_transactions_clean.rds"
)
