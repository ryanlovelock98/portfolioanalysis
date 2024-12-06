#Technical Assessment

# Set working directory
#setwd("xx")

#install.packages("readxl", "magrittr", "dplyr", "tidyr", "lubridate", "knitr", "ggplot2", "kableExtra")

# Load necessary libraries
library(readxl)
library(magrittr)
library(dplyr)
library(tidyr)
library(lubridate)
library(knitr)
library(ggplot2)
library(kableExtra)

# Read data from specified excel file and sheet
data <- read_excel('data.xlsx', sheet = 1, col_names = TRUE)

#Convert refdate to Date format
data$refdate <- as.Date(data$refdate)

# Rename columns to make them more readable
data <- data %>%
  rename(Date = refdate, `Portfolio Weight (%)` = `Weight (%)`, Beta = `Beta (Relative to Benchmark)`, `Market Cap ($)` = `Market Capitalization (USD)`, `Price ($)` = `Price(USD)`,
         `Environmental Score` = `Overall ESG Environmental Score`, `Social Score` = `Overall ESG Social Score`,
         `Governance Score` = `Overall ESG Governance Score`, `GICS Sector` = GICS_sector, `Cont. TE (%)` = `%Contribution to Tracking Error`,
         `Cont. Total Risk (%)` = `%Contribution to Total Risk`)

# Convert other columns to their required formats
data <- data %>%
  mutate(
    `Portfolio Weight (%)` = round(as.numeric(`Portfolio Weight (%)`*100),2),
    `Active Weight (%)` = round(as.numeric(`Active Weight (%)`*100),2),
    `Total Risk` = round(as.numeric(`Total Risk`),2),
    `Beta` = round(as.numeric(`Beta`),2),
    `Market Cap ($)` = round(as.numeric(`Market Cap ($)`),2),
    `Cont. TE (%)` = round(as.numeric(`Cont. TE (%)`*100),2),
    `Cont. Total Risk (%)` = round(as.numeric(`Cont. Total Risk (%)`*100),2),
    `Overall ESG Score` = as.numeric(`Overall ESG Score`),
    `Environmental Score` = as.numeric(`Environmental Score`),
    `Social Score` = as.numeric(`Social Score`),
    `Governance Score` = as.numeric(`Governance Score`)
  )

# Categorise assets into Cash, FX and Equity using the Market Cap column
data <- data %>%
  mutate(`Asset Type` = case_when(
    `Market Cap ($)` == 0 ~ 'Cash',
    is.na(`Market Cap ($)`) ~ 'FX',
    TRUE ~ 'Equity'
  ))

# Categorise assets into market cap categories
data <- data %>% 
  mutate(`Market Cap Category ($)` = case_when(
    `Market Cap ($)` < 300000000 ~ "<300mn",
    `Market Cap ($)` >= 300000000 & `Market Cap ($)`< 2000000000 ~ "300mn-2bn",
    `Market Cap ($)` >= 2000000000 & `Market Cap ($)`< 6000000000 ~ "2bn-6bn",
    `Market Cap ($)` >= 6000000000 & `Market Cap ($)`< 10000000000 ~ "6bn-10bn",
    `Market Cap ($)` > 10000000000 ~ ">10bn", 
    TRUE ~ NA_character_
  ))

# Categorise assets into ESG categories
data <- data %>%
  mutate(`ESG Category` = case_when(
    `Overall ESG Score` < 3 ~ 'Laggard',
    `Overall ESG Score` >= 3 & `Overall ESG Score` < 7 ~ 'Average',
    `Overall ESG Score` >= 7 ~ 'Leader',
    TRUE ~ NA_character_
  ))

# Remove 31st December as this is a weekend date and the data does not look accurate 
ts_data <- data %>% filter(Date != '2023-12-31')
# Split out latest date to get current portfolio shape
latest_data <- data %>% filter(Date == '2023-12-29')

# Use the latest data to calculate top-level risk stats 
portfolio_te <- round(sqrt(sum(((latest_data$`Active Weight (%)`/100)^2)*(latest_data$`Total Risk`^2))),2)
portfolio_beta <- round(weighted.mean(latest_data$`Beta`, w = latest_data$`Portfolio Weight (%)`, na.rm = TRUE),2)
portfolio_esg <- round(weighted.mean(latest_data$`Overall ESG Score`, w = latest_data$`Portfolio Weight (%)`, na.rm = TRUE),2)
n_holdings <- round(nrow(latest_data %>% filter(`Asset Type` == 'Equity')),0)
cash <- round(sum(latest_data %>% filter(`Asset Type` == "Cash") %>% pull(`Portfolio Weight (%)`), na.rm = TRUE),2)
fx <- round(sum(latest_data %>% filter(`Asset Type` == "FX") %>% pull(`Portfolio Weight (%)`), na.rm = TRUE),2)
fund_vol <- round(sum(latest_data$`Total Risk`, na.rm = TRUE)/100, 2)

# Create table to summarise top-level risk stats
portfolio_stats <- data.frame(Metric = c("Portfolio Tracking Error %", "Portfolio Beta", "Portfolio ESG Score", "Number of Holdings", "Cash %", "Fund Volatility %", "FX %"),
                              Value = c(portfolio_te,  portfolio_beta, portfolio_esg, n_holdings, cash, fund_vol, fx))
# Display
kable(portfolio_stats, caption = "Top Level Risk Statistics") %>%
  kable_styling(font_size = 9)

## Current portfolio shape 
# Calculate the sum of contributions
total_contribution <- sum(latest_data$`Cont. TE (%)`)

# Calculate the normalisation factor
normalisation_factor <- total_contribution / 100

# Normalise the contributions
latest_data <- latest_data %>%
  mutate(`Normalised Contribution to TE (%)` = `Cont. TE (%)` / normalisation_factor)

# Create new column showing normalised tracking error contributions that = 100
latest_data$`Normalised Contribution to TE (%)` <- round(latest_data$`Normalised Contribution to TE (%)`,2)

# Top 10 contributors to tracking error (relative to benchmark)
top_te_contributors <- latest_data %>% 
  arrange(desc(`Normalised Contribution to TE (%)`)) %>% 
  select(`Asset Name`, 
         `Portfolio Weight (%)`, 
         `Active Weight (%)`, 
         `Normalised Contribution to TE (%)`,
         `Beta`,
         `GICS Sector`,
         `Country`) %>%
  head(10)

# Find totals of these columns to bind to table
totals_te <- top_te_contributors %>%
  summarise(
    `Asset Name` = "Total",
    `Portfolio Weight (%)` = sum(`Portfolio Weight (%)`, na.rm = TRUE),
    `Active Weight (%)` = sum(`Active Weight (%)`, na.rm = TRUE),
    `Normalised Contribution to TE (%)` = sum(`Normalised Contribution to TE (%)`, na.rm = TRUE),
    `Beta` = NA,  # Not summing Beta, as it doesn't make sense to sum it
    `GICS Sector` = NA,
    `Country` = NA
  )

# Bind the total row to the existing data frame
top_te_contributors_with_total <- bind_rows(top_te_contributors, totals_te)

# Display
kable(top_te_contributors_with_total, caption = "Largest Contr. TE - Top 10") %>%
  kable_styling(font_size = 9)


# Top 10 contributors to total risk (absolute)
top_risk_contributors <- latest_data %>% 
  arrange(desc(`Cont. Total Risk (%)`)) %>% 
  select(`Asset Name`, 
         `Portfolio Weight (%)`, 
         `Active Weight (%)`, 
         `Cont. Total Risk (%)`,
         `Beta`,
         `GICS Sector`,
         `Country`) %>%
  head(10)

totals <- top_risk_contributors %>%
  summarise(
    `Asset Name` = "Total",
    `Portfolio Weight (%)` = sum(`Portfolio Weight (%)`, na.rm = TRUE),
    `Active Weight (%)` = sum(`Active Weight (%)`, na.rm = TRUE),
    `Cont. Total Risk (%)` = sum(`Cont. Total Risk (%)`, na.rm = TRUE),
    `Beta` = NA,  # Not summing Beta, as it doesn't make sense to sum it
    `GICS Sector` = NA,
    `Country` = NA
  )

# Bind the total row to the existing data frame
top_risk_contributors_with_total <- bind_rows(top_risk_contributors, totals)

# Display
kable(top_risk_contributors_with_total, caption = "Largest Contr. Total Risk - Top 10") %>%
  kable_styling(font_size = 9)



## Sector breakdown
sector_breakdown <- latest_data %>% 
  filter(`Asset Type` == 'Equity') %>%
  group_by(`GICS Sector`) %>% 
  summarise(`Port Stocks` = n(), 
            `Portfolio Weight (%)` = sum(`Portfolio Weight (%)`, na.rm = TRUE),
            `Active Weight (%)` = sum(`Active Weight (%)`, na.rm = TRUE),
            `Normalised Contribution to TE (%)` = sum(`Normalised Contribution to TE (%)`, na.rm = TRUE)) %>%
  arrange(desc(`Normalised Contribution to TE (%)`))

kable(sector_breakdown) %>%
  kable_styling(font_size = 9)

ggplot(sector_breakdown, aes(x=reorder(`GICS Sector`, -`Normalised Contribution to TE (%)`))) +
  geom_bar(aes(y = `Normalised Contribution to TE (%)`), stat = "identity", fill = "#4432a8") +
  geom_point(aes(y = `Active Weight (%)` * max(`Normalised Contribution to TE (%)`)/max(`Active Weight (%)`)), color = '#D55E00', size = 3) +
  scale_y_continuous(
    name = "Contribution to Tracking Error (%)",
    sec.axis = sec_axis(~ . * max(sector_breakdown$`Active Weight (%)`) / max(sector_breakdown$`Normalised Contribution to TE (%)`), name = "Active Weight (%)")
  ) +
  theme_minimal() +
  xlab("Sector") +
  ggtitle("Portfolio Sector Breakdown") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## Country breakdown
country_breakdown <- latest_data %>%
  filter(`Asset Type` == 'Equity') %>% 
  group_by(Country) %>% 
  summarise(`Port Stocks` = n(), 
            `Portfolio Weight (%)` = sum(`Portfolio Weight (%)`, na.rm = TRUE),
            `Active Weight (%)` = sum(`Active Weight (%)`, na.rm = TRUE),
            `Normalised Contribution to TE (%)` = sum(`Normalised Contribution to TE (%)`, na.rm = TRUE)) %>%
  arrange(desc(`Normalised Contribution to TE (%)`))


kable(country_breakdown) %>%
  kable_styling(font_size = 9)

ggplot(country_breakdown, aes(x=reorder(`Country`, -`Normalised Contribution to TE (%)`))) +
  geom_bar(aes(y = `Normalised Contribution to TE (%)`), stat = "identity", fill = "#4432a8") +
  geom_point(aes(y = `Active Weight (%)` * max(`Normalised Contribution to TE (%)`)/max(`Active Weight (%)`)), color = '#D55E00', size = 3) +
  scale_y_continuous(
    name = "Contribution to Tracking Error (%)",
    sec.axis = sec_axis(~ . * max(country_breakdown$`Active Weight (%)`) / max(country_breakdown$`Normalised Contribution to TE (%)`), name = "Active Weight (%)")
  ) +
  theme_minimal() +
  xlab("Country") +
  ggtitle("Portfolio Country Breakdown") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## Market cap breakdown
mc_breakdown <- latest_data %>% 
  filter(`Asset Type` == 'Equity') %>%
  group_by(`Market Cap Category ($)`) %>% 
  summarise(`Port Stocks` = n(), 
            `Portfolio Weight (%)` = sum(`Portfolio Weight (%)`, na.rm = TRUE),
            `Active Weight (%)` = sum(`Active Weight (%)`, na.rm = TRUE),
            `Normalised Contribution to TE (%)` = sum(`Normalised Contribution to TE (%)`, na.rm = TRUE)) %>%
  arrange(desc(`Normalised Contribution to TE (%)`))

# Display
kable(mc_breakdown) %>%
  kable_styling(font_size = 9)

ggplot(mc_breakdown, aes(x=reorder(`Market Cap Category ($)`, -`Normalised Contribution to TE (%)`))) +
  geom_bar(aes(y = `Normalised Contribution to TE (%)`), stat = "identity", fill = "#4432a8") +
  geom_point(aes(y = `Active Weight (%)` * max(`Normalised Contribution to TE (%)`)/max(`Active Weight (%)`)), color = '#D55E00', size = 3) +
  scale_y_continuous(
    name = "Contribution to Tracking Error (%)",
    sec.axis = sec_axis(~ . * max(mc_breakdown$`Active Weight (%)`) / max(mc_breakdown$`Normalised Contribution to TE (%)`), name = "Active Weight (%)")
  ) +
  theme_minimal() +
  xlab("Market Cap") +
  ggtitle("Portfolio Market Cap Breakdown") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ESG scores
top10_esg <- latest_data %>% 
  arrange(desc(`Overall ESG Score`)) %>%
  select(`Asset Name`, `Overall ESG Score`, `Environmental Score`, `Social Score`, `Governance Score`, `Active Weight (%)`, `Normalised Contribution to TE (%)`,  `GICS Sector`, Country) %>%
  head(10)

bottom10_esg <- latest_data %>% 
  arrange(`Overall ESG Score`) %>%
  select(`Asset Name`, `Overall ESG Score`, `Environmental Score`, `Social Score`, `Governance Score`, `Active Weight (%)`, `Normalised Contribution to TE (%)`,  `GICS Sector`, Country) %>%
  head(10)

# Display
kable(top10_esg) %>%
  kable_styling(font_size = 9)
kable(bottom10_esg) %>%
  kable_styling(font_size = 9)


## Time series analysis

# Filter for end of month data
month_end_data <- ts_data %>%
  group_by(year = year(Date), month = month(Date)) %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  select(-year, -month)

## Tracking error over time
te_ot <- ts_data %>% 
  filter(!is.na(Date)) %>%
  group_by(Date) %>%
  summarise(Tracking_Error = sqrt(sum(((`Active Weight (%)`/100)^2)*(`Total Risk`^2))))

# Line chart showing tracking error over time
ggplot(te_ot, aes( x= Date, y = Tracking_Error)) +
  geom_line(color = '#4432a8', size = 1) +
  theme_minimal() +
  xlab("Date") +
  ylab("Tracking Error") +
  ggtitle("Tracking Error Over Time") +
  expand_limits(y=0)

## Beta over time
beta_ot <- ts_data %>% 
  filter(!is.na(Date)) %>%
  group_by(Date) %>%
  summarise(Beta = weighted.mean(`Beta`, w = `Portfolio Weight (%)`, na.rm = TRUE))

# Line chart for beta over time
ggplot(beta_ot, aes( x= Date, y = Beta)) +
  geom_line(color = '#4432a8', size = 1) +
  theme_minimal() +
  xlab("Date") +
  ylab("Beta") +
  ggtitle("Beta Over Time") +
  expand_limits(y=0)

## Fund vol over time
fundvol_ot <- ts_data %>% 
  filter(!is.na(Date)) %>%
  group_by(Date) %>%
  summarise(FundVol = sum(`Total Risk`, na.rm = TRUE)/100)

# Line chart for fund volatility over time
ggplot(fundvol_ot, aes( x= Date, y = FundVol)) +
  geom_line(color = '#4432a8', size =1) +
  theme_minimal() +
  xlab("Date") +
  ylab("Fund Volatility") +
  ggtitle("Fund Volatility Over Time") +
  expand_limits(y=0)

# ESG breakdown 
# Calculate weighted average ESG scores for each date
weighted_esg_scores <- month_end_data %>%
  group_by(Date) %>%
  summarise(
    `E Score` = sum(`Portfolio Weight (%)` * `Environmental Score`, na.rm = TRUE) / sum(`Portfolio Weight (%)`, na.rm = TRUE),
    `S Score` = sum(`Portfolio Weight (%)` * `Social Score`, na.rm = TRUE) / sum(`Portfolio Weight (%)`, na.rm = TRUE),
    `G Score` = sum(`Portfolio Weight (%)` * `Governance Score`, na.rm = TRUE) / sum(`Portfolio Weight (%)`, na.rm = TRUE),
    `Overall ESG Score` = sum(`Portfolio Weight (%)` * `Overall ESG Score`, na.rm = TRUE) / sum(`Portfolio Weight (%)`, na.rm = TRUE)
  )

# transform wide data to long to make graphing easier 
long_esg_scores <- weighted_esg_scores %>%
  pivot_longer(cols = contains("Score"), names_to = "ESG_Type", values_to = "Score")

# Plot combined ESG scores over time
ggplot(long_esg_scores, aes(x = Date, y = Score, color = ESG_Type)) +
  geom_line(size = 1) +
  theme_minimal() +
  xlab("Date") +
  ylab("Weighted ESG Score") +
  ggtitle("Weighted ESG Scores Over Time") +
  scale_color_manual(values = c("E Score" = '#4432a8', "S Score" = '#D55E00', "G Score" = '#184f1a', "Overall ESG Score" = '#54180e'),
                     breaks = c("Overall ESG Score", "E Score", "S Score", "G Score")) +
  expand_limits(y = 0)

## Weights of ESG brackets
category_weights <- month_end_data %>%
  group_by(Date, `ESG Category`) %>%
  summarise(total_weight = sum(`Portfolio Weight (%)`, na.rm = TRUE)) %>%
  ungroup()

# Plot how portfolio weights of esg categories have changed over time
ggplot(category_weights, aes(x = Date, y = total_weight, color = `ESG Category`)) +
  geom_line(size = 1) +
  theme_minimal() +
  xlab("Date") +
  ylab("Total Portfolio Weight (%)") +
  ggtitle("Changes in Portfolio Weights by ESG Category Over Time") +
  scale_color_manual(
    values = c("Laggard" = '#4432a8', "Average" = '#D55E00', "Leader" = '#009E73'),
    name = "ESG Category"
  ) +
  expand_limits(y = 0)

# Performance over time 
# Filter for year to date figures, for equity positions and exclude health care as data doesnt look correct
perf_data <- ts_data %>% filter(`Asset Type` == 'Equity', Date >= "2023-01-01", `GICS Sector` != 'Health Care')

# Calculate the weighted average price of the portfolio over time
# Group the data by Date and compute the weighted average price
fund_performance <- perf_data %>%
  group_by(Date) %>%
  summarise(weighted_avg_price = sum(`Portfolio Weight (%)` * `Price ($)`, na.rm = TRUE) / sum(`Portfolio Weight (%)`, na.rm = TRUE)
            )
# Normalise the weighted average price to create an index
# The index is set to 100 at the first date
fund_performance <- fund_performance %>% 
  mutate(Index = (weighted_avg_price / first(weighted_avg_price)) *100)

# Plot the performance index over time
ggplot(fund_performance, aes(x = Date, y =Index)) +
  geom_line(color = '#4432a8') +
  geom_point(color = '#D55E00') +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")


# Sector performance over time
# Calculate the weighted average price of the portfolio over time
# Group the data by Date and sector to compute the weighted average price
index_sector_performance <- perf_data %>%
    group_by(Date, `GICS Sector`) %>%
    summarise(sector_weighted_avg_price = sum(`Portfolio Weight (%)` * `Price ($)`, na.rm = TRUE) / sum(`Portfolio Weight (%)`, na.rm = TRUE)
    ) %>%
    ungroup()

# Normalise the weighted average price to create an index
# The index is set to 100 at the first date
index_sector_performance <- index_sector_performance %>%
  group_by(`GICS Sector`) %>%
  mutate(Index = (sector_weighted_avg_price / first(sector_weighted_avg_price)) *100) %>%
           ungroup()

# Plot the performance index over time by sector breakdown
ggplot(index_sector_performance, aes(x = Date, y =Index, color = `GICS Sector`)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = 'right') + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
  



