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

data_fraud_transactions <- read_rds(
  "C:/Users/user/Documents/R3M/R3M-Week-3-Project/data/fraud_transactions_cleaned.rds"
)

# Count the number of unique values in selected columns of interest.
# This provides an indication of the level of variation in each variable.
# This also may provide insights that will guide the focus of the analysis.
data_fraud_transactions |>
  summarise(across(
    c(
      email_domain,
      currency,
      is_cross_border,
      product_type,
      payment_method,
      channel,
      merchant_category,
      customer_id,
      billing_country,
      shipping_country
    ),
    .fns = ~ n_distinct(.x), #This counts the number of unique values
    .names = "{.col}"
  )) |>
  pivot_longer(
    # This will return the object in a vertical tidy format.
    everything(),
    names_to = "items",
    values_to = "unique_values",
  ) # Customer ID has 1092 unique values. It may generally not be useful to analyse customer_id except for very specific purpose.

# Check for missing values and address
data_fraud_transactions |>
  summarise(
    across(everything(), ~ sum(is.na(.x))) # Total the missing values in each column
  ) |>
  pivot_longer(
    # This will return the object in a vertical tidy format.
    everything(),
    names_to = "variables",
    values_to = "missing_values"
  ) |>
  arrange(desc(missing_values)) # The device_fingerprint has the highest number of missing values, followed by the merchant risk scores.

View(data_fraud_transactions)

### Tweleve (12) Business Questions and their  Analyses -------------------------------------------------

# (1) What proportion of total transactions are fraudulent?

# Proportion of fraudulent transactions.
data_fraud_transactions |>
  count(actual_fraud) |>
  mutate(fraud_incidence_prop = n * 100 / nrow(data_fraud_transactions)) # The overall fraud rate for the month is about 2% of the total transaction.

# (2) What percentage of transactions are fraudulent?
# What is the total financial exposure to fraud (amount)?
# Are there differences in  total fraud rate and losses reported by currency?

# Fraud incidence by currency, total fraud losses and fraud rate.
data_fraud_transactions |>
  filter(actual_fraud == "Fraudulent") |>
  group_by(currency) |>
  summarise(
    fraud_count = n(),
    total_fraud_loss = format(sum(amount), big.mark = ","),
    average_fraud_loss = round(mean(amount, na.rm = T), digits = 1),
    fraud_rate = round(
      fraud_count / nrow(data_fraud_transactions) * 100,
      digits = 1
    )
  ) |>
  flextable() |>
  set_header_labels(
    currency = "Currency",
    fraud_count = "Fraud count",
    total_fraud_loss = "Total fraud loss",
    average_fraud_loss = "Average fraud loss",
    fraud_rate = " Fraud rate"
  ) |>
  align(j = (2:5), align = "center") |>
  autofit() |>
  add_header_lines("Fraud incidence by currency", top = TRUE) |> # Header title
  align(part = "header", align = 'center') |> # Align header
  bg(bg = "#d3d3d337", i = 2, part = "header") |> # Color background for style
  bold(part = "header") |>
  bg(bg = "#d3d3d337", part = "body", i = 2) |>
  color(i = 1, part = "body", color = "#088F8F") |> # Use color to emphasize finding
  bold(i = 1, part = "body", j = c(1, 2, 3, 5))
#Fraud rates vary by currency, with EUR recording the highest fraud rate at 1.5%, compared with GBP and USD.
#EUR accounted for the majority of fraudulent transactions, with an average fraudulent transaction value of approximately EUR 60.
#The total value of fraudulent transactions was approximately 1,200 EUR, 429 GBP, and 73 USD for EUR, GBP, and USD transactions, respectively.

# (3) What is the daily fraud rate?

# Fraud incidence and trend
# Set up variables for plot title
plot_title_fraud_incidence_1 <- "one"
plot_title_fraud_incidence_2 <- "two"

# Dynamic plot title
plot_title_fraud_incidence <- marquee_glue(
  "On days when fraud occurs,  {.#088F8F **{plot_title_fraud_incidence_1}** } or {.#088F8F **{plot_title_fraud_incidence_2 }** } cases are typical."
)


# Plot the chart
data_fraud_transactions |>
  mutate(
    day_of_the_month = day(transaction_date)
  ) |>
  filter(actual_fraud == "Fraudulent") |>
  group_by(day_of_the_month) |>
  summarise(
    total_fraud = sum(amount),
    incidence = n()
  ) |>
  mutate(
    fraud_incidence_per_day = if_else(
      incidence %in% c(1, 2),
      "typical",
      "outlier"
    )
  ) |>
  ggplot(aes(
    x = day_of_the_month,
    y = incidence,
    fill = fraud_incidence_per_day
  )) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values = c("#D3D3D3", '#088F8F')) +
  theme_minimal() +
  labs(
    title = plot_title_fraud_incidence,
    subtitle = "In a given month, fraud may not occur every day."
  ) +
  xlab("Day of the month") +
  ylab("Fraud count per day") +
  annotate(
    geom = "text",
    x = 29,
    y = 2,
    hjust = 1,
    color = 'black',
    family = "serif",
    label = "Exceptional case with 3 counts",
    size = 4.5,
    fontface = 'bold',
    angle = 90
  ) +
  theme(
    legend.position = 'none',
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(
      linetype = 0.3
    ),
    axis.title.y = element_text(
      size = 13,
      vjust = 1.8,
      hjust = 0.5,
    ),
    axis.text.x = element_text(
      face = "bold",
      size = 11,
      margin = NULL
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 11,
      margin = NULL
    ),
    plot.title = element_marquee(
      width = 1,
      size = 19,
      vjust = 0,
      margin = NULL,
      lineheight = 1,
    ),
    plot.subtitle = element_marquee(
      width = 1,
      size = 14,
      vjust = 0,
      margin = NULL,
      lineheight = 1
    )
  )
# One or two cases of fraud incidence per day are typical, with three cases on a single day being an outlier."
# Fraud incidence in a given month ranges between one and two per day with three being an outlier.

## Customer risk analysis

# (4) Which customer age groups have the highest incidence of fraud?
data_fraud_transactions |>
  filter(actual_fraud == "Fraudulent") |>
  count(customer_age, sort = TRUE) |>
  arrange(desc(customer_age)) #Fraud incidence does not appear to vary substantially by customer age. Customers across all age groups appear to be similarly likely to experience transaction fraud.
# The three largest fraudulent transactions by transaction value involved card payments. However, further analysis is required to determine whether card payments are more susceptible to fraud than other payment methods.

# Are VIP customers more or less likely to experience fraud than non-VIP customers?
data_fraud_transactions |>
  filter(
    actual_fraud == "Fraudulent"
  ) |>
  group_by(is_vip) |>
  summarise(
    fraud_count = n(),
    total_fraud_loss = sum(amount),
  ) # No transactions involving VIP customers were identified as fraudulent; all fraudulent transactions involved non-VIP customers.

# (6) Is customer tenure associated with a lower risk of fraud?
average_tenure <- mean(data_fraud_transactions$customer_tenure_days)

data_fraud_transactions |>
  mutate(
    diff_in_tenure = customer_tenure_days > average_tenure,
    tenure_category = if_else(
      customer_tenure_days > average_tenure,
      "Long tenure",
      "Short tenure"
    )
  ) |>
  filter(actual_fraud == "Fraudulent") |>
  count(diff_in_tenure, tenure_category, name = 'fraud_count') |>
  mutate(prop = percent(fraud_count / sum(fraud_count))) |>
  select(-diff_in_tenure) |>
  flextable() |>
  set_header_labels(
    values = c(
      tenure_category = 'Tenure category',
      fraud_count = "Fraud count",
      prop = "Proportion"
    )
  ) |>
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Shorter tenure days are associated with higher fraud incidence",
        props = fp_text_default(font.size = 12, bold = TRUE)
      )
    )
  ) |>
  autofit() |>
  hline(
    i = 1,
    part = "header",
    border = fp_border(width = 0.5)
  ) |>
  color(part = 'body', color = '#088F8F', i = 1) # Fraud incidence appears to vary by customer tenure. Customers with tenure below the average are more likely to have experienced fraud.
# Specifically, 60% of customers with shorter tenure were victims of fraud, compared with 40% of customers with longer tenure.

# (7) Which five customers experienced the highest fraud losses?
data_fraud_transactions |>
  filter(actual_fraud == "Fraudulent") |>
  slice_max(order_by = amount, n = 5) |>
  select(
    customer_id,
    customer_age,
    amount,
    currency,
    channel,
    payment_method,
    customer_tenure_days
  ) |>
  flextable() |>
  set_header_labels(
    customer_id = 'Customer ID',
    customer_age = 'Customer age',
    amount = 'Amount',
    currency = 'Currency',
    channel = 'Channel',
    payment_method = 'Payment',
    customer_tenure_days = 'Tenure'
  ) |>
  set_caption(
    caption = as_paragraph(as_chunk(
      "The single highest loss by a customer is EUR 216.80 ",
      props = fp_text_default(font.size = 12, bold = TRUE)
    ))
  ) |>
  theme_zebra(
    even_body = "#d3d3d337",
    odd_body = "transparent",
    odd_header = "transparent",
    even_header = "#d3d3d337"
  ) |>
  color(part = 'body', i = 1, color = '#088F8F') |>
  hline(part = "body", i = 5, border = fp_border(width = 1)) |>
  hline_top(part = "header", border = fp_border(width = 1)) |>
  hline_top(part = "body", border = fp_border(width = 0.5)) |>
  autofit()
#It is also evident that the highest-value fraud involved card payments. Does this suggest that card payments are more susceptible to fraud?

## Digital properties and fraud incidence.
# (8) Which payment methods have the highest incidence of fraud?

# Set up plot title variables.
plot_title_variable_payment_method <- "Card"
plot_title_payment_method <- marquee_glue(
  "{.#088F8F **{plot_title_variable_payment_method}** } payment transactions recorded the highest fraud cases."
)

# Plot fraud incidence chart
data_fraud_transactions |>
  filter(actual_fraud == "Fraudulent") |>
  count(payment_method, sort = TRUE) |>
  mutate(
    payment_method_risk = if_else(
      n >= 18,
      "High risk",
      "Risky"
    )
  ) |>
  ggplot(aes(
    y = fct_reorder(payment_method, n, .desc = F),
    x = n,
    fill = payment_method_risk,
    label = n
  )) +
  geom_col(width = 0.6) +
  scale_fill_manual(
    values = c('#088F8F', "#D3D3D3")
  ) +
  xlab('Fraud count') +
  ylab('Payment method') +
  geom_text_repel(
    hjust = 2,
    color = "White",
    size = 4,
    fontface = "bold"
  ) +
  theme_minimal() +
  scale_y_discrete(
    labels = c(
      "card" = "Card",
      "wallet" = "Wallet",
      "bank_transfer" = "Bank transfer"
    )
  ) +
  labs(
    title = plot_title_payment_method,
    subtitle = "Making it the riskiest transaction payment method."
  ) +
  theme(
    plot.title = element_marquee(
      width = 1,
      size = 19,
      vjust = 0,
      margin = NULL,
      lineheight = 0.5
    ),
    plot.subtitle = element_text(
      size = 15,
      vjust = 1,
      lineheight = 0.5
    ),
    plot.title.position = "plot",
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      linewidth = 0.3
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 11,
      vjust = 1,
      hjust = 1,
      margin = NULL
    ),
    axis.title.y = element_text(
      face = "plain",
      size = 13,
      vjust = 0.5,
      hjust = 0.5
    ),
    legend.position = 'none',
    axis.text.x.bottom = element_blank(),
    axis.title.x = element_blank()
  ) # Card payment transactions recorded the highest fraud.


# (9) Which transactions channels have the highest fraud rates?

# Set up the plot title variables
plot_title_variable_channel <- "Web"
plot_title_channel <- marquee_glue(
  "{.#088F8F **{plot_title_variable_channel}** }  transactions channel recorded the highest fraud cases."
)

# Plot chart
data_fraud_transactions |>
  filter(actual_fraud == "Fraudulent") |>
  count(channel, sort = TRUE) |>
  mutate(
    channel_risk = if_else(
      n > 15,
      "High risk channel",
      "Low risk channel"
    )
  ) |>
  ggplot(aes(
    y = fct_reorder(channel, n, .desc = F),
    x = n,
    fill = channel_risk,
    labels = n
  )) +
  geom_col(width = 0.6) +
  ylab(
    "Transaction channels"
  ) +
  scale_fill_manual(
    values = c('#088F8F', "#D3D3D3")
  ) +
  scale_y_discrete(
    labels = c(
      "web" = "Web",
      "mobile_app" = "Mobile app",
      "pos" = "POS"
    )
  ) +
  geom_text_repel(
    hjust = 2,
    color = "White",
    size = 4,
    fontface = "bold"
  ) +
  theme_minimal() +
  labs(
    title = plot_title_channel,
    subtitle = "Making it the most vulnerable transaction channel."
  ) +
  theme(
    plot.title = element_marquee(
      width = 1,
      size = 19,
      vjust = 0,
      margin = NULL,
      lineheight = 1
    ),
    plot.subtitle = element_text(
      size = 15,
      vjust = 1,
      lineheight = 1.5
    ),
    plot.title.position = "plot",
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      linewidth = 0.3
    ),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      face = "bold",
      size = 12,
      vjust = 1,
      hjust = 1,
      margin = NULL
    ),
    axis.title.y = element_text(
      face = "plain",
      size = 13,
      vjust = 2,
      hjust = 0.5
    ),
  ) # Web channel transations reported the highest fraud.


## Geographic locations and  Fraud Patterns.
# (10) Are cross-border transactions riskier?
# I defined a cross-border transaction as one where the ip_country, billing_country, and shipping_country differ.

data_fraud_transactions_cross_border <- data_fraud_transactions |>
  # create cross_border variable based on the above definition criteria
  mutate(
    is_cross_border = if_else(
      (ip_country != billing_country) &
        (billing_country != shipping_country),
      "cross border",
      "within border",
      "check"
    )
  )

# Determine the proportion of fraudulent transactions that are cross border.
data_fraud_transactions_cross_border |>
  filter(actual_fraud == "Fraudulent") |>
  count(actual_fraud, is_cross_border) |>
  mutate(prop = percent(n / sum(n))) |>
  flextable() |>
  set_header_labels(
    values = c(
      actual_fraud = "Status",
      is_cross_border = "Nature",
      n = "Fraud count",
      prop = "Prop."
    )
  ) |>
  color(i = 1, part = "body", color = '#088F8F') |>
  set_caption(
    as_paragraph(
      as_chunk(
        "Majority of fradulent transactions are cross border",
        props = fp_text_default(font.size = 12, bold = TRUE)
      )
    )
  ) |>
  autofit()

# The majority of fraudulent transactions (88%) are cross-border transactions.
# These are transactions in which the IP country differs from the billing country and the billing country differs from the shipping country.

# (11) Which are the top billing countries for fraudulent transaction?

# variables for the plot title
billing_country_high_risk_1 <- "Great Britain (GB)"
billing_country_high_risk_2 <- "Ireland (IE)"

# The plot title
plot_title <- marquee_glue(
  "{.#088F8F **{billing_country_high_risk_1}**} and {.#088F8F **{billing_country_high_risk_2}**} are the top billing destinations
     for fraudulent transactions."
)


data_fraud_transactions_cross_border |>
  filter(actual_fraud == "Fraudulent") |>
  summarise(
    fraud_incidence = n(),
    .by = billing_country
  ) |>
  mutate(
    prop = fraud_incidence / sum(fraud_incidence),
  ) |>
  mutate(
    billing_country_risk = if_else(
      billing_country %in% c("GB", "IE"),
      "High risk",
      "Risky"
    )
  ) |>
  mutate(
    billing_country = fct_reorder(billing_country, prop, .desc = FALSE)
  ) |>
  ggplot(
    aes(
      y = billing_country,
      x = prop,
      fill = billing_country_risk,
      label = percent(prop)
    ) # This turns the prop doubles into percentage values
  ) +
  geom_col() +
  scale_fill_manual(
    values = c('#088F8F', "#D3D3D3")
  ) +
  geom_text_repel(
    hjust = 1.4,
    color = "White",
    size = 4,
    fontface = "bold"
  ) +
  theme_minimal() +
  labs(
    title = plot_title,
    subtitle = "They represent more than 50% of the total fraud losses.",
  ) +
  ylab("Billing country") +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      linewidth = 0.3
    ),
    axis.text.x = element_blank(), #This is made invisble
    axis.text.y = element_text(
      face = "bold",
      size = 11,
      vjust = 1,
      hjust = 1,
      margin = NULL
    ),
    axis.title.x = element_blank(),
    legend.position = 'none',
    axis.title.y = element_text(
      face = "plain",
      size = 13,
      vjust = 2,
      hjust = 0.5
    ),
    plot.title = element_marquee(
      width = 1,
      size = 19,
      vjust = 0,
      margin = NULL,
      lineheight = 1
    ),
    plot.subtitle = element_text(
      size = 15,
      vjust = 1,
      lineheight = 1.5
    ),
    plot.title.position = "plot"
  ) +
  scale_x_continuous(
    labels = percent_format() # This formats the x-axis text to % but I decided to leave the axis not visible.
  )

# Which billing country recorded the highest fraud incidence?
data_fraud_transactions |>
  filter(actual_fraud == "Fraudulent") |>
  count(billing_country, sort = TRUE) |>
  head(10) # Great Britain and Ireland

# Merchant Risk Analysis
# (11) ) Does fraud incidence vary by merchant risk score?

# Set up plot title variable
plot_title_boxplot_variabe <- "Fraudulent transactions"
plot_title_box_plot <- marquee_glue(
  "{.#6cabdd **{plot_title_boxplot_variabe}** }  are more prevalent with high-risk merchants."
)

# Plot chart
data_fraud_transactions |>
  ggplot(aes(x = actual_fraud, y = merchant_riskscore, fill = actual_fraud)) +
  geom_boxplot(
    outlier.shape = 1,
    outlier.color = "orange",
    box.linewidth = 1,
    outlier.fill = "orange",
    whisker.linewidth = 1
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c('#088F8F', "#6cabdd")
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.3
    ),
    panel.grid.minor.y = element_blank(),
    #panel.grid.major.y = element_blank(),
    axis.text.x = element_text(
      face = "bold",
      size = 11,
      vjust = 1,
      margin = NULL
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 11,
      vjust = 1,

      margin = NULL
    ),
    axis.title.x = element_blank(),
    legend.position = 'none',
    axis.title.y = element_text(
      face = "plain",
      size = 13,
      vjust = 3,
      hjust = 0.5
    ),
    plot.title = element_marquee(
      width = 1,
      size = 18,
      hjust = 0,
      vjust = 1,
      margin = NULL,
      lineheight = 1
    ),
    plot.subtitle = element_text(
      size = 14,
      vjust = 1,
      lineheight = 7
    ),
    plot.title.position = "plot"
  ) +
  ylab("Merchants' risk scores") +
  labs(
    title = plot_title_box_plot,
    subtitle = "Low-risk merchants are less vulnerable."
  ) # Fraud transactions are common with high-risk scores relative to non-fraudulent ones


# (12) Which merchant categories have the highest fraud rates?

# set up plot title variables.
plot_title_merchant_category_marketplaces <- 'Marketplaces'
plot_title_merchant_category_fashion <- 'fashion'
plot_title_merchant_category <- marquee_glue(
  " {.#088F8F **{plot_title_merchant_category_marketplaces}**} and {.#088F8F **{plot_title_merchant_category_fashion}** } merchants are the most susceptible to fraudulent transactions."
)

# Plot chart
data_fraud_transactions |>
  filter(actual_fraud == "Fraudulent") |>
  count(
    merchant_category,
    actual_fraud,
    sort = T,
    name = "fraud_count"
  ) |>
  mutate(
    merchant_category_risk = if_else(
      merchant_category %in% c("marketplace", "fashion"),
      "High-risk merchant",
      "Risky merchant"
    )
  ) |>
  ggplot(aes(
    y = fct_reorder(merchant_category, fraud_count, .desc = F),
    x = fraud_count,
    fill = merchant_category_risk,
    label = fraud_count
  )) +
  geom_col() +
  theme_minimal() +
  geom_text_repel(
    hjust = 2,
    color = "White",
    size = 6,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c('#088F8F', "#D3D3D3")
  ) +
  labs(
    title = plot_title_merchant_category
  ) +
  ylab("Merchant category channels") +
  scale_y_discrete(
    labels = c(
      "marketplace" = "Marketplace",
      "fashion" = "Fashion",
      "digital_goods" = "Digital goods",
      "gaming" = "Gaming",
      "electronics" = "Electronics",
      "utilities" = "Utilities",
      "telecoms" = "Telecoms",
      "restaurants" = "Restaurants"
    )
  ) +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      linewidth = 0.3
    ),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      face = "bold",
      size = 12,
      vjust = 1,
      hjust = 1,
      margin = NULL
    ),
    axis.title.x = element_blank(),
    legend.position = 'none',
    axis.title.y = element_text(
      face = "plain",
      size = 13,
      vjust = 2,
      hjust = 0.5
    ),
    plot.title = element_marquee(
      width = 1,
      size = 19,
      hjust = 0,
      vjust = 1,
      margin = NULL,
      lineheight = 1
    ),
    plot.subtitle = element_text(
      size = 14,
      vjust = 1,
      lineheight = 7
    ),
    plot.title.position = "plot"
  ) # Fraudulent transations are more prevalent in marketplaces than in other channels
