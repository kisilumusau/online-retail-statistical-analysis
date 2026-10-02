# ================================================================
# ONLINE RETAIL STATISTICAL ANALYSIS
# ================================================================
# Dataset: Online Retail.xlsx
# Purpose: Data cleaning, exploratory analysis, product/country analysis,
#          and an inferential comparison of UK vs non-UK transaction values.
# ================================================================

# -----------------------------
# 1. PACKAGES
# -----------------------------

library(readxl)
library(dplyr)
library(ggplot2)

# -----------------------------
# 2. IMPORT AND INITIAL CHECKS
# -----------------------------

data <- read_excel("data/Online Retail.xlsx")

cat("Dataset dimensions:\n")
print(dim(data))

cat("\nFirst six rows:\n")
print(head(data))

cat("\nDataset structure:\n")
str(data)

cat("\nSummary statistics:\n")
print(summary(data))

cat("\nMissing values by variable:\n")
print(colSums(is.na(data)))

cat("\nDuplicate rows:\n")
print(sum(duplicated(data)))

cat("\nQuantity range:\n")
print(range(data$Quantity, na.rm = TRUE))

cat("\nUnit price range:\n")
print(range(data$UnitPrice, na.rm = TRUE))

cat("\nNegative quantities:\n")
print(sum(data$Quantity < 0, na.rm = TRUE))

cat("\nZero quantities:\n")
print(sum(data$Quantity == 0, na.rm = TRUE))

cat("\nZero unit prices:\n")
print(sum(data$UnitPrice == 0, na.rm = TRUE))

cat("\nCancelled invoices:\n")
print(sum(grepl("^C", data$InvoiceNo)))

# Compare negative quantities with cancellation-coded invoices.
negative_rows <- data$Quantity < 0
cancelled_rows <- grepl("^C", data$InvoiceNo)

cat("\nNegative quantity rows that are cancelled:\n")
print(sum(negative_rows & cancelled_rows, na.rm = TRUE))

cat("\nNegative quantity rows that are not cancelled:\n")
print(sum(negative_rows & !cancelled_rows, na.rm = TRUE))

# -----------------------------
# 3. DATA CLEANING
# -----------------------------
# The analysis focuses on completed positive sales records.
# Duplicate rows are removed and observations with non-positive
# quantity or unit price are excluded.

clean_data <- data %>%
  distinct() %>%
  filter(
    Quantity > 0,
    UnitPrice > 0
  ) %>%
  mutate(
    SalesValue = Quantity * UnitPrice
  )

cat("\nOriginal number of rows:\n")
print(nrow(data))

cat("\nCleaned number of rows:\n")
print(nrow(clean_data))

cat("\nRows removed during cleaning:\n")
print(nrow(data) - nrow(clean_data))

# -----------------------------
# 4. INVOICE-LEVEL DATA
# -----------------------------
# Product-line records are aggregated so that each invoice is one
# transaction-level observation for the UK/non-UK hypothesis test.

invoice_data <- clean_data %>%
  group_by(InvoiceNo) %>%
  summarise(
    TransactionValue = sum(SalesValue),
    Country = first(Country),
    CustomerID = first(CustomerID),
    InvoiceDate = first(InvoiceDate),
    .groups = "drop"
  ) %>%
  mutate(
    CustomerGroup = if_else(
      Country == "United Kingdom",
      "United Kingdom",
      "Non-United Kingdom"
    )
  )

cat("\nNumber of invoices:\n")
print(nrow(invoice_data))

cat("\nTransaction value summary:\n")
print(summary(invoice_data$TransactionValue))

# -----------------------------
# 5. UK VS NON-UK DESCRIPTIVE ANALYSIS
# -----------------------------

group_summary <- invoice_data %>%
  group_by(CustomerGroup) %>%
  summarise(
    n = n(),
    mean = mean(TransactionValue),
    median = median(TransactionValue),
    sd = sd(TransactionValue),
    minimum = min(TransactionValue),
    maximum = max(TransactionValue),
    .groups = "drop"
  )

cat("\nInvoices by customer group:\n")
print(table(invoice_data$CustomerGroup))

cat("\nDescriptive statistics by customer group:\n")
print(group_summary, width = Inf)

uk_mean <- mean(
  invoice_data$TransactionValue[
    invoice_data$CustomerGroup == "United Kingdom"
  ]
)

non_uk_mean <- mean(
  invoice_data$TransactionValue[
    invoice_data$CustomerGroup == "Non-United Kingdom"
  ]
)

mean_difference <- uk_mean - non_uk_mean

cat("\nMean difference (UK - non-UK):\n")
print(mean_difference)

# Distribution of transaction values.
ggplot(invoice_data, aes(x = TransactionValue)) +
  geom_histogram(bins = 50) +
  labs(
    title = "Distribution of Transaction Values",
    x = "Transaction Value (€)",
    y = "Number of Invoices"
  )

# Boxplot by customer group.
ggplot(invoice_data, aes(x = CustomerGroup, y = TransactionValue)) +
  geom_boxplot() +
  labs(
    title = "Transaction Value by Customer Group",
    x = "Customer Group",
    y = "Transaction Value (€)"
  )

# QQ plot.
ggplot(invoice_data, aes(sample = TransactionValue)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "QQ Plot of Transaction Values",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# -----------------------------
# 6. HYPOTHESIS TEST
# -----------------------------
# H0: Mean transaction value is the same for UK and non-UK invoices.
# H1: Mean transaction value differs between UK and non-UK invoices.
# Welch's two-sample t-test is used because equal variances are not assumed.

test_result <- t.test(
  TransactionValue ~ CustomerGroup,
  data = invoice_data,
  var.equal = FALSE
)

cat("\nWelch two-sample t-test:\n")
print(test_result)

cat("\n95% confidence interval:\n")
print(test_result$conf.int)

cat("\np-value:\n")
print(test_result$p.value)

cat("\nt-statistic:\n")
print(test_result$statistic)

# -----------------------------
# 7. EFFECT SIZE: COHEN'S d
# -----------------------------

pooled_sd <- sqrt(
  (
    (group_summary$n[1] - 1) * group_summary$sd[1]^2 +
      (group_summary$n[2] - 1) * group_summary$sd[2]^2
  ) /
    (group_summary$n[1] + group_summary$n[2] - 2)
)

cohens_d <- (non_uk_mean - uk_mean) / pooled_sd

cat("\nCohen's d (non-UK - UK):\n")
print(cohens_d)

# -----------------------------
# 8. COUNTRY-LEVEL SALES ANALYSIS
# -----------------------------

country_summary <- clean_data %>%
  group_by(Country) %>%
  summarise(
    NumberOfTransactions = n_distinct(InvoiceNo),
    TotalSales = sum(SalesValue),
    AverageLineValue = mean(SalesValue),
    MedianLineValue = median(SalesValue),
    .groups = "drop"
  ) %>%
  arrange(desc(TotalSales))

cat("\nTop 10 countries by total sales:\n")
top_10_countries <- country_summary %>%
  arrange(desc(TotalSales)) %>%
  head(10)

print(top_10_countries, width = Inf)

ggplot(
  top_10_countries,
  aes(
    x = reorder(Country, TotalSales),
    y = TotalSales
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Total Sales",
    x = "Country",
    y = "Total Sales (€)"
  )

# Top countries by average product-line sales value.
top_10_avg_countries <- country_summary %>%
  arrange(desc(AverageLineValue)) %>%
  head(10)

cat("\nTop 10 countries by average product-line sales value:\n")
print(top_10_avg_countries, width = Inf)

ggplot(
  top_10_avg_countries,
  aes(
    x = reorder(Country, AverageLineValue),
    y = AverageLineValue
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Average Product-Line Sales Value",
    x = "Country",
    y = "Average Sales Value per Product Line (€)"
  )

# -----------------------------
# 9. PRODUCT-LEVEL ANALYSIS
# -----------------------------

product_summary <- clean_data %>%
  group_by(StockCode, Description) %>%
  summarise(
    TotalQuantity = sum(Quantity),
    TotalSales = sum(SalesValue),
    NumberOfTransactions = n_distinct(InvoiceNo),
    AverageUnitPrice = mean(UnitPrice),
    .groups = "drop"
  ) %>%
  arrange(desc(TotalSales))

# Exclude the three explicitly identified non-merchandise/service codes
# from the merchandise ranking.
merchandise_data <- clean_data %>%
  filter(!StockCode %in% c("DOT", "POST", "M"))

merchandise_summary <- merchandise_data %>%
  group_by(StockCode, Description) %>%
  summarise(
    TotalQuantity = sum(Quantity),
    TotalSales = sum(SalesValue),
    NumberOfTransactions = n_distinct(InvoiceNo),
    AverageUnitPrice = mean(UnitPrice),
    .groups = "drop"
  ) %>%
  arrange(desc(TotalSales))

top_10_products <- merchandise_summary %>%
  head(10)

cat("\nTop 10 merchandise products by total sales:\n")
print(top_10_products, width = Inf)

ggplot(
  top_10_products,
  aes(
    x = reorder(Description, TotalSales),
    y = TotalSales
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Merchandise Products by Total Sales",
    x = "Product",
    y = "Total Sales (€)"
  )

# -----------------------------
# 10. END OF ANALYSIS
# -----------------------------
