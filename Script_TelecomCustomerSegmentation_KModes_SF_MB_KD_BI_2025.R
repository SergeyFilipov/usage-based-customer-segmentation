### Block 01: Load libraries and initial data ----
library(dplyr)
library(readr)
library(klaR)
library(ggplot2)
library(tidyr)
library(corrplot)
library(stringr)

rm(list = ls()) # Remove all objects from the environment

load("C:/Users/Sergey Filipov/Desktop/Проект по Наука за данните/Задание/Digital_2015_2016.RData")

df <- dd
df_orig <- df
dim(df)


### Block 02: Data cleaning – handle missing values, duplicates, and filter records ----
# Check for missing values in main service flag columns
sum(is.na(df$CUS_FL_MTEL))
sum(is.na(df$CUS_FL_INT))
sum(is.na(df$CUS_FL_DTV))
sum(is.na(df$CUS_FL_TEL))

# Remove rows where all key service flags are NA
df <- df %>%
  filter(!(is.na(CUS_FL_MTEL) & is.na(CUS_FL_INT) & is.na(CUS_FL_DTV) & is.na(CUS_FL_TEL)))

df_orig <- df # Update the original copy after cleaning

# Drop duplicates: keep the most recent entry per customer
# Convert MONTH_CODE to integer (if needed)
df <- df %>%
  mutate(MONTH_CODE = as.integer(MONTH_CODE))

# Extract year from MONTH_CODE
df <- df %>%
  mutate(Year = substr(as.character(MONTH_CODE), 1, 4))

# Sort data by customer and month (latest first)
df_sorted <- df %>%
  arrange(CUSTOMERNUMBER, desc(MONTH_CODE))

# Keep only the latest record for each customer
df <- df_sorted %>%
  distinct(CUSTOMERNUMBER, .keep_all = TRUE)

# Remove rows where the customer is a business
df <- df %>%
  filter(CUS_SEX != "Business")


### Block 03: Create column groups for each service flag ----
dtv_columns <- grep("DTV", names(df), value = TRUE)         # DTV columns
internet_columns <- grep("^INT_", names(df), value = TRUE)  # Internet columns
mobile_columns <- grep("^MTEL", names(df), value = TRUE)    # Mobile columns
tel_columns <- grep("^TEL_", names(df), value = TRUE)       # Telephone columns

# Check datatypes of internet columns for users with and without Internet service
df %>%
  filter(CUS_FL_INT == 1) %>%
  dplyr::select(all_of(internet_columns)) %>%
  sapply(class)

df %>%
  filter(CUS_FL_INT == 0) %>%
  dplyr::select(all_of(internet_columns)) %>%
  sapply(class)


### Block 04: CReplace NAs with 0 for active service users; summarize flag columns ----
# Impute NAs for feature columns based on active service flag
idx <- which(df$CUS_FL_TEL == 1)
df[idx, tel_columns] <- lapply(df[idx, tel_columns], function(x) {
  x[is.na(x)] <- 0
  x
})

idx <- which(df$CUS_FL_MTEL == 1)
df[idx, mobile_columns] <- lapply(df[idx, mobile_columns], function(x) {
  x[is.na(x)] <- 0
  x
})

idx <- which(df$CUS_FL_DTV == 1)
df[idx, dtv_columns] <- lapply(df[idx, dtv_columns], function(x) {
  x[is.na(x)] <- 0
  x
})

idx <- which(df$CUS_FL_INT == 1)
df[idx, internet_columns] <- lapply(df[idx, internet_columns], function(x) {
  x[is.na(x)] <- 0
  x
})

# Keep only numeric columns; add time variables
df <- df[, 1:273]
df$Year <- substr(as.character(df$MONTH_CODE), 1, 4)
df$MONTH <- as.integer(substr(as.character(df$MONTH_CODE), 5, 6))
df$QUARTER <- ceiling(df$MONTH / 3)
df$Year_Quarter <- paste0(df$Year, " Q", df$QUARTER)

# Summarize per-flag column statistics
prefix_to_flag <- c(
  "DTV_" = "CUS_FL_DTV",
  "INT_" = "CUS_FL_INT",
  "MTEL_" = "CUS_FL_MTEL",
  "TEL_" = "CUS_FL_TEL"
)

numeric_cols <- names(df)[sapply(df, is.numeric)]
ColumnValuesSummary <- data.frame(Column = numeric_cols, stringsAsFactors = FALSE)

# Map columns to their service group (DTV, INT, MTEL, TEL) based on prefix
ColumnValuesSummary$Group <- sapply(ColumnValuesSummary$Column, function(col) {
  prefix <- names(prefix_to_flag)[
    sapply(names(prefix_to_flag), function(p) str_starts(col, p))
  ]
  if (length(prefix) == 0) return(NA)
  gsub("_$", "", prefix)
})

ColumnValuesSummary <- ColumnValuesSummary[!is.na(ColumnValuesSummary$Group), ]
flag_map <- c(DTV = "CUS_FL_DTV", INT = "CUS_FL_INT", MTEL = "CUS_FL_MTEL", TEL = "CUS_FL_TEL")
ColumnValuesSummary$Flag <- flag_map[ColumnValuesSummary$Group]

count_na_flag_0 <- function(col, flag_col) {
  sum(is.na(df[[col]][df[[flag_col]] == 0]))
}

count_na_flag_1 <- function(col, flag_col) {
  sum(is.na(df[[col]][df[[flag_col]] == 1]))
}

count_zero_flag_1 <- function(col, flag_col) {
  sum(df[[col]][df[[flag_col]] == 1] == 0, na.rm = TRUE)
}

count_nonnull_gt0_flag_1 <- function(col, flag_col) {
  sum(df[[col]][df[[flag_col]] == 1] > 0, na.rm = TRUE)
}

ColumnValuesSummary <- ColumnValuesSummary %>%
  rowwise() %>%
  mutate(
    `NaN (flag=0)` = count_na_flag_0(Column, Flag),
    `NaN (flag=1)` = count_na_flag_1(Column, Flag),
    `Zero values (flag=1)` = count_zero_flag_1(Column, Flag),
    `Non-null > 0 (flag=1)` = count_nonnull_gt0_flag_1(Column, Flag)
  ) %>%
  ungroup()

ColumnValuesSummary <- ColumnValuesSummary %>%
  dplyr::select(Group, Column, `NaN (flag=0)`, `NaN (flag=1)`, `Zero values (flag=1)`, `Non-null > 0 (flag=1)`) %>%
  arrange(desc(`Non-null > 0 (flag=1)`))

print(ColumnValuesSummary) # Summary of column values by flag


### Block 05: Correlation matrix and outlier check ----
# Correlation matrix of main usage features
windows(width = 10, height = 8)  # Opens a new plotting window of size 10x8 inches

cols <- c('MTEL_VOICE_MOU', 'MTEL_TOT_VOL_MB', 'TEL_OUT_MOU', 'DTV_VIEW_NBR', 
          'DTV_NR_VOD', 'INT_VOL_DOWN', 'INT_VOL_UP', 'INT_VOL_STREAMING', 
          'INT_VOL_TOT')

corr <- cor(df[, cols], use = "pairwise.complete.obs")

corrplot(corr, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black",
         col = colorRampPalette(c("blue", "white", "red"))(200))

# Boxplot outlier visualization for key features
df_long <- data.frame(
  Feature = rep(cols, each = nrow(df)),
  Value = unlist(df[cols])
)

df_long <- df_long[!is.na(df_long$Value), ]

ggplot(df_long, aes(x = Feature, y = Value)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplots to visualize outliers")

rm(df_long, df_sorted)


### Block 06: Feature binning for K-Modes clustering ----

# MTEL_VOICE_CNT: binning based on business logic and distribution skew
filtered <- df %>%
  filter(CUS_FL_MTEL == 1, MTEL_VOICE_CNT >= 0)

max_val <- max(filtered$MTEL_VOICE_CNT, na.rm = TRUE)
median_val <- median(df$MTEL_VOICE_CNT[df$CUS_FL_MTEL == 1], na.rm = TRUE)

ggplot(filtered, aes(x = MTEL_VOICE_CNT)) +
  geom_histogram(bins = 30, aes(y = after_stat(density)), color = "black", fill = "lightblue") +
  geom_vline(xintercept = median_val, color = "red", linetype = "dashed") +
  labs(
    title = paste("Histogram of MTEL_VOICE_CNT (0 -", round(max_val), ")"),
    x = "MTEL_VOICE_CNT",
    y = "Probability Density"
  ) +
  theme_minimal() +
  theme(panel.grid.major = element_line(linetype = "dashed", color = "grey80"))


voice_data <- df$MTEL_VOICE_CNT[df$CUS_FL_MTEL == 1]
q <- quantile(voice_data, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
iqr <- q[3] - q[1]
outlier_threshold <- q[3] + 1.5 * iqr

bins <- c(0, 0.01, 22, 75, outlier_threshold, max(df$MTEL_VOICE_CNT, na.rm = TRUE))
labels <- c("Zero user", "Low", "Medium", "High", "Very High")

df$MTEL_VOICE_CNT_BIN <- cut(df$MTEL_VOICE_CNT, breaks = bins, labels = labels, include.lowest = TRUE)
df$MTEL_VOICE_CNT_BIN <- factor(df$MTEL_VOICE_CNT_BIN, levels = c(labels, "no user"))
df$MTEL_VOICE_CNT_BIN[df$CUS_FL_MTEL == 0 | is.na(df$MTEL_VOICE_CNT_BIN)] <- "no user"

ggplot(df, aes(x = MTEL_VOICE_CNT_BIN)) +
  geom_bar(color = "black", fill = "skyblue") +
  labs(
    title = "MTEL_VOICE_CNT Distribution with Outlier Bin",
    x = "Usage Level",
    y = "Number of Users"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey80"),
    axis.text.x = element_text(angle = 0)
  )


# MTEL_TOT_VOL_MB: binning based on business logic and distribution
filtered <- df[df$CUS_FL_MTEL == 1 & df$MTEL_TOT_VOL_MB >= 0, ]

max_val <- max(filtered$MTEL_TOT_VOL_MB, na.rm = TRUE)
median_val <- median(df$MTEL_TOT_VOL_MB[df$CUS_FL_MTEL == 1], na.rm = TRUE)

hist(filtered$MTEL_TOT_VOL_MB, breaks = 50, probability = TRUE, 
     col = "lightblue", border = "black", 
     main = paste("Histogram of MTEL_TOT_VOL_MB (0 -", round(max_val), ")"),
     xlab = "MTEL_TOT_VOL_MB")
abline(v = median_val, col = "red", lty = 2)
legend("topright", legend = paste("Median =", round(median_val, 2)), col = "red", lty = 2)

vol_data <- df$MTEL_TOT_VOL_MB[df$CUS_FL_MTEL == 1]
q1 <- quantile(vol_data, 0.25, na.rm = TRUE)
q3 <- quantile(vol_data, 0.75, na.rm = TRUE)
iqr <- q3 - q1
outlier_limit <- q3 + 1.5 * iqr

bins <- c(0, 0.01, 100, 1300, outlier_limit, max(df$MTEL_TOT_VOL_MB, na.rm = TRUE))
labels <- c("Zero user", "Low", "Medium", "High", "Very High")
df$MTEL_TOT_VOL_MB_BIN <- cut(df$MTEL_TOT_VOL_MB, breaks = bins, labels = labels, include.lowest = TRUE)

df$MTEL_TOT_VOL_MB_BIN <- factor(df$MTEL_TOT_VOL_MB_BIN, levels = c(labels, "no user"))
df$MTEL_TOT_VOL_MB_BIN[df$CUS_FL_MTEL == 0 | is.na(df$MTEL_TOT_VOL_MB_BIN)] <- "no user"

barplot(table(df$MTEL_TOT_VOL_MB_BIN), border = "black", col = "lightgreen",
        main = "MTEL_TOT_VOL_MB Distribution with Outlier Bin",
        xlab = "Usage Level", ylab = "Number of Users")

   
# TEL_OUT_MOU: binning for clustering
filtered <- df[df$TEL_OUT_MOU >= 0, ]
max_val <- max(filtered$TEL_OUT_MOU, na.rm = TRUE)
median_val <- median(df$TEL_OUT_MOU, na.rm = TRUE)

hist(filtered$TEL_OUT_MOU, breaks = 30, col = "lightblue", border = "black",
     main = paste("Histogram of TEL_OUT_MOU (0 -", round(max_val), ")"),
     xlab = "TEL_OUT_MOU", ylab = "Frequency")
abline(v = median_val, col = "red", lty = 2)
abline(v = max_val, col = "green", lty = 2)
legend("topright", legend = c(paste("Median =", round(median_val, 2)), 
                              paste("Max =", round(max_val, 2))),
       col = c("red", "green"), lty = 2)

tel_data <- df$TEL_OUT_MOU[df$CUS_FL_MTEL == 1]
q1 <- quantile(tel_data, 0.25, na.rm = TRUE)
q3 <- quantile(tel_data, 0.75, na.rm = TRUE)
iqr <- q3 - q1
outlier_threshold <- q3 + 1.5 * iqr

bins <- c(0, 0.01, 2000, 6000, outlier_threshold, max(df$TEL_OUT_MOU, na.rm = TRUE))
labels <- c("Zero user", "Low", "Medium", "High", "Very High")

df$TEL_OUT_MOU_BIN <- cut(df$TEL_OUT_MOU, breaks = bins, labels = labels, include.lowest = TRUE)

df$TEL_OUT_MOU_BIN <- factor(df$TEL_OUT_MOU_BIN, levels = c(labels, "no user"))
df$TEL_OUT_MOU_BIN[df$CUS_FL_MTEL == 0 | is.na(df$TEL_OUT_MOU_BIN)] <- "no user"

barplot(table(df$TEL_OUT_MOU_BIN), col = "lightgreen", border = "black",
        main = "TEL_OUT_MOU Distribution with Outlier Bin",
        xlab = "Usage Level", ylab = "Number of Users")


# DTV_VIEW_NBR: binning for clustering
filtered <- df[df$CUS_FL_DTV == 1 & df$DTV_VIEW_NBR >= 0, ]
median_val <- median(df$DTV_VIEW_NBR[df$CUS_FL_DTV == 1], na.rm = TRUE)

hist(filtered$DTV_VIEW_NBR, breaks = 50, col = "lightblue", border = "black",
     main = "Histogram of DTV_VIEW_NBR (>0)", xlab = "DTV_VIEW_NBR", freq = FALSE)
abline(v = median_val, col = "red", lty = 2)
legend("topright", legend = paste("Median =", round(median_val, 2)), col = "red", lty = 2)

view_data <- df$DTV_VIEW_NBR[df$CUS_FL_DTV == 1]
q1 <- quantile(view_data, 0.25, na.rm = TRUE)
q3 <- quantile(view_data, 0.75, na.rm = TRUE)
iqr <- q3 - q1
outlier_threshold <- q3 + 1.5 * iqr

bins <- c(0, 0.01, 200, 800, outlier_threshold, max(df$DTV_VIEW_NBR, na.rm = TRUE))
labels <- c("Zero user", "Low", "Medium", "High", "Very High")

df$DTV_VIEW_NBR_BIN <- cut(df$DTV_VIEW_NBR, breaks = bins, labels = labels, include.lowest = TRUE)

df$DTV_VIEW_NBR_BIN <- factor(df$DTV_VIEW_NBR_BIN, levels = c(labels, "no user"))
df$DTV_VIEW_NBR_BIN[df$CUS_FL_DTV == 0 | is.na(df$DTV_VIEW_NBR_BIN)] <- "no user"

barplot(table(df$DTV_VIEW_NBR_BIN), col = "lightgreen", border = "black",
        main = "DTV_VIEW_NBR Quartile Distribution with Outlier Bin",
        xlab = "Usage Level", ylab = "Number of Users")


# DTV_NR_VOD_1YR: binning for clustering
dtv_data <- df$DTV_NR_VOD_1YR[df$CUS_FL_DTV == 1 & df$DTV_NR_VOD_1YR >= 0]
median_val <- median(dtv_data, na.rm = TRUE)

hist(dtv_data, breaks = 50, col = "lightblue", border = "black", freq = FALSE,
     main = "Histogram of DTV_NR_VOD_1YR (>= 0)", xlab = "DTV_NR_VOD_1YR")
abline(v = median_val, col = "red", lty = 2)
legend("topright", legend = paste("Median =", round(median_val, 2)), col = "red", lty = 2)

q1 <- quantile(dtv_data, 0.25, na.rm = TRUE)
q3 <- quantile(dtv_data, 0.75, na.rm = TRUE)
iqr <- q3 - q1
outlier_threshold <- q3 + 1.5 * iqr
max_val <- max(dtv_data, na.rm = TRUE)

bins <- c(0, 2, 10, 50, outlier_threshold, max_val)
labels <- c("Zero user", "Low", "Medium", "High", "Very High")

df$DTV_NR_VOD_1YR_BIN <- cut(df$DTV_NR_VOD_1YR, breaks = bins, labels = labels, include.lowest = TRUE)

df$DTV_NR_VOD_1YR_BIN <- factor(df$DTV_NR_VOD_1YR_BIN, levels = c(labels, "no user"))
df$DTV_NR_VOD_1YR_BIN[df$CUS_FL_DTV == 0 | is.na(df$DTV_NR_VOD_1YR_BIN)] <- "no user"

barplot(table(df$DTV_NR_VOD_1YR_BIN), col = "lightgreen", border = "black",
        main = "DTV_NR_VOD_1YR Quartile-Based Bins",
        xlab = "Usage Level", ylab = "Number of Users")


# INTERNET columns: binning for clustering
cols <- c('INT_VOL_DOWN', 'INT_VOL_STREAMING', 'INT_VOL_UP', 'INT_VOL_TOT')
labels <- c("Low", "Medium", "High", "Very High")
all_levels <- c("Zero user", labels, "no user")

for (col in cols) {
  
  data_internet <- df[[col]][df$CUS_FL_INT == 1 & !is.na(df[[col]])]
  
  median_val <- median(data_internet)
  mean_val <- mean(data_internet)
  mode_val <- as.numeric(names(sort(table(data_internet), decreasing = TRUE))[1])
  
  filtered <- df[df$CUS_FL_INT == 1 & df[[col]] > 0 & df[[col]] <= 5000000, ]
  
  # Plot histogram with mean, median, mode lines
  p <- ggplot(filtered, aes_string(x = col)) +
    geom_histogram(aes(y = ..density..), bins = 50, color = "black", fill = "grey") +
    geom_vline(xintercept = mean_val, color = "red", linetype = "dashed", size = 1) +
    geom_vline(xintercept = median_val, color = "purple", linetype = "dashed", size = 1) +
    geom_vline(xintercept = mode_val, color = "blue", linetype = "dashed", size = 1) +
    labs(title = paste("Histogram of", col, "(>0)"), x = col, y = "Density") +
    theme_minimal()
  
  print(p)
  
  # Initialize bin column with 'no user'
  binned_col <- paste0(col, "_BIN")
  df[[binned_col]] <- factor("no user", levels = all_levels, ordered = TRUE)
  
  mask <- df$CUS_FL_INT == 1 & !is.na(df[[col]]) & df[[col]] > 0
  data_pos <- df[[col]][mask]
  
  median_val <- median(data_pos)
  q1 <- quantile(data_pos, 0.25)
  q3 <- quantile(data_pos, 0.75)
  iqr <- q3 - q1
  max_val <- max(data_pos)
  
  bin1 <- max(0.01, median_val * 0.3)
  bin2 <- median_val + median_val * 0.5
  bin3 <- q3 + 1.5 * iqr
  
  raw_bins <- c(bin1, bin2, bin3, max_val + 1)
  bins <- c()
  for (b in raw_bins) {
    if (length(bins) == 0 || b > tail(bins, 1)) {
      bins <- c(bins, b)
    } else {
      bins <- c(bins, tail(bins, 1) + 1e-5)
    }
  }
  breaks <- c(0, bins)
  
  df[[binned_col]][mask] <- cut(data_pos, breaks = breaks, labels = labels, include.lowest = TRUE)
  
  zero_mask <- df$CUS_FL_INT == 1 & df[[col]] == 0
  df[[binned_col]][zero_mask] <- "Zero user"
  
  bin_counts <- table(df[[binned_col]])
  bin_counts <- bin_counts[all_levels]
  bin_counts[is.na(bin_counts)] <- 0
  
  count_df <- data.frame(Bin = names(bin_counts), Count = as.numeric(bin_counts))
  
  p2 <- ggplot(count_df, aes(x = Bin, y = Count)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    geom_text(aes(label = Count), vjust = -0.5) +
    labs(title = paste("Distribution of", col, "Bins"), x = "Bins", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  
  print(p2)
}

rm(filtered)
rm(p)


### Block 07: Prepare binned feature dataframe and summary ----
# Subset dataframe with original and binned features for clustering
cols <- c('MTEL_VOICE_MOU', 'MTEL_TOT_VOL_MB', 'TEL_OUT_MOU', 'DTV_VIEW_NBR', 
          'DTV_NR_VOD_1YR', 'INT_VOL_DOWN', 'INT_VOL_UP', 'INT_VOL_STREAMING', 'INT_VOL_TOT')

cols_bin <- c('MTEL_VOICE_CNT_BIN', 'MTEL_TOT_VOL_MB_BIN', 'TEL_OUT_MOU_BIN', 'DTV_VIEW_NBR_BIN',
              'DTV_NR_VOD_1YR_BIN', 'INT_VOL_DOWN_BIN', 'INT_VOL_UP_BIN', 'INT_VOL_STREAMING_BIN',
              'INT_VOL_TOT_BIN')

selected_cols <- c('CUSTOMERNUMBER', 'CUS_FL_DTV', 'CUS_FL_INT', 'CUS_FL_MTEL', 'CUS_FL_TEL', cols, cols_bin)

df_bin <- df[, selected_cols, drop = FALSE]
head(df_bin, 5)

# Summarize binned columns (count NA, zero user, no user, etc.)
cols <- c(
  'MTEL_VOICE_CNT_BIN', 'MTEL_TOT_VOL_MB_BIN', 'TEL_OUT_MOU_BIN', 'DTV_VIEW_NBR_BIN',
  'DTV_NR_VOD_1YR_BIN', 'INT_VOL_DOWN_BIN', 'INT_VOL_UP_BIN', 'INT_VOL_STREAMING_BIN',
  'INT_VOL_TOT_BIN'
)

summary_list <- list()

for (col in cols) {
  if (!col %in% names(df)) {
    summary_list[[col]] <- data.frame(
      Column = col,
      NaN_total = NA,
      Non_null_total = NA,
      Zero_user_count = NA,
      No_user_count = NA,
      stringsAsFactors = FALSE
    )
    next
  }
  
  na_count <- sum(is.na(df[[col]]))
  non_null_count <- sum(!is.na(df[[col]]))
  zero_user_count <- sum(df[[col]] == 'Zero user', na.rm = TRUE)
  no_user_count <- sum(df[[col]] == 'no user', na.rm = TRUE)
  
  summary_list[[col]] <- data.frame(
    Column = col,
    NaN_total = na_count,
    Non_null_total = non_null_count,
    Zero_user_count = zero_user_count,
    No_user_count = no_user_count,
    stringsAsFactors = FALSE
  )
}

summary_df <- do.call(rbind, summary_list)
print(summary_df, row.names = FALSE)

# Dictionary of binned columns for k-modes by service group
binned_cols_dict <- list(
  MTEL = c('MTEL_VOICE_CNT_BIN', 'MTEL_TOT_VOL_MB_BIN'),
  TEL = c('TEL_OUT_MOU_BIN'),
  INT = c('INT_VOL_DOWN_BIN', 'INT_VOL_STREAMING_BIN', 'INT_VOL_UP_BIN'),
  DTV = c('DTV_VIEW_NBR_BIN')
)


### Block 08: Run K-Modes clustering per service group ----
# Prepare factor columns for binned features
bin_cols <- c('MTEL_VOICE_CNT_BIN', 'MTEL_TOT_VOL_MB_BIN', 
              'TEL_OUT_MOU_BIN', 
              'INT_VOL_DOWN_BIN', 'INT_VOL_STREAMING_BIN', 'INT_VOL_UP_BIN', 
              'DTV_VIEW_NBR_BIN')
df[bin_cols] <- lapply(df[bin_cols], as.factor)

# Prepare data per service group
mtel_data <- dplyr::select(df, MTEL_VOICE_CNT_BIN, MTEL_TOT_VOL_MB_BIN, CUSTOMERNUMBER)
tel_data  <- dplyr::select(df, TEL_OUT_MOU_BIN, CUSTOMERNUMBER)
int_data  <- dplyr::select(df, INT_VOL_DOWN_BIN, INT_VOL_STREAMING_BIN, INT_VOL_UP_BIN, CUSTOMERNUMBER)
dtv_data  <- dplyr::select(df, DTV_VIEW_NBR_BIN, CUSTOMERNUMBER)

# MTEL group (8 clusters)
set.seed(42)
km_mtel <- kmodes(mtel_data[,1:2], modes = 8, iter.max = 20, weighted = FALSE)
mtel_data$Cluster_MTEL <- km_mtel$cluster
df$Cluster_MTEL <- km_mtel$cluster

summary_mtel <- mtel_data %>%
  group_by(Cluster_MTEL) %>%
  summarise(
    MTEL_VOICE_CNT_BIN = names(sort(table(MTEL_VOICE_CNT_BIN), decreasing = TRUE))[1],
    MTEL_TOT_VOL_MB_BIN = names(sort(table(MTEL_TOT_VOL_MB_BIN), decreasing = TRUE))[1]
  )
summary_mtel <- summary_mtel %>% mutate(Group = "MTEL", .before = 1)
print(table(df$Cluster_MTEL))
print(summary_mtel)

# TEL group (6 clusters)
tel_data_factor <- tel_data[, "TEL_OUT_MOU_BIN", drop = FALSE] %>%
  mutate_all(as.factor)
tel_data_factor$dummy <- factor("dummy")
km_tel <- kmodes(tel_data_factor, modes = 6, iter.max = 20, weighted = FALSE)
tel_data$Cluster_TEL <- km_tel$cluster
df$Cluster_TEL <- km_tel$cluster

summary_tel <- tel_data %>%
  group_by(Cluster_TEL) %>%
  summarise(
    TEL_OUT_MOU_BIN = names(sort(table(TEL_OUT_MOU_BIN), decreasing = TRUE))[1]
  ) %>%
  mutate(Group = "TEL", .before = 1)
print(summary_tel)

# INT group (8 clusters)
set.seed(42)
km_int <- kmodes(int_data[,1:3], modes = 8, iter.max = 20, weighted = FALSE)
int_data$Cluster_INT <- km_int$cluster
df$Cluster_INT <- km_int$cluster

summary_int <- int_data %>%
  group_by(Cluster_INT) %>%
  summarise(
    INT_VOL_DOWN_BIN = names(sort(table(INT_VOL_DOWN_BIN), decreasing = TRUE))[1],
    INT_VOL_STREAMING_BIN = names(sort(table(INT_VOL_STREAMING_BIN), decreasing = TRUE))[1],
    INT_VOL_UP_BIN = names(sort(table(INT_VOL_UP_BIN), decreasing = TRUE))[1]
  )
summary_int <- summary_int %>% mutate(Group = "INT", .before = 1)
print(table(df$Cluster_INT))
print(summary_int)

# DTV group (6 clusters)
dtv_data_factor <- dtv_data[, "DTV_VIEW_NBR_BIN", drop = FALSE] %>%
  mutate_all(as.factor)
dtv_data_factor$dummy <- factor("dummy")
set.seed(42)
km_dtv <- kmodes(dtv_data_factor, modes = 6, iter.max = 20, weighted = FALSE)
dtv_data$Cluster_DTV <- km_dtv$cluster
df$Cluster_DTV <- km_dtv$cluster

summary_dtv <- dtv_data %>%
  group_by(Cluster_DTV) %>%
  summarise(
    DTV_VIEW_NBR_BIN = names(sort(table(DTV_VIEW_NBR_BIN), decreasing = TRUE))[1]
  ) %>%
  mutate(Group = "DTV", .before = 1)

print(table(df$Cluster_DTV))
print(summary_dtv)


### Block 09: Map cluster numbers to names and run final K-Modes clustering ----
cluster_name_mapping <- list(
  Cluster_MTEL = c(
    '1' = 'low voice, medium data',
    '2' = 'high voice, high data',
    '3' = 'high voice, low data',
    '4' = 'low voice, low data',
    '5' = 'medium voice, medium data',
    '6' = 'low voice, zero user',
    '7' = 'very high voice, very high data',
    '8' = 'no user'
  ),
  Cluster_INT = c(
    '1' = 'low usage',
    '2' = 'high usage',
    '3' = 'high usage, very high upload',
    '4' = 'medium usage, low streaming',
    '5' = 'very high usage',
    '6' = 'medium usage',
    '7' = 'high usage, medium streaming',
    '8' = 'high usage, very high streaming'
  ),
  Cluster_DTV = c(
    '1' = 'high view',
    '2' = 'very high view',
    '3' = 'medium view',
    '4' = 'low view',
    '5' = 'zero user',
    '6' = 'no user'
  ),
  Cluster_TEL = c(
    '1' = 'zero user',
    '2' = 'low usage',
    '3' = 'no user',
    '4' = 'medium usage',
    '5' = 'high usage',
    '6' = 'very high usage'
  )
)

df$Cluster_MTEL_NAME <- unname(cluster_name_mapping$Cluster_MTEL[as.character(df$Cluster_MTEL)])
df$Cluster_INT_NAME  <- unname(cluster_name_mapping$Cluster_INT[as.character(df$Cluster_INT)])
df$Cluster_DTV_NAME  <- unname(cluster_name_mapping$Cluster_DTV[as.character(df$Cluster_DTV)])
df$Cluster_TEL_NAME  <- unname(cluster_name_mapping$Cluster_TEL[as.character(df$Cluster_TEL)])

name_columns <- c("Cluster_MTEL_NAME", "Cluster_INT_NAME", "Cluster_DTV_NAME", "Cluster_TEL_NAME")
df <- df %>%
  mutate(across(all_of(name_columns), as.factor))

km_data <- df[name_columns]
set.seed(42)
km_final <- kmodes(km_data, modes = 11, iter.max = 30, weighted = FALSE)

df$FINAL_CLUSTER <- km_final$cluster
km_data$FINAL_CLUSTER <-km_final$cluster

print(head(df[, c("FINAL_CLUSTER", name_columns)]))


# Display the result from final K-Modes clustering
cluster_props <- prop.table(table(df$FINAL_CLUSTER))

barplot(cluster_props,
        main = "Cluster Proportions",
        ylab = "Proportion",
        xlab = "Final Cluster",
        col = "steelblue",
        names.arg = names(cluster_props),
        las = 1)

summary_FINAL_CLUSTERS <- df %>%
  group_by(FINAL_CLUSTER) %>%
  summarise(
    Cluster_MTEL_NAME = names(sort(table(Cluster_MTEL_NAME), decreasing = TRUE))[1],
    Cluster_INT_NAME  = names(sort(table(Cluster_INT_NAME), decreasing = TRUE))[1],
    Cluster_DTV_NAME  = names(sort(table(Cluster_DTV_NAME), decreasing = TRUE))[1],
    Cluster_TEL_NAME  = names(sort(table(Cluster_TEL_NAME), decreasing = TRUE))[1],
    .groups = "drop"
  )

print(summary_FINAL_CLUSTERS)


# ---- DEMOGRAPHICS ----
# Replace NA with "Unknown" for categorical variables relevant to customer profile
df$CUS_SEX[is.na(df$CUS_SEX)] <- "Unknown"
df$CUS_ZIP[is.na(df$CUS_ZIP)] <- "Unknown"
df$CUS_LIFESTAGE[is.na(df$CUS_LIFESTAGE)] <- "Unknown"
df$CUS_LIFESTAGE_DETAILS[is.na(df$CUS_LIFESTAGE_DETAILS)] <- "Unknown"
df$LANGUAGE[is.na(df$LANGUAGE)] <- "Unknown"

# Distribution of gender by cluster (percentage)
gender_by_cluster <- df %>%
  group_by(FINAL_CLUSTER, CUS_SEX) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(FINAL_CLUSTER) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup() %>%
  dplyr::select(-count) %>%
  pivot_wider(names_from = CUS_SEX, values_from = percent, values_fill = list(percent = 0))
print(gender_by_cluster)

# Distribution of postal code by cluster (percentage)
zip_by_cluster <- df %>%
  count(FINAL_CLUSTER, CUS_ZIP) %>%
  group_by(FINAL_CLUSTER) %>%
  mutate(percent = n / sum(n) * 100) %>%
  pivot_wider(names_from = CUS_ZIP, values_from = percent, values_fill = list(percent = 0))
print(zip_by_cluster)

# Distribution of lifestage and lifestage details by cluster (percentage)
lifestage_by_cluster <- df %>%
  count(FINAL_CLUSTER, CUS_LIFESTAGE) %>%
  group_by(FINAL_CLUSTER) %>%
  mutate(percent = n / sum(n) * 100)
print(lifestage_by_cluster)

lifestage_details_by_cluster <- df %>%
  count(FINAL_CLUSTER, CUS_LIFESTAGE_DETAILS) %>%
  group_by(FINAL_CLUSTER) %>%
  mutate(percent = n / sum(n) * 100)
print(lifestage_details_by_cluster)

# Visualize lifestage distributions
ggplot(lifestage_by_cluster, aes(x = factor(FINAL_CLUSTER), y = percent, fill = CUS_LIFESTAGE)) +
  geom_bar(stat = "identity") +
  labs(x = "FINAL_CLUSTER", y = "Percentage of Customers", 
       title = "Distribution of Customer Lifestage by Cluster") +
  theme_minimal() +
  theme(legend.position = "right") +
  guides(fill = guide_legend(title = "CUS_LIFESTAGE"))

ggplot(lifestage_details_by_cluster, aes(x = factor(FINAL_CLUSTER), y = percent, fill = CUS_LIFESTAGE_DETAILS)) +
  geom_bar(stat = "identity") +
  labs(x = "FINAL_CLUSTER", y = "Percentage of Customers", 
       title = "Distribution of Customer Lifestage Details by Cluster") +
  theme_minimal() +
  theme(legend.position = "right") +
  guides(fill = guide_legend(title = "CUS_LIFESTAGE_DETAILS"))

# Convert and analyze customer lifetime per cluster
df$CUS_LIFETIME <- as.numeric(df$CUS_LIFETIME)
df$CUS_LIFETIME[is.na(df$CUS_LIFETIME)] <- 0

average_lifetime_by_cluster <- df %>%
  group_by(FINAL_CLUSTER) %>%
  summarise(avg_lifetime = round(mean(CUS_LIFETIME), 2))
print(average_lifetime_by_cluster)

lifetime_summary <- df %>%
  group_by(FINAL_CLUSTER) %>%
  summarise(
    count = n(),
    mean = mean(CUS_LIFETIME),
    sd = sd(CUS_LIFETIME),
    median = median(CUS_LIFETIME),
    IQR = IQR(CUS_LIFETIME),
    min = min(CUS_LIFETIME),
    max = max(CUS_LIFETIME)
  )
print(lifetime_summary)

# Boxplot for customer lifetime distribution by cluster
ggplot(df, aes(x = factor(FINAL_CLUSTER), y = CUS_LIFETIME)) +
  geom_boxplot() +
  labs(title = "Lifetime distribution per cluster", x = "FINAL_CLUSTER", y = "CUS_LIFETIME") +
  theme_minimal()

# Language proportions by cluster
tab <- table(df$FINAL_CLUSTER, df$LANGUAGE)
prop_tab <- prop.table(tab, margin = 1) * 100
language_by_cluster <- as.data.frame.matrix(prop_tab)
print(language_by_cluster)


# Revenue by cluster
# Ensure revenue columns are numeric, fill NAs with 0
df$CUS_VALUE <- as.numeric(as.character(df$CUS_VALUE))
df$CUS_VALUE_REVENUE <- as.numeric(as.character(df$CUS_VALUE_REVENUE))
df$CUS_VALUE[is.na(df$CUS_VALUE)] <- 0
df$CUS_VALUE_REVENUE[is.na(df$CUS_VALUE_REVENUE)] <- 0

# Cluster-level revenue aggregation
value_by_cluster <- df %>%
  group_by(FINAL_CLUSTER) %>%
  summarise(CUS_VALUE_SUM = round(sum(CUS_VALUE), 2))
value_revenue_by_cluster <- df %>%
  group_by(FINAL_CLUSTER) %>%
  summarise(CUS_VALUE_REVENUE_SUM = round(sum(CUS_VALUE_REVENUE), 2))

print("Sum of CUS_VALUE by cluster:")
print(value_by_cluster)
print("Sum of CUS_VALUE_REVENUE by cluster:")
print(value_revenue_by_cluster)
options(scipen=999)

# Revenue visualization by cluster
ggplot(value_revenue_by_cluster, aes(x = factor(FINAL_CLUSTER), y = CUS_VALUE_REVENUE_SUM)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  labs(
    x = "FINAL_CLUSTER",
    y = "Total CUS_VALUE_REVENUE",
    title = "Total Revenue by Cluster"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, color = "black") +
  theme(panel.grid.major.y = element_line(linetype = "dashed", color = "gray80"))

# Clean up
rm(dtv_data_factor)
rm(km_data)
rm(zero_mask)
rm(view_data)
rm(voice_data)
rm(tel_data_factor)


### Block 09: NPS / Satisfaction based on the questionnaire ----
# Select questionnaire columns from df_orig (starting from Q1_1)
start_col <- "Q1_1"
start_index <- which(names(df_orig) == start_col)
questionair <- df_orig[, start_index:ncol(df_orig)]

# Add CUSTOMER NUMBER for joining
questionair <- questionair %>%
  mutate(CUSTOMERNUMBER = df_orig$CUSTOMERNUMBER)

# Ensure FINAL_CLUSTER is integer and no NAs
df <- df %>%
  mutate(FINAL_CLUSTER = ifelse(is.na(FINAL_CLUSTER), -1, FINAL_CLUSTER) %>% as.integer())

# Merge clusters with questionnaire responses
df_combined <- df %>%
  left_join(questionair, by = "CUSTOMERNUMBER")

# Calculate percent of missing answers per cluster/question
missing_by_cluster <- df_combined %>%
  group_by(FINAL_CLUSTER) %>%
  summarise(across(all_of(names(questionair)[names(questionair) != "CUSTOMERNUMBER"]),
                   ~ mean(is.na(.)) * 100, .names = "missing_{col}")) %>%
  mutate(across(where(is.numeric), function(x) round(x, 2)))

# Function to bin Likert-scale responses for satisfaction/NPS
bin_likert_vec <- function(x) {
  ifelse(is.na(x), "Not Answered",
         ifelse(x >= 0 & x <= 6, "Dissatisfied",
                ifelse(x >= 7 & x <= 8, "Satisfied",
                       ifelse(x >= 9 & x <= 10, "Very Happy", "Invalid")
                )
         )
  )
}

# List Likert-scale columns for binned NPS/SAT reporting
likert_columns <- c('Q1_1', 'Q1_2_1', 'Q1_2_2', 'Q1_2_3', 'Q1_2_4', 
                    'Q3_3_1', 'Q4_2_9B', 'Q4_1_1', 'Q4_1_2', 'Q4_1_3', 'Q5_1_1')

df_combined <- df_combined %>%
  mutate(across(all_of(likert_columns), ~ as.numeric(as.character(.)))) %>%
  mutate(across(all_of(likert_columns), bin_likert_vec))

print(head(df_combined[likert_columns]))

# Plot distribution of each NPS/SAT question by cluster
for (question in likert_columns) {
  cluster_counts <- df_combined %>%
    group_by(FINAL_CLUSTER, !!sym(question)) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(FINAL_CLUSTER) %>%
    mutate(percent = count / sum(count) * 100) %>%
    ungroup()
  
  p <- ggplot(cluster_counts, aes(x = factor(FINAL_CLUSTER), y = percent, fill = !!sym(question))) +
    geom_bar(stat = "identity", position = "stack") +
    labs(
      title = paste("Distribution of", question, "Responses by Cluster"),
      x = "FINAL_CLUSTER",
      y = "Percentage",
      fill = paste(question, "Response")
    ) +
    theme_minimal() +
    theme(legend.position = "right")
  
  print(p)
  # Sys.sleep(1) # optional: pause 1 second between plots
}
