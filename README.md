📊 Telecom Customer Segmentation & Satisfaction Analysis

Behavioral segmentation of telecom customers based on real usage data and demographics, combined with NPS/satisfaction survey analysis. The workflow includes data cleaning, feature binning, K-Modes clustering per service, final integrated clustering, demographic profiling, and satisfaction analysis.

Written entirely in R using dplyr, tidyr, ggplot2, klaR, corrplot, stringr, and readr.

📁 Project Structure
data/           – Digital_2015_2016.RData (raw telecom data)
output/         – processed datasets, plots, cluster summaries
src/            – R scripts: analysis, clustering, NPS processing
README.md       – project documentation

🔢 Block 01: Load libraries & data
Load required packages: dplyr, tidyr, ggplot2, klaR, corrplot, stringr, readr
Remove all objects from environment (rm(list = ls()))
Load dataset Digital_2015_2016.RData → df
Inspect dimensions
load("Digital_2015_2016.RData")
df <- dd
df_orig <- df
dim(df)

🔢 Block 02: Data Cleaning
Remove rows with all key service flags missing (CUS_FL_MTEL, CUS_FL_INT, CUS_FL_DTV, CUS_FL_TEL)
Keep latest record per customer
Filter out business customers (CUS_SEX != "Business")
Extract year/month/quarter variables from MONTH_CODE

🔢 Block 03: Create column groups per service
Identify DTV, INT, MTEL, TEL columns using prefixes
Check datatypes for active vs inactive service users

🔢 Block 04: Impute missing values for active users
Replace NAs with 0 for active service users (per service flag)
Keep only numeric features
Add time variables (Year, MONTH, QUARTER, Year_Quarter)
Summarize numeric columns by service group and flag:
NaN (flag=0), NaN (flag=1), Zero values (flag=1), Non-null >0 (flag=1)

🔢 Block 05: Correlation & Outlier Check
Correlation matrix for main usage features (MTEL_VOICE_MOU, MTEL_TOT_VOL_MB, TEL_OUT_MOU, DTV_VIEW_NBR, etc.)
Boxplots for key features to visualize outliers

🔢 Block 06: Feature binning
Binning based on usage distribution and business logic:
MTEL: MTEL_VOICE_CNT_BIN, MTEL_TOT_VOL_MB_BIN
TEL: TEL_OUT_MOU_BIN
DTV: DTV_VIEW_NBR_BIN, DTV_NR_VOD_1YR_BIN
INT: INT_VOL_DOWN_BIN, INT_VOL_STREAMING_BIN, INT_VOL_UP_BIN, INT_VOL_TOT_BIN
Levels: Zero user, Low, Medium, High, Very High, no user
Plots: histograms and barplots per bin

🔢 Block 07: Prepare binned dataframe
Combine original and binned features for clustering
Summarize binned features (NA count, zero user, no user)
Dictionary of binned columns per service group for K-Modes

🔢 Block 08: K-Modes clustering per service group
MTEL (8 clusters), TEL (6 clusters), INT (8 clusters), DTV (6 clusters)
Assign clusters to df
Summarize cluster profiles per service group

🔢 Block 09: Final K-Modes clustering
Map initial service clusters to descriptive names
Final K-Modes clustering (modes = 11) using service cluster names
Assign FINAL_CLUSTER and summarize cluster profiles
Visualize cluster proportions

🔢 Block 10: Demographics Analysis
Replace NAs with "Unknown" for CUS_SEX, CUS_ZIP, CUS_LIFESTAGE, CUS_LIFESTAGE_DETAILS, LANGUAGE
Gender, lifestage, lifestage details, language distribution per cluster
Customer lifetime and revenue by cluster
Visualizations:
Lifestage distributions
Lifetime boxplots
Total revenue per cluster

🔢 Block 11: NPS / Customer Satisfaction
Select survey columns (Q1_1 … Q5_1_1) from df_orig
Merge FINAL_CLUSTER with survey data
Bin Likert-scale responses into Dissatisfied, Satisfied, Very Happy, Not Answered
Visualize distribution of each survey question by cluster

⚠️ Limitations
Missing values: some features >60–70% missing
Duplicates handled by keeping latest snapshot
Outliers handled via binning
Temporal changes not captured; snapshot only

✅ Conclusion

Behavioral segmentation identifies 11 meaningful clusters
K-Modes clustering applied per service and integrated into final clusters
Demographics and NPS analysis provide actionable insights for marketing and product targeting
Methodology can be extended with predictive modeling or automation

📚 Dataset
Internal telecom usage logs and surveys (Digital_2015_2016.RData)
