📊 Telecom Customer Segmentation & Satisfaction Analysis
Behavioral segmentation of telecom customers based on real usage data (CallMinutes, SMSCount, DataUsage) and demographic variables (Gender, Lifestage, PlanType). Includes preprocessing, KMeans/K-Modes clustering, demographic profiling, NPS survey analysis, and marketing recommendations.
The project is fully written in Python (pandas, numpy, sklearn, matplotlib, seaborn) and structured for reproducibility.

📁 Project Structure
data/           – raw input data (usage logs, demographic info, survey responses)
output/         – processed datasets, plots, cluster visualizations
src/            – core scripts for preprocessing, clustering, analysis
README.md       – project documentation

🔢 Block 1: Load required packages and dataset
📄 src/analysis.py
Load necessary libraries (pandas, numpy, sklearn, matplotlib, seaborn)
Load usage data, demographics, and survey responses
Inspect missing values and duplicates

import pandas as pd
import numpy as np
from sklearn.preprocessing import LabelEncoder, StandardScaler
from sklearn.cluster import KMeans
import matplotlib.pyplot as plt
import seaborn as sns


📥 Input files:
telecom_usage.csv – call, SMS, and data usage
demographics.csv – gender, age, lifestage, plan type
survey_responses.csv – NPS and service feedback

✅ Ensures a clean starting point for preprocessing and clustering.

🔢 Block 2: Data preprocessing

📄 src/analysis.py
Handle missing values (imputation or removal depending on feature)
Drop duplicate records; keep latest usage snapshot per customer
Convert categorical variables to numeric or factor encodings (Gender, PlanType)
Scale continuous usage variables (CallMinutes, SMSCount, DataUsage)

df['Gender'] = df['Gender'].map({'Male': 1, 'Female': 2})
df['PlanType'] = LabelEncoder().fit_transform(df['PlanType'])
scaler = StandardScaler()
df[['CallMinutes', 'SMSCount', 'DataUsage']] = scaler.fit_transform(df[['CallMinutes', 'SMSCount', 'DataUsage']])


📝 Preprocessing addresses missing data, duplicates, and outliers for robust clustering.

🔢 Block 3: Customer segmentation with K-Modes/KMeans

📄 src/segmentation.py
Apply K-Means or K-Modes clustering on behavioral and demographic features
Optimal number of clusters selected via silhouette score or domain knowledge
Resulting 11 clusters represent distinct usage and lifestyle segments

📊 Outputs:
clustered_customers.csv – processed dataset with cluster assignments
Visualizations: distribution of usage and demographic variables per cluster

💡 Example clusters:
High usage, multi-product households
Medium usage
Zero/low usage

🔢 Block 4: Demographic profiling

📄 src/analysis.py

Analyze gender distribution per cluster: 66–75% male, cluster 7 highest male share (75%)
Lifestage distribution (Families, Mediors/Seniors, Young adults, Soho, Unknown)
Subcategories: Families (-6, 6-12, 12-18, 18+), Seniors, Medior, Young adults

🖼️ Figures:

Figure 13: Lifestage proportion per cluster
Figure 14: Detailed lifestage subcategories per cluster

💡 Insight: clusters are reasonably balanced across gender and language; lifestage profiles inform marketing and product targeting.

🔢 Block 5: Customer satisfaction analysis (NPS/Survey)

📄 src/survey_analysis.py

Aggregate survey responses per cluster
Analyze Net Promoter Score (NPS) and service-specific satisfaction: internet, TV, mobile, fixed-line
Identify risk segments and areas for improvement

🖼️ Figures:

Figure 15–20: Distribution of survey responses per cluster and per service
Key insights:
Majority are “Satisfied” or “Very Happy”
High proportion of “Not Answered” for phone services
Overall moderate satisfaction, no extreme negative clusters
Price/quality dissatisfaction notable across most clusters

🔢 Block 6: Marketing recommendations

📄 src/marketing.py

Focus on behavior-based targeting rather than generic demographic segmentation
Key actionable segments:
High usage multi-product households – up-sell and cross-sell opportunities
Zero users – reactivation campaigns
Low satisfaction segments – retention campaigns

💡 Leverage lifestage and usage profiles for personalized offers and communications.

⚠️ Limitations

Missing values: some survey and usage variables >60–70% missing
Duplicates: multiple monthly records; only latest snapshot used
Outliers: extreme usage values handled via binning
Results reflect the current snapshot; temporal changes are not captured

✅ Conclusion

Behavioral segmentation based on usage data identifies 11 meaningful customer clusters
Provides reliable basis for marketing, product targeting, and customer satisfaction strategies
Segments clearly differentiate low, medium, and high usage profiles
Methodology demonstrates practical application of data science in telecom sector
Approach can be extended with predictive modeling or automation for higher business value

📚 Dataset Sources

Internal telecom usage logs, demographic data, and customer surveys
