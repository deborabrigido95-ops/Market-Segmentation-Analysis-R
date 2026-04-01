# Market Segmentation & Consumer Profiling

## Project Overview
This project identifies distinct consumer segments for organic food products using a survey dataset of 500 respondents. The analysis validates market potential for specific consumer profiles, such as "foodies" and "sustainability-oriented" shoppers.

## Methodology
* **Hierarchical Clustering:** Used Ward's Method to determine the optimal number of clusters (k=2 and k=4) through dendrograms and scree plots.
* **K-Means Clustering:** Applied to refine segment membership and test the stability of the solution.
* **Segment Validation:** * **ANOVA & Tukey HSD:** Used to identify which product attributes (Taste, Bio-content, Recycling) significantly differentiate the groups.
    * **Chi-Squared Tests:** Employed to profile segments using demographic variables like Gender, Age, and Area.
* **Visualization:** Created PCA plots and 3D scatterplots to illustrate cluster separation.

## Key Findings
* Identified a high-potential "Sustainability" segment, validating the brand manager's hypothesis.
* Profiled segments to provide actionable marketing recommendations based on taste vs. environmental consciousness.

## Tech Stack
* **Language:** R
* **Main Libraries:** `cluster`, `scatterplot3d`, `stats`

