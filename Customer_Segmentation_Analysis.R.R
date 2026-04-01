#============================================================
  # Assignment 4 - Statistical Learning in Marketing
  # Student: 6543537
  # Segmentation Analysis for Ixmør
#============================================================

# ---- Load and inspect data ----

dataset <- read.csv("C:/Users/debor/Downloads/Segmentation_data.csv")

str(dataset)
summary(dataset)

# Active variables (cols 6:12): Taste, Fattiness, Salt, Spreadability,
#                                Appearance, Recycling, Bio-content
# Passive variables (cols 2:5): Gender, Age, Education, Area

# ---- Exploratory plots of active variables ----

x <- dataset$Taste
y <- dataset$Fattiness
c <- dataset$Recycling
d <- dataset$Bio.content  

plot(x, y, xlab = "Taste", ylab = "Fattiness",
     xlim = c(1,7), ylim = c(1,7), col = "red", pch = 16,
     main = "Taste & Fattiness")

plot(d, x, xlab = "Bio content", ylab = "Taste",
     xlim = c(1,7), ylim = c(1,7), col = "red", pch = 16,
     main = "Bio content & Taste")

plot(d, y, xlab = "Bio content", ylab = "Fattiness",
     xlim = c(1,7), ylim = c(1,7), col = "red", pch = 16,
     main = "Bio content & Fattiness")

plot(d, c, xlab = "Bio content", ylab = "Recycling",
     xlim = c(1,7), ylim = c(1,7), col = "red", pch = 16,
     main = "Bio content & Recycling")

# 3D plot
library(scatterplot3d)
scatterplot3d(x, y, d, xlab = "Taste", ylab = "Fattiness", zlab = "Bio content",
              xlim = c(1,7), ylim = c(1,7), zlim = c(1,7),
              color = "red", pch = 16,
              main = "Taste, Fattiness & Bio content")

# ============================================================
# STEP 1: HIERARCHICAL CLUSTERING (Ward's method)
# ============================================================

# Standardise active variables
dataset2 <- dataset
dataset2[, 6:12] <- data.frame(scale(dataset[, 6:12]))

# Calculate Euclidean distance
library(cluster)
dataset2.dist <- daisy(dataset2[, c(6:12)])

# --- Run all linkage methods for reference ---
dataset2a.hc <- hclust(dataset2.dist, method = "single")
plot(dataset2a.hc, main = "Single (linkage)")

dataset2b.hc <- hclust(dataset2.dist, method = "complete")
plot(dataset2b.hc, main = "Complete (linkage)")

dataset2c.hc <- hclust(dataset2.dist, method = "average")
plot(dataset2c.hc, main = "Average (linkage)")

dataset2d.hc <- hclust(dataset2.dist, method = "centroid")
plot(dataset2d.hc, main = "Centroid")

# --- Ward's method (main method) ---
dataset2e.hc <- hclust(dataset2.dist, method = "ward.D2")
plot(dataset2e.hc, main = "Ward's Method")

datasetaggloe <- cbind(as.data.frame(dataset2e.hc[1]),
                       as.data.frame(dataset2e.hc[2]))
datasetaggloe
datasetaggloe[1:15, ]
datasetaggloe[(nrow(dataset)-10):(nrow(dataset)-1), ]
diff(datasetaggloe[(nrow(dataset)-10):(nrow(dataset)-1), ]$height)

# --- Scree plots for all methods ---
datasetscreea <- sort(cbind(as.data.frame(dataset2a.hc[1]),
                            as.data.frame(dataset2a.hc[2]))[(nrow(dataset)-15):(nrow(dataset)-1), c(3)],
                      decreasing = TRUE)
plot(datasetscreea, type = "l", col = "red", lwd = 5, xlab = "clusters",
     ylab = "Cluster distance", main = "Scree Plot single linkage", xaxt = "n")
axis(1, at = seq(1, 18, by = 1))

datasetscreeb <- sort(cbind(as.data.frame(dataset2b.hc[1]),
                            as.data.frame(dataset2b.hc[2]))[(nrow(dataset)-15):(nrow(dataset)-1), c(3)],
                      decreasing = TRUE)
plot(datasetscreeb, type = "l", col = "red", lwd = 5, xlab = "clusters",
     ylab = "Cluster distance", main = "Scree Plot complete linkage", xaxt = "n")
axis(1, at = seq(1, 18, by = 1))

datasetscreec <- sort(cbind(as.data.frame(dataset2c.hc[1]),
                            as.data.frame(dataset2c.hc[2]))[(nrow(dataset)-15):(nrow(dataset)-1), c(3)],
                      decreasing = TRUE)
plot(datasetscreec, type = "l", col = "red", lwd = 5, xlab = "clusters",
     ylab = "Cluster distance", main = "Scree Plot average linkage", xaxt = "n")
axis(1, at = seq(1, 18, by = 1))

datasetscreed <- sort(cbind(as.data.frame(dataset2d.hc[1]),
                            as.data.frame(dataset2d.hc[2]))[(nrow(dataset)-15):(nrow(dataset)-1), c(3)],
                      decreasing = TRUE)
plot(datasetscreed, type = "l", col = "red", lwd = 5, xlab = "clusters",
     ylab = "Cluster distance", main = "Scree Plot centroid", xaxt = "n")
axis(1, at = seq(1, 18, by = 1))

# --- Scree plot Ward's method (KEY PLOT - use to determine number of clusters) ---
datasetscreee <- sort(datasetaggloe[(nrow(dataset)-15):(nrow(dataset)-1), c(3)],
                      decreasing = TRUE)
plot(datasetscreee, type = "l", col = "red", lwd = 5, xlab = "clusters",
     ylab = "Cluster distance", main = "Scree Plot Ward's method", xaxt = "n")
axis(1, at = seq(1, 18, by = 1))

# --- Dendrograms for different k ---

plot(dataset2e.hc, main = "Cluster Dendrogram n = 4")
rect.hclust(dataset2e.hc, k = 4, border = "red")

plot(dataset2e.hc, main = "Cluster Dendrogram n = 2")
rect.hclust(dataset2e.hc, k = 2, border = "red")

# ============================================================
# WARD'S: 2-CLUSTER SOLUTION
# ============================================================

seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

dataset2e.hc.segment2 <- cutree(dataset2e.hc, k = 2)
table(dataset2e.hc.segment2)

dataset2e2.hc.means <- seg.summ(dataset2[, c(6:12)], dataset2e.hc.segment2)
dataset2e2.hc.means  # cluster centroids (standardised) - used later for K-means start

# ANOVA - 2 clusters
clusmember <- as.factor(dataset2e.hc.segment2)
dataset2eaovbase <- cbind(clusmember, dataset2[, c(6:12)])

active_vars <- c("Taste", "Fattiness", "Salt", "Spreadability", 
                 "Appearance", "Recycling", "Bio.content")

anova_summary <- do.call(rbind, lapply(active_vars, function(var) {
  model <- aov(dataset2eaovbase[[var]] ~ clusmember, data = dataset2eaovbase)
  f_val <- summary(model)[[1]]$`F value`[1]
  p_val <- summary(model)[[1]]$`Pr(>F)`[1]
  data.frame(Variable = var, F_value = round(f_val, 2), P_value = round(p_val, 4))
}))

print(anova_summary)

# Tukey HSD - 2 clusters
tukey_summary <- do.call(rbind, lapply(active_vars, function(var) {
  model <- aov(dataset2eaovbase[[var]] ~ clusmember, data = dataset2eaovbase)
  tukey <- TukeyHSD(model)$clusmember
  data.frame(Variable = var, 
             Diff = round(tukey[,"diff"], 3),
             P_adj = round(tukey[,"p adj"], 4))
}))

print(tukey_summary)

# Chi-squared tests on passive variables - 2 clusters
# Gender
freq.gender <- table(clusmember, dataset2[, c(2)])
freq.gender
chisq.test(freq.gender)

# Age (calculate weighted mean age per cluster)
freq.age <- table(clusmember, dataset2[, c(3)])
freq.age
sum(as.numeric(colnames(freq.age)) * freq.age[1, ]) / sum(freq.age[1, ])  # cluster 1 mean age
sum(as.numeric(colnames(freq.age)) * freq.age[2, ]) / sum(freq.age[2, ])  # cluster 2 mean age
chisq.test(freq.age)

# Education
freq.edu <- table(clusmember, dataset2[, c(4)])
freq.edu
chisq.test(freq.edu)

# Area
freq.area <- table(clusmember, dataset2[, c(5)])
freq.area
chisq.test(freq.area)

# PCA plot - 2 clusters (Ward's)
pca <- prcomp(dataset2[, c(6:12)])
pca_scores <- predict(pca, dataset2[, c(6:12)])
plot(pca_scores[, 1], pca_scores[, 2], col = dataset2e.hc.segment2, pch = 16, cex = 2,
     xlab = "PC1", ylab = "PC2", main = "Cluster Plot PCA - Ward's 2 clusters")

# ============================================================
# WARD'S: 4-CLUSTER SOLUTION
# ============================================================

dataset2e.hc.segment4 <- cutree(dataset2e.hc, k = 4)
table(dataset2e.hc.segment4)

dataset2e4.hc.means <- seg.summ(dataset2[, c(6:12)], dataset2e.hc.segment4)
print(dataset2e4.hc.means)  # cluster centroids - used later for K-means start

# ANOVA - 4 clusters
clusmember <- as.factor(dataset2e.hc.segment4)
dataset2eaovbase <- cbind(clusmember, dataset2[, c(6:12)])

anova_summary4 <- do.call(rbind, lapply(active_vars, function(var) {
  model <- aov(dataset2eaovbase[[var]] ~ clusmember, data = dataset2eaovbase)
  f_val <- summary(model)[[1]]$`F value`[1]
  p_val <- summary(model)[[1]]$`Pr(>F)`[1]
  data.frame(Variable = var, F_value = round(f_val, 2), P_value = round(p_val, 4))
}))

print(anova_summary4)

# Tukey HSD - 4 clusters
tukey_summary4 <- do.call(rbind, lapply(active_vars, function(var) {
  model <- aov(dataset2eaovbase[[var]] ~ clusmember, 
               data = dataset2eaovbase)
  tukey <- TukeyHSD(model)$clusmember
  data.frame(Comparison = rownames(tukey),
             Variable = var,
             Diff = round(tukey[,"diff"], 3),
             P_adj = round(tukey[,"p adj"], 4))
}))

print(tukey_summary4)

# Chi-squared tests on passive variables - 4 clusters
# Gender
freq.gender <- table(clusmember, dataset2[, c(2)])
freq.gender
chisq.test(freq.gender)

# Age
freq.age <- table(clusmember, dataset2[, c(3)])
freq.age
sum(as.numeric(colnames(freq.age)) * freq.age[1, ]) / sum(freq.age[1, ])  # cluster 1
sum(as.numeric(colnames(freq.age)) * freq.age[2, ]) / sum(freq.age[2, ])  # cluster 2
sum(as.numeric(colnames(freq.age)) * freq.age[3, ]) / sum(freq.age[3, ])  # cluster 3
sum(as.numeric(colnames(freq.age)) * freq.age[4, ]) / sum(freq.age[4, ])  # cluster 4
chisq.test(freq.age)

# Education
freq.edu <- table(clusmember, dataset2[, c(4)])
freq.edu
chisq.test(freq.edu)

# Area
freq.area <- table(clusmember, dataset2[, c(5)])
freq.area
chisq.test(freq.area)

# PCA plot - 4 clusters (Ward's)
pca_scores <- predict(pca, dataset2[, c(6:12)])
plot(pca_scores[, 1], pca_scores[, 2], col = dataset2e.hc.segment4, pch = 16, cex = 2,
     xlab = "PC1", ylab = "PC2", main = "Cluster Plot PCA - Ward's 4 clusters")

# ============================================================
# STEP 2: K-MEANS CLUSTERING (standalone, random start)
# ============================================================

set.seed(6543537)

# --- K-means: 2 clusters ---
dataset2.k2 <- kmeans(dataset2[, c(6:12)], centers = 2)
print(dataset2.k2)
kmeans2_means <- as.data.frame(round(dataset2.k2$centers, 3))
print(kmeans2_means)

boxplot(dataset2$Fattiness ~ dataset2.k2$cluster, ylab = "Fattiness", xlab = "Cluster",
        main = "K-means 2: Fattiness")
boxplot(dataset2$Taste     ~ dataset2.k2$cluster, ylab = "Taste",     xlab = "Cluster",
        main = "K-means 2: Taste")
boxplot(dataset2$Recycling ~ dataset2.k2$cluster, ylab = "Recycling", xlab = "Cluster",
        main = "K-means 2: Recycling")
boxplot(dataset2$Bio.content ~ dataset2.k2$cluster, ylab = "Bio content", xlab = "Cluster",
        main = "K-means 2: Bio content")

dataset2.k2.segment <- dataset2.k2$cluster
plot(pca_scores[, 1], pca_scores[, 2], col = dataset2.k2.segment, pch = 16, cex = 2,
     xlab = "PC1", ylab = "PC2", main = "Cluster Plot PCA - K-means 2 clusters")

# --- K-means: 4 clusters ---
set.seed(6543537)
dataset2.k4 <- kmeans(dataset2[, c(6:12)], centers = 4)
table(dataset2.k4$cluster)
seg.summ(dataset2[, c(6:12)], dataset2.k4$cluster)
kmeans4_means <- as.data.frame(round(dataset2.k4$centers, 3))
print(kmeans4_means)

boxplot(dataset2$Fattiness   ~ dataset2.k4$cluster, ylab = "Fattiness",   xlab = "Cluster",
        main = "K-means 4: Fattiness")
boxplot(dataset2$Taste       ~ dataset2.k4$cluster, ylab = "Taste",       xlab = "Cluster",
        main = "K-means 4: Taste")
boxplot(dataset2$Recycling   ~ dataset2.k4$cluster, ylab = "Recycling",   xlab = "Cluster",
        main = "K-means 4: Recycling")
boxplot(dataset2$Bio.content ~ dataset2.k4$cluster, ylab = "Bio content", xlab = "Cluster",
        main = "K-means 4: Bio content")

dataset2.k4.segment <- dataset2.k4$cluster
plot(pca_scores[, 1], pca_scores[, 2], col = dataset2.k4.segment, pch = 16, cex = 2,
     xlab = "PC1", ylab = "PC2", main = "Cluster Plot PCA - K-means 4 clusters")

# ============================================================
# STEP 3: COMBINED (Ward's centroids → K-means start)
# ============================================================

# --- Combined: 2 clusters (Ward centroids as starting point) ---
kmeanstart2 <- dataset2e2.hc.means[, c(2:8)]
dataset2.combined2 <- kmeans(dataset2[, c(6:12)], kmeanstart2)
table(dataset2.combined2$cluster)
seg.summ(dataset2[, c(6:12)], dataset2.combined2$cluster)
combined2_means <- as.data.frame(round(dataset2.combined2$centers, 3))
print(combined2_means)

boxplot(dataset2$Fattiness   ~ dataset2.combined2$cluster, ylab = "Fattiness",   xlab = "Cluster",
        main = "Combined 2: Fattiness")
boxplot(dataset2$Taste       ~ dataset2.combined2$cluster, ylab = "Taste",       xlab = "Cluster",
        main = "Combined 2: Taste")
boxplot(dataset2$Recycling   ~ dataset2.combined2$cluster, ylab = "Recycling",   xlab = "Cluster",
        main = "Combined 2: Recycling")
boxplot(dataset2$Bio.content ~ dataset2.combined2$cluster, ylab = "Bio content", xlab = "Cluster",
        main = "Combined 2: Bio content")

dataset2.combined2.segment <- dataset2.combined2$cluster
plot(pca_scores[, 1], pca_scores[, 2], col = dataset2.combined2.segment, pch = 16, cex = 2,
     xlab = "PC1", ylab = "PC2", main = "Cluster Plot PCA - Combined 2 clusters")

# ANOVA - Combined 2
clusmember <- as.factor(dataset2.combined2.segment)
dataset2eaovbase <- cbind(clusmember, dataset2[, c(6:12)])

anova_summary_c2 <- do.call(rbind, lapply(active_vars, function(var) {
  model <- aov(dataset2eaovbase[[var]] ~ clusmember,
               data = dataset2eaovbase)
  f_val <- summary(model)[[1]]$`F value`[1]
  p_val <- summary(model)[[1]]$`Pr(>F)`[1]
  data.frame(Variable = var,
             F_value = round(f_val, 2),
             P_value = round(p_val, 4))
}))
print(anova_summary_c2)

anova_summary_c2$P_value <- ifelse(anova_summary_c2$P_value < 0.001, 
                                   "< 0.001", 
                                   as.character(anova_summary_c2$P_value))
print(anova_summary_c2)

tukey_nonsig_c2 <- do.call(rbind, lapply(active_vars, function(var) {
  model <- aov(dataset2eaovbase[[var]] ~ clusmember,
               data = dataset2eaovbase)
  tukey <- TukeyHSD(model)$clusmember
  data.frame(Comparison = rownames(tukey),
             Variable = var,
             Diff = round(tukey[,"diff"], 3),
             P_adj = round(tukey[,"p adj"], 4))
}))
print(tukey_nonsig_c2)
tukey_nonsig_c2$P_adj <- ifelse(tukey_nonsig_c2$P_adj < 0.001, "< 0.001", as.character(tukey_nonsig_c2$P_adj))
print(tukey_nonsig_c2)

# Passive variables - Combined 2
freq.gender <- table(clusmember, dataset2[, c(2)]); freq.gender; chisq.test(freq.gender)

freq.age <- table(clusmember, dataset2[, c(3)])
freq.age
sum(as.numeric(colnames(freq.age)) * freq.age[1, ]) / sum(freq.age[1, ])
sum(as.numeric(colnames(freq.age)) * freq.age[2, ]) / sum(freq.age[2, ])
chisq.test(freq.age)

freq.edu  <- table(clusmember, dataset2[, c(4)]); freq.edu;  chisq.test(freq.edu)
freq.area <- table(clusmember, dataset2[, c(5)]); freq.area; chisq.test(freq.area)

# --- Combined: 4 clusters (Ward centroids as starting point) ---
kmeanstart4 <- dataset2e4.hc.means[, c(2:8)]
dataset2.combined4 <- kmeans(dataset2[, c(6:12)], kmeanstart4)
table(dataset2.combined4$cluster)
seg.summ(dataset2[, c(6:12)], dataset2.combined4$cluster)
combined4_means <- as.data.frame(round(dataset2.combined4$centers, 3))
print(combined4_means)

boxplot(dataset2$Fattiness   ~ dataset2.combined4$cluster, ylab = "Fattiness",   xlab = "Cluster",
        main = "Combined 4: Fattiness")
boxplot(dataset2$Taste       ~ dataset2.combined4$cluster, ylab = "Taste",       xlab = "Cluster",
        main = "Combined 4: Taste")
boxplot(dataset2$Recycling   ~ dataset2.combined4$cluster, ylab = "Recycling",   xlab = "Cluster",
        main = "Combined 4: Recycling")
boxplot(dataset2$Bio.content ~ dataset2.combined4$cluster, ylab = "Bio content", xlab = "Cluster",
        main = "Combined 4: Bio content")

dataset2.combined4.segment <- dataset2.combined4$cluster
plot(pca_scores[, 1], pca_scores[, 2], col = dataset2.combined4.segment, pch = 16, cex = 2,
     xlab = "PC1", ylab = "PC2", main = "Cluster Plot PCA - Combined 4 clusters")

# ANOVA - Combined 4
clusmember <- as.factor(dataset2.combined4.segment)
dataset2eaovbase <- cbind(clusmember, dataset2[, c(6:12)])

anova_summary_c4 <- do.call(rbind, lapply(active_vars, function(var) {
  model <- aov(dataset2eaovbase[[var]] ~ clusmember,
               data = dataset2eaovbase)
  f_val <- summary(model)[[1]]$`F value`[1]
  p_val <- summary(model)[[1]]$`Pr(>F)`[1]
  data.frame(Variable = var,
             F_value = round(f_val, 2),
             P_value = round(p_val, 4))
}))
print(anova_summary_c4)

tukey_summary_c4 <- do.call(rbind, lapply(active_vars, function(var) {
  model <- aov(dataset2eaovbase[[var]] ~ clusmember,
               data = dataset2eaovbase)
  tukey <- TukeyHSD(model)$clusmember
  data.frame(Comparison = rownames(tukey),
             Variable = var,
             Diff = round(tukey[,"diff"], 3),
             P_adj = round(tukey[,"p adj"], 4))
}))
tukey_nonsig_c4 <- tukey_summary_c4[tukey_summary_c4$P_adj > 0.05, ]
print(tukey_nonsig_c4)

# Passive variables - Combined 4
freq.gender <- table(clusmember, dataset2[, c(2)]); freq.gender; chisq.test(freq.gender)

freq.age <- table(clusmember, dataset2[, c(3)])
freq.age
sum(as.numeric(colnames(freq.age)) * freq.age[1, ]) / sum(freq.age[1, ])
sum(as.numeric(colnames(freq.age)) * freq.age[2, ]) / sum(freq.age[2, ])
sum(as.numeric(colnames(freq.age)) * freq.age[3, ]) / sum(freq.age[3, ])
sum(as.numeric(colnames(freq.age)) * freq.age[4, ]) / sum(freq.age[4, ])
chisq.test(freq.age)

freq.edu  <- table(clusmember, dataset2[, c(4)]); freq.edu;  chisq.test(freq.edu)
freq.area <- table(clusmember, dataset2[, c(5)]); freq.area; chisq.test(freq.area)

# ============================================================
# STEP 4: MODEL-BASED CLUSTERING (Gaussian Mixture Models)
# ============================================================

library(mclust)
library(RColorBrewer)

# Density estimate
mod4 <- densityMclust(dataset2[, c(6:12)])
summary(mod4)
plot(mod4, what = "density", type = "persp", col = brewer.pal(10, "Spectral"))

# Check all solutions up to 9 clusters
mclustBIC(dataset2[, c(6:12)])

# Fit top models based on BIC results
dataset2.mc2 <- Mclust(dataset2[, c(6:12)], G = 2, modelName = "EEV")
summary(dataset2.mc2)

dataset2.mc3 <- Mclust(dataset2[, c(6:12)], G = 3, modelName = "EEV")
summary(dataset2.mc3)

# Compare BIC
BIC(dataset2.mc2, dataset2.mc3)

# Model summary table
mclust_summary <- data.frame(
  Model = c("EEV 2", "EEV 3"),
  Components = c(2, 3),
  BIC = c(-9378.09, -9383.33),
  ICL = c(-9541.73, -9610.86)
)
print(mclust_summary)

# Cluster means tables
mc2_means <- as.data.frame(round(seg.summ(dataset2[, c(6:12)], 
                                          dataset2.mc2$class)[,-1], 3))
print(mc2_means)

mc3_means <- as.data.frame(round(seg.summ(dataset2[, c(6:12)], 
                                          dataset2.mc3$class)[,-1], 3))
print(mc3_means)

# Cluster sizes as % of total
round(table(dataset2.mc2$class) / nrow(dataset2) * 100, 1)
round(table(dataset2.mc3$class) / nrow(dataset2) * 100, 1)

# PCA plot - 2-cluster model-based
dataset2.mc2.segment <- dataset2.mc2$class
plot(pca_scores[, 1], pca_scores[, 2], col = dataset2.mc2.segment, pch = 16, cex = 2,
     xlab = "PC1", ylab = "PC2", main = "Cluster Plot PCA - Model-based 2 clusters")

# PCA plot - 3-cluster model-based
dataset2.mc3.segment <- dataset2.mc3$class
plot(pca_scores[, 1], pca_scores[, 2], col = dataset2.mc3.segment, pch = 16, cex = 2,
     xlab = "PC1", ylab = "PC2", main = "Cluster Plot PCA - Model-based 3 clusters")
# ============================================================
# STEP 5: CLUSTER SIZE SUMMARY (for ~40% check)
# ============================================================

# Check which clusters in combined 4-cluster solution match MQ personas
# and calculate their combined share of the market

cluster_sizes_combined4 <- table(dataset2.combined4.segment)

cluster_summary <- data.frame(
  Cluster = c(1, 2, 3, 4),
  N = as.vector(cluster_sizes_combined4),
  Percentage = as.vector(round(cluster_sizes_combined4 / 
                                 sum(cluster_sizes_combined4) * 100, 1)),
  Profile = c("Sustainability segment", "Urban foodie segment", 
              "Traditional/unengaged", "Indulgent")
)
print(cluster_summary)

# ============================================================
# END OF ANALYSIS
# ============================================================

