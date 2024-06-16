## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
library(mclust)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table) 
library(Rsubread)
library(lcmm)
library(latrend)
library(kableExtra)
library(mixAK)
library(coda)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
sc1 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc1.csv")
sc2 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc2.csv")
sc3 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc3.csv")
sc4 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc4.csv")
sc5 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc5.csv")
sc6 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc6.csv")
sc7 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc7.csv")
sc8 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc8.csv")
sc9 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc9.csv")

# Function to clean SampleID column
csi <- function(df) {
  df$SampleID <- as.character(df$SampleID)      # Ensure SampleID is a character
  df$SampleID <- gsub("\\D", "", df$SampleID)   # Remove non-numeric characters
  df$SampleID <- as.numeric(df$SampleID)        # Convert to numeric
  return(df)
}

sc1 <- csi(read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc1.csv"))
sc2 <- csi(read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc2.csv"))
sc3 <- csi(read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc3.csv"))
sc4 <- csi(read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc4.csv"))
sc5 <- csi(read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc5.csv"))
sc6 <- csi(read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc6.csv"))
sc7 <- csi(read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc7.csv"))
sc8 <- csi(read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc8.csv"))
sc9 <- csi(read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc9.csv"))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# data
data <- c(8, 6, 5, 13, 2, 8, 6, 13, 6, 7, 4, 5, 4, 3, 2, 4, 2, 3, 11, 
          5, 9, 13, 5, 4, 4, 13, 7, 10, 12, 9, 2, 6, 9, 5, 9, 7, 3, 11, 
          11, 5, 6, 9, 13, 13, 8, 13, 4, 9, 11, 3, 5, 8, 8, 9, 8, 11, 5, 
          5, 9, 10, 8, 6, 4, 9, 4, 5, 8, 6, 8, 13, 10, 3, 8, 2, 8, 3, 8, 
          11, 11, 11, 9, 3, 8, 11, 3, 10, 7, 4, 3, 5, 2, 8, 6, 11, 10, 12, 3, 8, 2, 9)

length <- length(data)

# maximum number of exchanges
max_exchanges <- max(data)

# maximum number of exchanges + 2
max_exchange_vector <- rep(max_exchanges + 2, length)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# deuterium content by max exchanges
sc1$HDX_transformed <- (sc1$HDX/max_exchange_vector)
sc2$HDX_transformed <- (sc2$HDX/max_exchange_vector)
sc3$HDX_transformed <- (sc3$HDX/max_exchange_vector)
sc4$HDX_transformed <- (sc4$HDX/max_exchange_vector)
sc5$HDX_transformed <- (sc5$HDX/max_exchange_vector)
sc6$HDX_transformed <- (sc6$HDX/max_exchange_vector)
sc7$HDX_transformed <- (sc7$HDX/max_exchange_vector)
sc8$HDX_transformed <- (sc8$HDX/max_exchange_vector)
sc9$HDX_transformed <- (sc9$HDX/max_exchange_vector)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
sc1$Time <- as.numeric(sc1$Time)
sc2$Time <- as.numeric(sc2$Time)
sc3$Time <- as.numeric(sc3$Time)
sc4$Time <- as.numeric(sc4$Time)
sc5$Time <- as.numeric(sc5$Time)
sc6$Time <- as.numeric(sc6$Time)
sc7$Time <- as.numeric(sc7$Time)
sc8$Time <- as.numeric(sc8$Time)
sc9$Time <- as.numeric(sc9$Time)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
sc1$Time[sc1$Time == 1] <- 0
sc2$Time[sc2$Time == 1] <- 0
sc3$Time[sc3$Time == 1] <- 0
sc4$Time[sc4$Time == 1] <- 0
sc5$Time[sc5$Time == 1] <- 0
sc6$Time[sc6$Time == 1] <- 0
sc7$Time[sc7$Time == 1] <- 0
sc8$Time[sc8$Time == 1] <- 0
sc9$Time[sc9$Time == 1] <- 0


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
sc1$Time <- as.factor(sc1$Time)
sc2$Time <- as.factor(sc2$Time)
sc3$Time <- as.factor(sc3$Time)
sc4$Time <- as.factor(sc4$Time)
sc5$Time <- as.factor(sc5$Time)
sc6$Time <- as.factor(sc6$Time)
sc7$Time <- as.factor(sc7$Time)
sc8$Time <- as.factor(sc8$Time)
sc9$Time <- as.factor(sc9$Time)


## ------------------------------------------------------------------------------------------------------------------------------------
tmp1 <- rep(data, each = 11)
tmp2 <- rep(data, each = 6)
tmp3 <- rep(data, each = 4)

sc1$length <- rep(tmp1, 10)
sc2$length <- rep(tmp1, 5)
sc3$length <- rep(tmp1, 3)

sc4$length <- rep(tmp2, 10)
sc5$length <- rep(tmp2, 5)
sc6$length <- rep(tmp2, 3)

sc7$length <- rep(tmp3, 10)
sc8$length <- rep(tmp3, 5)
sc9$length <- rep(tmp3, 3)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
sc1$condition <- as.factor(sc1$condition)
sc2$condition <- as.factor(sc2$condition)
sc3$condition <- as.factor(sc3$condition)
sc4$condition <- as.factor(sc4$condition)
sc5$condition <- as.factor(sc5$condition)
sc6$condition <- as.factor(sc6$condition)
sc7$condition <- as.factor(sc7$condition)
sc8$condition <- as.factor(sc8$condition)
sc9$condition <- as.factor(sc9$condition)

sc1$condition <- as.numeric(sc1$condition)
sc2$condition <- as.numeric(sc2$condition)
sc3$condition <- as.numeric(sc3$condition)
sc4$condition <- as.numeric(sc4$condition)
sc5$condition <- as.numeric(sc5$condition)
sc6$condition <- as.numeric(sc6$condition)
sc7$condition <- as.numeric(sc7$condition)
sc8$condition <- as.numeric(sc8$condition)
sc9$condition <- as.numeric(sc9$condition)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
sc1$Peptide <- as.numeric(sc1$Peptide)
sc3$Peptide <- as.numeric(sc3$Peptide)
sc4$Peptide <- as.numeric(sc4$Peptide)
sc6$Peptide <- as.numeric(sc6$Peptide)
sc7$Peptide <- as.numeric(sc7$Peptide)
sc9$Peptide <- as.numeric(sc9$Peptide)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# convert factor levels to actual numeric values
sc1$Time_ordered <- as.numeric(as.character(sc1$Time))
sc3$Time_ordered <- as.numeric(as.character(sc3$Time))
sc4$Time_ordered <- as.numeric(as.character(sc4$Time))
sc6$Time_ordered <- as.numeric(as.character(sc6$Time))
sc7$Time_ordered <- as.numeric(as.character(sc7$Time))
sc9$Time_ordered <- as.numeric(as.character(sc9$Time))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#set.seed(123)
start.time <- Sys.time()

mod1 <- lcMethodMixAK_GLMM(fixed = HDX_transformed ~ Time_ordered + condition + Time_ordered*condition, 
                           random = ~ SampleID,
                           id = "Peptide", 
                           time = "Time", 
                           nClusters = 10)
 
model1 <- latrend(mod1, sc1)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
summary(model1)
#mod1rep <- latrendRep(mod1, data = sc1, nClusters = 5, .rep = 5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# compute and plot the posterior cluster prob
mixAK1pp <- postprob(model1)
mixAK1cluster <-  trajectoryAssignments(model1)
# maximum posterior probabilities
maxpp <- apply(mixAK1pp, 1, max)
# dataframe with postprob
mixAK1plot <- data.frame(Peptide = sc1$Peptide,
                         cluster = mixAK1cluster,
                         postprob = maxpp)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
mixp1 <- ggplot(mixAK1plot,
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("mixAK") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

mixp1

# Save the plot
ggsave("mixp1.png", plot = mixp1, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
cluster_counts <- table(mixAK1cluster)
cluster_percentages <- round(100 * cluster_counts / length(mixAK1cluster))
non_zero_clusters <- cluster_percentages[cluster_percentages > 0]
permixAK1 <- paste(non_zero_clusters, "%", sep = "")

cluster_labels <- paste("Cluster ", names(non_zero_clusters), " (", permixAK1, ")", sep = "")

mixAK1plot$cluster <- factor(mixAK1plot$cluster,
                             levels = names(non_zero_clusters),
                             labels = cluster_labels)

# add time and HDX
mixAK1plot$Time <- sc1$Time
mixAK1plot$HDX <- sc1$HDX_transformed
mixAK1plot$Condition <- sc1$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = mixAK1plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("Smoothed HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +  # Adding smoothing for all peptides
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette = "Set1") + 
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# cluster trajectories
mix1 <- ggplot(data = mixAK1plot, 
               aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
  ggtitle("HDX Cluster Trajectories Over Time by Condition") +
  stat_smooth(method = "loess", aes(group = cluster), linetype = "solid", color = "black", span = 0.4, size = 0.5 ,se = FALSE) + # Mean trajectory per cluster with smoothing
  stat_summary(fun = mean, geom = "point", aes(group = cluster), color = "blue") + # Mean point per cluster
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3) + # Mean points per cluster with error bars
  theme_bw() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Time (secs)") +
  ylab("HDX") + 
  facet_wrap(.~Condition, ncol = 2)

mix1

# Save the plot
ggsave("mix1.png", plot = mix1, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list1 <- mixAK1plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list1


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
mixAKpred1 <- predict(model1, sc1)
mixAKpred1 <- as.data.frame(mixAKpred1)
mixAKpredicted1 <- apply(mixAKpred1, 1, which.max)
mixAKpred1$Time <- sc1$Time
mixAKpred1$HDX <- sc1$HDX_transformed
mixAKpred1$Peptide <- sc1$Peptide
mixAKpred1$Condition <- sc1$condition

# convert predictions to long format
mixAKpredict1 <- gather(mixAKpred1, key = "Component", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
mixAKpredict1$Component <- factor(mixAKpredict1$Component, 
                                  levels = c("Fit", "Fit.1", "Fit.2", "Fit.3", "Fit.4"), 
                                  labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"))
mixAKpredict1$Predicted[mixAKpredict1$Predicted < 0] <- 0


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
mixpred1 <- ggplot(data = mixAKpredict1, 
       aes(x = Time, y = HDX, group = interaction(Component, Condition), color = Component)) +
  stat_summary(aes(y = HDX, color = "Actual HDX"), 
               fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.3) +  # Mean points per cluster with error bars for actual HDX values
  stat_summary(aes(y = Predicted, color = "Predicted HDX"), 
               fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.3) +  # Mean points per cluster with error bars for predicted HDX values
  stat_summary(aes(y = HDX, color = "Actual HDX"), 
               fun.y = mean, 
               geom = "line", 
               linetype = "dashed") +  # Mean trajectory per cluster for actual HDX values
  stat_summary(aes(y = Predicted, color = "Predicted HDX"), 
               fun.y = mean, 
               geom = "line", 
               linetype = "dashed") +  # Mean trajectory per cluster for predicted HDX values
  stat_summary(aes(y = HDX, color = "Actual HDX"), 
               fun.y = mean, 
               geom = "point", 
               size = 0.5) +  # Mean point per cluster for actual HDX values, decreased size
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.3) + # Mean points per cluster with error bars
  stat_summary(aes(y = Predicted, color = "Predicted HDX"), 
               fun.y = mean, 
               geom = "point", 
               size = 0.5) +  # Mean point per cluster for predicted HDX values, decreased size
  scale_color_manual(name = "HDX", 
                     values = c("Actual HDX" = "blue", "Predicted HDX" = "red")) +  # Customize color scale
  ggtitle("Actual vs. Predicted HDX Trajectories Over Time") +
  xlab("Time (secs)") +
  ylab("HDX") +
  theme_bw() +
  facet_grid(Condition ~ Component) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

mixpred1

# Save the plot
ggsave("mixpred1.png", plot = mixpred1, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
model1@model$PED

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#set.seed(123)
start.time <- Sys.time()

mod2 <- lcMethodMixAK_GLMM(fixed = HDX_transformed ~ Time_ordered + condition + Time_ordered*condition, 
                            random = ~ SampleID,
                            id = "Peptide", 
                            time = "Time", 
                            nClusters = 10)
 
model2 <- latrend(mod2, sc3)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
summary(model2)
#mod2rep <- latrendRep(mod2, data = sc3, nClusters = 5, .rep = 5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# compute and plot the posterior cluster prob
mixAK2pp <- postprob(model2)
mixAK2cluster <-  trajectoryAssignments(model2)
# maximum posterior probabilities
maxpp2 <- apply(mixAK2pp, 1, max)
# dataframe with postprob
mixAK2plot <- data.frame(Peptide = sc3$Peptide,
                         cluster = mixAK2cluster,
                         postprob = maxpp2)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
mixp2 <- ggplot(mixAK2plot,
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("mixAK") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

mixp2

# Save the plot
ggsave("mixp2.png", plot = mixp2, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
cluster_counts2 <- table(mixAK2cluster)
cluster_percentages2 <- round(100 * cluster_counts2 / length(mixAK2cluster))
non_zero_clusters2 <- cluster_percentages2[cluster_percentages2 > 0]
permixAK2 <- paste(non_zero_clusters2, "%", sep = "")

cluster_labels2 <- paste("Cluster ", names(non_zero_clusters2), " (", permixAK2, ")", sep = "")

mixAK2plot$cluster <- factor(mixAK2plot$cluster,
                             levels = names(non_zero_clusters2),
                             labels = cluster_labels2)

# add time and HDX
mixAK2plot$Time <- sc3$Time
mixAK2plot$HDX <- sc3$HDX_transformed
mixAK2plot$Condition <- sc3$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = mixAK2plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("Smoothed HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +  # Adding smoothing for all peptides
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# cluster trajectories
mix2 <- ggplot(data = mixAK2plot, 
                 aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
          ggtitle("HDX Cluster Trajectories Over Time by Condition") +
          stat_summary(fun = mean, geom = "line", aes(group = cluster), linetype = "dashed", color = "black") + # Mean trajectory per cluster
          stat_summary(fun = mean, geom = "point", aes(group = cluster), color = "blue") + # Mean point per cluster
          stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3) + # Mean points per cluster with error bars
          theme_bw() +
          theme(legend.position = "right",
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)
mix2
# Save the plot
ggsave("mix2.png", plot = mix2, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list2 <- mixAK2plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list2


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
mixAKpred2 <- predict(model2, sc3)
mixAKpred2 <- as.data.frame(mixAKpred2)
mixAKpredicted2 <- apply(mixAKpred2, 1, which.max)
mixAKpred2$Time <- sc3$Time
mixAKpred2$HDX <- sc3$HDX_transformed
mixAKpred2$Peptide <- sc3$Peptide
mixAKpred2$Condition <- sc3$condition

# convert predictions to long format
mixAKpredict2 <- gather(mixAKpred2, key = "Component", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
mixAKpredict2$Component <- factor(mixAKpredict2$Component, 
                                  levels = c("Fit", "Fit.1", "Fit.2", "Fit.3", "Fit.4"), 
                                  labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"))
mixAKpredict2$Predicted[mixAKpredict2$Predicted < 0] <- 0


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
mixpred2 <- ggplot(data = mixAKpredict2, 
       aes(x = Time, y = HDX, group = interaction(Component, Condition), color = Component)) +
  stat_summary(aes(y = HDX, color = "Actual HDX"), 
               fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.3) +  # Mean points per cluster with error bars for actual HDX values
  stat_summary(aes(y = Predicted, color = "Predicted HDX"), 
               fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.3) +  # Mean points per cluster with error bars for predicted HDX values
  stat_summary(aes(y = HDX, color = "Actual HDX"), 
               fun.y = mean, 
               geom = "line", 
               linetype = "dashed") +  # Mean trajectory per cluster for actual HDX values
  stat_summary(aes(y = Predicted, color = "Predicted HDX"), 
               fun.y = mean, 
               geom = "line", 
               linetype = "dashed") +  # Mean trajectory per cluster for predicted HDX values
  stat_summary(aes(y = HDX, color = "Actual HDX"), 
               fun.y = mean, 
               geom = "point", 
               size = 0.5) +  # Mean point per cluster for actual HDX values, decreased size
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.3) + # Mean points per cluster with error bars
  stat_summary(aes(y = Predicted, color = "Predicted HDX"), 
               fun.y = mean, 
               geom = "point", 
               size = 0.5) +  # Mean point per cluster for predicted HDX values, decreased size
  scale_color_manual(name = "HDX", 
                     values = c("Actual HDX" = "blue", "Predicted HDX" = "red")) +  # Customize color scale
  ggtitle("Actual vs. Predicted HDX Trajectories Over Time") +
  xlab("Time (secs)") +
  ylab("HDX") +
  theme_bw() +
  facet_grid(Condition ~ Component) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

mixpred2

# Save the plot
ggsave("mixpred2.png", plot = mixpred2, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
model2@model$PED


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#set.seed(123)
start.time <- Sys.time()

mod3 <- lcMethodMixAK_GLMM(fixed = HDX_transformed ~ Time_ordered + condition + Time_ordered*condition, 
                            random = ~ SampleID,
                            id = "Peptide", 
                            time = "Time", 
                            nClusters = 10)
 
 model3 <- latrend(mod3, sc4)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
summary(model3)
#mod3rep <- latrendRep(mod3, data = sc4, nClusters = 5, .rep = 5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# compute and plot the posterior cluster prob
mixAK3pp <- postprob(model3)
mixAK3cluster <-  trajectoryAssignments(model3)
# maximum posterior probabilities
maxpp3 <- apply(mixAK3pp, 1, max)
# dataframe with postprob
mixAK3plot <- data.frame(Peptide = sc4$Peptide,
                         cluster = mixAK3cluster,
                         postprob = maxpp3)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
mixp3 <- ggplot(mixAK3plot,
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("mixAK") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

mixp3

# Save the plot
ggsave("mixp3.png", plot = mixp3, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
cluster_counts3 <- table(mixAK3cluster)
cluster_percentages3 <- round(100 * cluster_counts3 / length(mixAK3cluster))
non_zero_clusters3 <- cluster_percentages3[cluster_percentages3 > 0]
permixAK3 <- paste(non_zero_clusters3, "%", sep = "")

cluster_labels3 <- paste("Cluster ", names(non_zero_clusters3), " (", permixAK3, ")", sep = "")

mixAK3plot$cluster <- factor(mixAK3plot$cluster,
                             levels = names(non_zero_clusters3),
                             labels = cluster_labels3)

# add time and HDX
mixAK3plot$Time <- sc4$Time
mixAK3plot$HDX <- sc4$HDX_transformed
mixAK3plot$Condition <- sc4$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = mixAK3plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("Smoothed HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +  # Adding smoothing for all peptides
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# cluster trajectories
mix3 <- ggplot(data = mixAK3plot, 
                 aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
          ggtitle("HDX Cluster Trajectories Over Time by Condition") +
          stat_summary(fun = mean, geom = "line", aes(group = cluster), linetype = "dashed", color = "black") + # Mean trajectory per cluster
          stat_summary(fun = mean, geom = "point", aes(group = cluster), color = "blue") + # Mean point per cluster
          stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3) + # Mean points per cluster with error bars
          theme_bw() +
          theme(legend.position = "right",
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)
mix3
# Save the plot
ggsave("mix3.png", plot = mix3, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list3 <- mixAK3plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list3


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
mixAKpred3 <- predict(model3, sc4)
mixAKpred3 <- as.data.frame(mixAKpred3)
mixAKpredicted3 <- apply(mixAKpred3, 1, which.max)
mixAKpred3$Time <- sc4$Time
mixAKpred3$HDX <- sc4$HDX_transformed
mixAKpred3$Peptide <- sc4$Peptide
mixAKpred3$Condition <- sc4$condition

# convert predictions to long format
mixAKpredict3 <- gather(mixAKpred3, key = "Component", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
mixAKpredict3$Component <- factor(mixAKpredict3$Component, 
                                  levels = c("Fit", "Fit.1", "Fit.2", "Fit.3", "Fit.4"), 
                                  labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"))
mixAKpredict3$Predicted[mixAKpredict3$Predicted < 0] <- 0


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
mixpred3 <- ggplot(data = mixAKpredict3, 
       aes(x = Time, y = HDX, group = interaction(Component, Condition), color = Component)) +
  stat_summary(aes(y = HDX, color = "Actual HDX"), 
               fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.3) +  # Mean points per cluster with error bars for actual HDX values
  stat_summary(aes(y = Predicted, color = "Predicted HDX"), 
               fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.3) +  # Mean points per cluster with error bars for predicted HDX values
  stat_summary(aes(y = HDX, color = "Actual HDX"), 
               fun.y = mean, 
               geom = "line", 
               linetype = "dashed") +  # Mean trajectory per cluster for actual HDX values
  stat_summary(aes(y = Predicted, color = "Predicted HDX"), 
               fun.y = mean, 
               geom = "line", 
               linetype = "dashed") +  # Mean trajectory per cluster for predicted HDX values
  stat_summary(aes(y = HDX, color = "Actual HDX"), 
               fun.y = mean, 
               geom = "point", 
               size = 0.5) +  # Mean point per cluster for actual HDX values, decreased size
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.3) + # Mean points per cluster with error bars
  stat_summary(aes(y = Predicted, color = "Predicted HDX"), 
               fun.y = mean, 
               geom = "point", 
               size = 0.5) +  # Mean point per cluster for predicted HDX values, decreased size
  scale_color_manual(name = "HDX", 
                     values = c("Actual HDX" = "blue", "Predicted HDX" = "red")) +  # Customize color scale
  ggtitle("Actual vs. Predicted HDX Trajectories Over Time") +
  xlab("Time (secs)") +
  ylab("HDX") +
  theme_bw() +
  facet_grid(Condition ~ Component) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

mixpred3

# Save the plot
ggsave("mixpred3.png", plot = mixpred3, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
model3@model$PED


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#set.seed(123)
start.time <- Sys.time()

mod4 <- lcMethodMixAK_GLMM(fixed = HDX_transformed ~ Time_ordered + condition + Time_ordered*condition, 
                            random = ~ SampleID,
                            id = "Peptide", 
                            time = "Time", 
                            nClusters = 10)
 
model4 <- latrend(mod4, sc6)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
summary(model4)
#mod4rep <- latrendRep(mod4, data = sc6, nClusters = 5, .rep = 5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# compute and plot the posterior cluster prob
mixAK4pp <- postprob(model4)
mixAK4cluster <-  trajectoryAssignments(model4)
# maximum posterior probabilities
maxpp4 <- apply(mixAK4pp, 1, max)
# dataframe with postprob
mixAK4plot <- data.frame(Peptide = sc6$Peptide,
                         cluster = mixAK4cluster,
                         postprob = maxpp4)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
mixp4 <- ggplot(mixAK4plot,
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("mixAK") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

mixp4

# Save the plot
ggsave("mixp4.png", plot = mixp4, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
cluster_counts4 <- table(mixAK4cluster)
cluster_percentages4 <- round(100 * cluster_counts4 / length(mixAK4cluster))
non_zero_clusters4 <- cluster_percentages4[cluster_percentages4 > 0]
permixAK4 <- paste(non_zero_clusters4, "%", sep = "")

cluster_labels4 <- paste("Cluster ", names(non_zero_clusters4), " (", permixAK4, ")", sep = "")

mixAK4plot$cluster <- factor(mixAK4plot$cluster,
                             levels = names(non_zero_clusters4),
                             labels = cluster_labels4)

# add time and HDX
mixAK4plot$Time <- sc6$Time
mixAK4plot$HDX <- sc6$HDX_transformed
mixAK4plot$Condition <- sc6$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = mixAK4plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("Smoothed HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +  # Adding smoothing for all peptides
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# cluster trajectories
mix4 <- ggplot(data = mixAK4plot, 
                 aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
          ggtitle("HDX Cluster Trajectories Over Time by Condition") +
          stat_summary(fun = mean, geom = "line", aes(group = cluster), linetype = "dashed", color = "black") + # Mean trajectory per cluster
          stat_summary(fun = mean, geom = "point", aes(group = cluster), color = "blue") + # Mean point per cluster
          stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3) + # Mean points per cluster with error bars
          theme_bw() +
          theme(legend.position = "right",
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)
mix4
# Save the plot
ggsave("mix4.png", plot = mix4, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list4 <- mixAK4plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list4


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
mixAKpred4 <- predict(model4, sc6)
mixAKpred4 <- as.data.frame(mixAKpred4)
mixAKpredicted4 <- apply(mixAKpred4, 1, which.max)
mixAKpred4$Time <- sc6$Time
mixAKpred4$HDX <- sc6$HDX_transformed
mixAKpred4$Peptide <- sc6$Peptide
mixAKpred4$Condition <- sc6$condition

# convert predictions to long format
mixAKpredict4 <- gather(mixAKpred4, key = "Component", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
mixAKpredict4$Component <- factor(mixAKpredict4$Component, 
                                  levels = c("Fit", "Fit.1", "Fit.2", "Fit.3", "Fit.4"), 
                                  labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"))
mixAKpredict4$Predicted[mixAKpredict4$Predicted < 0] <- 0


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
mixpred4 <- ggplot(data = mixAKpredict4, 
       aes(x = Time, y = HDX, group = interaction(Component, Condition), color = Component)) +
  stat_summary(aes(y = HDX, color = "Actual HDX"), 
               fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.3) +  # Mean points per cluster with error bars for actual HDX values
  stat_summary(aes(y = Predicted, color = "Predicted HDX"), 
               fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.3) +  # Mean points per cluster with error bars for predicted HDX values
  stat_summary(aes(y = HDX, color = "Actual HDX"), 
               fun.y = mean, 
               geom = "line", 
               linetype = "dashed") +  # Mean trajectory per cluster for actual HDX values
  stat_summary(aes(y = Predicted, color = "Predicted HDX"), 
               fun.y = mean, 
               geom = "line", 
               linetype = "dashed") +  # Mean trajectory per cluster for predicted HDX values
  stat_summary(aes(y = HDX, color = "Actual HDX"), 
               fun.y = mean, 
               geom = "point", 
               size = 0.5) +  # Mean point per cluster for actual HDX values, decreased size
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.3) + # Mean points per cluster with error bars
  stat_summary(aes(y = Predicted, color = "Predicted HDX"), 
               fun.y = mean, 
               geom = "point", 
               size = 0.5) +  # Mean point per cluster for predicted HDX values, decreased size
  scale_color_manual(name = "HDX", 
                     values = c("Actual HDX" = "blue", "Predicted HDX" = "red")) +  # Customize color scale
  ggtitle("Actual vs. Predicted HDX Trajectories Over Time") +
  xlab("Time (secs)") +
  ylab("HDX") +
  theme_bw() +
  facet_grid(Condition ~ Component) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

mixpred4

# Save the plot
ggsave("mixpred4.png", plot = mixpred4, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
model4@model$PED


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#set.seed(123)
start.time <- Sys.time()

mod5 <- lcMethodMixAK_GLMM(fixed = HDX_transformed ~ Time_ordered + condition + Time_ordered*condition,
                           random = ~ SampleID,
                           id = "Peptide", 
                           time = "Time", 
                           nClusters = 10)
 
 model5 <- latrend(mod5, sc7)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
summary(model5)
#mod5rep <- latrendRep(mod5, data = sc7, nClusters = 5, .rep = 5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# compute and plot the posterior cluster prob
mixAK5pp <- postprob(model5)
mixAK5cluster <-  trajectoryAssignments(model5)
# maximum posterior probabilities
maxpp5 <- apply(mixAK5pp, 1, max)
# dataframe with postprob
mixAK5plot <- data.frame(Peptide = sc7$Peptide,
                         cluster = mixAK5cluster,
                         postprob = maxpp5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
mixp5 <- ggplot(mixAK5plot,
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("mixAK") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

mixp5

# Save the plot
ggsave("mixp5.png", plot = mixp5, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
cluster_counts5 <- table(mixAK5cluster)
cluster_percentages5 <- round(100 * cluster_counts5 / length(mixAK5cluster))
non_zero_clusters5 <- cluster_percentages5[cluster_percentages5 > 0]
permixAK5 <- paste(non_zero_clusters5, "%", sep = "")

cluster_labels5 <- paste("Cluster ", names(non_zero_clusters5), " (", permixAK5, ")", sep = "")

mixAK5plot$cluster <- factor(mixAK5plot$cluster,
                             levels = names(non_zero_clusters5),
                             labels = cluster_labels5)

# add time and HDX
mixAK5plot$Time <- sc7$Time
mixAK5plot$HDX <- sc7$HDX_transformed
mixAK5plot$Condition <- sc7$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = mixAK5plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("Smoothed HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +  # Adding smoothing for all peptides
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# cluster trajectories
mix5 <- ggplot(data = mixAK5plot, 
                 aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
          ggtitle("HDX Cluster Trajectories Over Time by Condition") +
          stat_summary(fun = mean, geom = "line", aes(group = cluster), linetype = "dashed", color = "black") + # Mean trajectory per cluster
          stat_summary(fun = mean, geom = "point", aes(group = cluster), color = "blue") + # Mean point per cluster
          stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3) + # Mean points per cluster with error bars
          theme_bw() +
          theme(legend.position = "right",
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)
mix5
# Save the plot
ggsave("mix5.png", plot = mix5, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list5 <- mixAK5plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list5


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
mixAKpred5 <- predict(model5, sc7)
mixAKpred5 <- as.data.frame(mixAKpred5)
mixAKpredicted5 <- apply(mixAKpred5, 1, which.max)
mixAKpred5$Time <- sc7$Time
mixAKpred5$HDX <- sc7$HDX_transformed
mixAKpred5$Peptide <- sc7$Peptide
mixAKpred5$Condition <- sc7$condition

# convert predictions to long format
mixAKpredict5 <- gather(mixAKpred5, key = "Component", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
mixAKpredict5$Component <- factor(mixAKpredict5$Component, 
                                  levels = c("Fit", "Fit.1", "Fit.2", "Fit.3", "Fit.4"), 
                                  labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"))
mixAKpredict5$Predicted[mixAKpredict5$Predicted < 0] <- 0


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
mixpred5 <- ggplot(data = mixAKpredict5, 
       aes(x = Time, y = HDX, group = interaction(Component, Condition), color = Component)) +
  stat_summary(aes(y = HDX, color = "Actual HDX"), 
               fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.3) +  # Mean points per cluster with error bars for actual HDX values
  stat_summary(aes(y = Predicted, color = "Predicted HDX"), 
               fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.3) +  # Mean points per cluster with error bars for predicted HDX values
  stat_summary(aes(y = HDX, color = "Actual HDX"), 
               fun.y = mean, 
               geom = "line", 
               linetype = "dashed") +  # Mean trajectory per cluster for actual HDX values
  stat_summary(aes(y = Predicted, color = "Predicted HDX"), 
               fun.y = mean, 
               geom = "line", 
               linetype = "dashed") +  # Mean trajectory per cluster for predicted HDX values
  stat_summary(aes(y = HDX, color = "Actual HDX"), 
               fun.y = mean, 
               geom = "point", 
               size = 0.5) +  # Mean point per cluster for actual HDX values, decreased size
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.3) + # Mean points per cluster with error bars
  stat_summary(aes(y = Predicted, color = "Predicted HDX"), 
               fun.y = mean, 
               geom = "point", 
               size = 0.5) +  # Mean point per cluster for predicted HDX values, decreased size
  scale_color_manual(name = "HDX", 
                     values = c("Actual HDX" = "blue", "Predicted HDX" = "red")) +  # Customize color scale
  ggtitle("Actual vs. Predicted HDX Trajectories Over Time") +
  xlab("Time (secs)") +
  ylab("HDX") +
  theme_bw() +
  facet_grid(Condition ~ Component) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

mixpred5

# Save the plot
ggsave("mixpred5.png", plot = mixpred5, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
model5@model$PED


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#set.seed(123)
start.time <- Sys.time()

mod6 <- lcMethodMixAK_GLMM(fixed = HDX_transformed ~ Time_ordered + condition + Time_ordered*condition,
                           random = ~ SampleID,
                           id = "Peptide", 
                           time = "Time", 
                           nClusters = 10)
 
 model6 <- latrend(mod6, sc9)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
summary(model6)
#mod6rep <- latrendRep(mod6, data = sc9, nClusters = 5, .rep = 5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# compute and plot the posterior cluster prob
mixAK6pp <- postprob(model6)
mixAK6cluster <-  trajectoryAssignments(model6)
# maximum posterior probabilities
maxpp6 <- apply(mixAK6pp, 1, max)
# dataframe with postprob
mixAK6plot <- data.frame(Peptide = sc9$Peptide,
                         cluster = mixAK6cluster,
                         postprob = maxpp6)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
mixp6 <- ggplot(mixAK6plot,
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("mixAK") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

mixp6

# Save the plot
ggsave("mixp6.png", plot = mixp6, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
cluster_counts6 <- table(mixAK6cluster)
cluster_percentages6 <- round(100 * cluster_counts6 / length(mixAK6cluster))
non_zero_clusters6 <- cluster_percentages6[cluster_percentages6 > 0]
permixAK6 <- paste(non_zero_clusters6, "%", sep = "")

cluster_labels6 <- paste("Cluster ", names(non_zero_clusters6), " (", permixAK6, ")", sep = "")

mixAK6plot$cluster <- factor(mixAK6plot$cluster,
                             levels = names(non_zero_clusters6),
                             labels = cluster_labels6)

# add time and HDX
mixAK6plot$Time <- sc9$Time
mixAK6plot$HDX <- sc9$HDX_transformed
mixAK6plot$Condition <- sc9$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = mixAK6plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("Smoothed HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +  # Adding smoothing for all peptides
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# cluster trajectories
mix6 <- ggplot(data = mixAK6plot, 
                 aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
          ggtitle("HDX Cluster Trajectories Over Time by Condition") +
          stat_summary(fun = mean, geom = "line", aes(group = cluster), linetype = "dashed", color = "black") + # Mean trajectory per cluster
          stat_summary(fun = mean, geom = "point", aes(group = cluster), color = "blue") + # Mean point per cluster
          stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3) + # Mean points per cluster with error bars
          theme_bw() +
          theme(legend.position = "right",
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)
mix6
# Save the plot
ggsave("mix6.png", plot = mix6, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list6 <- mixAK6plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list6


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
mixAKpred6 <- predict(model6, sc9)
mixAKpred6 <- as.data.frame(mixAKpred6)
mixAKpredicted6 <- apply(mixAKpred6, 1, which.max)
mixAKpred6$Time <- sc9$Time
mixAKpred6$HDX <- sc9$HDX_transformed
mixAKpred6$Peptide <- sc9$Peptide
mixAKpred6$Condition <- sc9$condition

# convert predictions to long format
mixAKpredict6 <- gather(mixAKpred6, key = "Component", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
mixAKpredict6$Component <- factor(mixAKpredict6$Component, 
                                  levels = c("Fit", "Fit.1", "Fit.2", "Fit.3", "Fit.4"), 
                                  labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"))
mixAKpredict6$Predicted[mixAKpredict6$Predicted < 0] <- 0


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
summary(mixAKpredict6)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
mixpred6 <- ggplot(data = mixAKpredict6, 
       aes(x = Time, y = HDX, group = interaction(Component, Condition), color = Component)) +
  stat_summary(aes(y = HDX, color = "Actual HDX"), 
               fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.3) +  # Mean points per cluster with error bars for actual HDX values
  stat_summary(aes(y = Predicted, color = "Predicted HDX"), 
               fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.3) +  # Mean points per cluster with error bars for predicted HDX values
  stat_summary(aes(y = HDX, color = "Actual HDX"), 
               fun.y = mean, 
               geom = "line", 
               linetype = "dashed") +  # Mean trajectory per cluster for actual HDX values
  stat_summary(aes(y = Predicted, color = "Predicted HDX"), 
               fun.y = mean, 
               geom = "line", 
               linetype = "dashed") +  # Mean trajectory per cluster for predicted HDX values
  stat_summary(aes(y = HDX, color = "Actual HDX"), 
               fun.y = mean, 
               geom = "point", 
               size = 0.5) +  # Mean point per cluster for actual HDX values, decreased size
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.3) + # Mean points per cluster with error bars
  stat_summary(aes(y = Predicted, color = "Predicted HDX"), 
               fun.y = mean, 
               geom = "point", 
               size = 0.5) +  # Mean point per cluster for predicted HDX values, decreased size
  scale_color_manual(name = "HDX", 
                     values = c("Actual HDX" = "blue", "Predicted HDX" = "red")) +  # Customize color scale
  ggtitle("Actual vs. Predicted HDX Trajectories Over Time") +
  xlab("Time (secs)") +
  ylab("HDX") +
  theme_bw() +
  facet_grid(Condition ~ Component) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

mixpred6

# Save the plot
ggsave("mixpred6.png", plot = mixpred6, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
model6@model$PED


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
df1 <- mixAK1plot %>% mutate(source = 'setting 1')
df2 <- mixAK2plot %>% mutate(source = 'setting 2')
df3 <- mixAK3plot %>% mutate(source = 'setting 3')
df4 <- mixAK4plot %>% mutate(source = 'setting 4')
df5 <- mixAK5plot %>% mutate(source = 'setting 5')
df6 <- mixAK6plot %>% mutate(source = 'setting 6')

# combine
mixmerge <- bind_rows(df1, df2, df3, df4, df5, df6)
mixmerge$source <- as.factor(mixmerge$source)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
mixbox <- ggplot(mixmerge, 
                  aes(x = factor(cluster), y = postprob, color = source)) + 
  geom_boxplot() + 
  ggtitle("mixAK") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

mixbox

# Save the plot
ggsave("mixbox.png", plot = mixbox, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------


