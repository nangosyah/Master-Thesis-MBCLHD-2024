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


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# data
data <- c(8, 6, 5, 13, 2, 8, 6, 13, 6, 7, 4, 5, 4, 3, 2, 4, 2, 3, 11, 
          5, 9, 13, 5, 4, 4, 13, 7, 10, 12, 9, 2, 6, 9, 5, 9, 7, 3, 11, 
          11, 5, 6, 9, 13, 13, 8, 13, 4, 9, 11, 3, 5, 8, 8, 9, 8, 11, 5, 
          5, 9, 10, 8, 6, 4, 9, 4, 5, 8, 6, 8, 13, 10, 3, 8, 2, 8, 3, 8, 
          11, 11, 11, 9, 3, 8, 11, 3, 10, 7, 4, 3, 5, 2, 8, 6, 11, 10, 12, 3, 8, 2, 9)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
length <- length(data)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# maximum number of exchanges
max_exchanges <- max(data)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
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


## ------------------------------------------------------------------------------------------------------------------------------------
sc1$cathdx <- rep(tmp1, 10)
sc2$cathdx <- rep(tmp1, 5)
sc3$cathdx <- rep(tmp1, 3)

sc4$cathdx <- rep(tmp2, 10)
sc5$cathdx <- rep(tmp2, 5)
sc6$cathdx <- rep(tmp2, 3)

sc7$cathdx <- rep(tmp3, 10)
sc8$cathdx <- rep(tmp3, 5)
sc9$cathdx <- rep(tmp3, 3)


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


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

lcga1 <- hlme(HDX_transformed ~ Time + condition + Time*condition,
              subject = "Peptide", 
              ng = 1, 
              data = sc1) 

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

lcga2 <- hlme(HDX_transformed ~ Time + condition + Time*condition,
              subject = "Peptide", 
              ng = 2, 
              data = sc1,
              mixture = ~ Time,
              B = lcga1)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

lcga3 <- hlme(HDX_transformed ~ Time + condition + Time*condition,
              subject = "Peptide", 
              ng = 3, 
              data = sc1,
              mixture = ~ Time,
              B = lcga1)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

lcga4 <- hlme(HDX_transformed ~ Time + condition + Time*condition,
              subject = "Peptide", 
              ng = 4, 
              data = sc1,
              mixture = ~ Time,
              B = lcga1)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make table with results for the 3 models:
summarytable(lcga1, lcga2, lcga3, lcga4)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
summary(lcga4)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
lcmm::postprob(lcga4)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# goodness of fit
#plot(lcga4)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# predicted trajectories
lcga41plot <- predictY(lcga4, sc1, var.time = "Time")
lcga42plot <- as.data.frame(lcga41plot$pred)
lcga42plot$Time <- lcga41plot$times[,1]
lcga42plot$HDX <- sc1$HDX_transformed
lcga42plot$Peptide <- sc1$Peptide
lcga42plot$Condition <- sc1$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# dataframe with postprob
lcga43plot <- lcga4$pprob %>% 
  mutate(max_prob = pmax(prob1, prob2, prob3, prob4))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(lcga43plot, 
       aes(x = factor(class), y = max_prob)) + 
  geom_boxplot() + 
  ggtitle("lcmm") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Compute percentages per cluster
per1 <- paste(round(100 * table(lcga43plot$class) / length(lcga43plot$class)), "%", sep = "")

# cluster labels
lcga43plot$cluster <- factor(lcga43plot$class,
                                labels = paste("Cluster ", 1:length(per1), " (", per1, ")", sep = ""))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Merge based on peptide column
lcga44plot <- inner_join(lcga42plot, lcga43plot, by = "Peptide")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = lcga44plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
lcga1c <- ggplot(data = lcga44plot, 
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
lcga1c
# Save the plot
ggsave("lcga1c.png", plot = lcga1c, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list1 <- lcga44plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list1


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
lpred1 <- gather(lcga44plot, key = "cluster", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
lpred1$cluster <- factor(lpred1$cluster, levels = c("Ypred_class1", "Ypred_class2", "Ypred_class3", "Ypred_class4"))

lpred1$Predicted <- as.numeric(lpred1$Predicted)

lpred1 <- na.omit(lpred1)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
lcmmpred1 <- ggplot(data = lpred1, 
       aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
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
  facet_grid(Condition ~ cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

lcmmpred1

# Save the plot
ggsave("lcmmpred1.png", plot = lcmmpred1, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

lcga5 <- hlme(HDX_transformed ~ Time + condition + Time*condition,
              subject = "Peptide", 
              ng = 1, 
              data = sc3)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# scenario 2
fit_models <- function(min_k, max_k, data) {
  results <- list()
  
  for (k in min_k:max_k) {
    set.seed(123)
    start.time <- Sys.time()
    
    model <- hlme(HDX_transformed ~ Time + condition + Time*condition,
                  subject = "Peptide",
                  ng = k,
                  data = data,
                  mixture = ~ Time,
                  B = lcga5)
    
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    
    results[[paste0("lcga", k)]] <- list(model = model, time_taken = time.taken)
  }
  return(results)
}


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
scenario2 <- fit_models(min_k = 2, max_k = 10, data = sc3)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make table with results for the 3 models:
lcga6 <- scenario2$lcga2$model
lcga7 <- scenario2$lcga3$model
lcga8 <- scenario2$lcga4$model

summarytable(lcga5, lcga6, lcga7, lcga8)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
summary(lcga8)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
lcmm::postprob(lcga8)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# predicted trajectories
lcga81plot <- predictY(lcga8, sc3, var.time = "Time")
lcga82plot <- as.data.frame(lcga81plot$pred)
lcga82plot$Time <- lcga81plot$times[,1]
lcga82plot$HDX <- sc3$HDX_transformed
lcga82plot$Peptide <- sc3$Peptide
lcga82plot$Condition <- sc3$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# goodness of fit
#plot(lcga8)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# dataframe with postprob
lcga83plot <- lcga8$pprob %>% 
  mutate(max_prob = pmax(prob1, prob2, prob3, prob4))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(lcga83plot, 
       aes(x = factor(class), y = max_prob)) + 
  geom_boxplot() + 
  ggtitle("lcmm") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Compute percentages per cluster
per2 <- paste(round(100 * table(lcga83plot$class) / length(lcga83plot$class)), "%", sep = "")

# cluster labels
lcga83plot$cluster <- factor(lcga83plot$class,
                                labels = paste("Cluster ", 1:length(per2), " (", per2, ")", sep = ""))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Merge based on peptide column
lcga84plot <- inner_join(lcga82plot, lcga83plot, by = "Peptide")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = lcga84plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("Smoothed HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +  # Adding smoothing for all peptides
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Cluster trajectories
lcga2c <- ggplot(data = lcga84plot, 
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
lcga2c
# Save the plot
ggsave("lcga2c.png", plot = lcga2c, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list2 <- lcga84plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list2


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
lpred2 <- gather(lcga84plot, key = "cluster", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
lpred2$cluster <- factor(lpred2$cluster, levels = c("Ypred_class1", "Ypred_class2", "Ypred_class3", "Ypred_class4"))

lpred2$Predicted <- as.numeric(lpred2$Predicted)

lpred2 <- na.omit(lpred2)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
lcmmpred2 <- ggplot(data = lpred2, 
       aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
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
  facet_grid(Condition ~ cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

lcmmpred2

# Save the plot
ggsave("lcmmpred2.png", plot = lcmmpred2, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

lcga9 <- hlme(HDX_transformed ~ Time + condition + Time*condition,
              subject = "Peptide", 
              ng = 1, 
              data = sc4)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# scenario 3
fit_models1 <- function(min_k, max_k, data) {
  results <- list()
  
  for (k in min_k:max_k) {
    set.seed(123)
    start.time <- Sys.time()
    
    model <- hlme(HDX_transformed ~ Time + condition + Time*condition,
                  subject = "Peptide",
                  ng = k,
                  data = data,
                  mixture = ~ Time,
                  B = lcga9)
    
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    
    results[[paste0("lcga", k)]] <- list(model = model, time_taken = time.taken)
  }
  return(results)
}


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
scenario3 <- fit_models1(min_k = 2, max_k = 10, data = sc4)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make table with results for the 3 models:
lcga10 <- scenario3$lcga2$model
lcga11 <- scenario3$lcga3$model
lcga12 <- scenario3$lcga4$model

summarytable(lcga9, lcga10, lcga11, lcga12)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
lcmm::postprob(lcga12)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
summary(lcga12)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# goodness of fit
#plot(lcga12)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# predicted trajectories
lcga121plot <- predictY(lcga12, sc4, var.time = "Time")
lcga122plot <- as.data.frame(lcga121plot$pred)
lcga122plot$Time <- lcga121plot$times[,1]
lcga122plot$HDX <- sc4$HDX_transformed
lcga122plot$Peptide <- sc4$Peptide
lcga122plot$Condition <- sc4$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# dataframe with postprob
lcga123plot <- lcga12$pprob %>% 
  mutate(max_prob = pmax(prob1, prob2, prob3, prob4))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(lcga123plot, 
       aes(x = factor(class), y = max_prob)) + 
  geom_boxplot() + 
  ggtitle("lcmm") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Compute percentages per cluster
per3 <- paste(round(100 * table(lcga123plot$class) / length(lcga123plot$class)), "%", sep = "")

# cluster labels
lcga123plot$cluster <- factor(lcga123plot$class,
                                labels = paste("Cluster ", 1:length(per3), " (", per3, ")", sep = ""))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Merge based on peptide column
lcga124plot <- inner_join(lcga122plot, lcga123plot, by = "Peptide")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = lcga124plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
lcga3c <- ggplot(data = lcga124plot, 
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
lcga3c
# Save the plot
ggsave("lcga3c.png", plot = lcga3c, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list3 <- lcga124plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list3


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
lpred3 <- gather(lcga124plot, key = "cluster", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
lpred3$cluster <- factor(lpred3$cluster, levels = c("Ypred_class1", "Ypred_class2", "Ypred_class3", "Ypred_class4"))

lpred3$Predicted <- as.numeric(lpred3$Predicted)

lpred3 <- na.omit(lpred3)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
lcmmpred3 <- ggplot(data = lpred3, 
       aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
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
  facet_grid(Condition ~ cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

lcmmpred3

# Save the plot
ggsave("lcmmpred3.png", plot = lcmmpred3, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

lcga13 <- hlme(HDX_transformed ~ Time + condition + Time*condition,
              subject = "Peptide", 
              ng = 1, 
              data = sc6)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# scenario 4
fit_models2 <- function(min_k, max_k, data) {
  results <- list()
  
  for (k in min_k:max_k) {
    set.seed(123)
    start.time <- Sys.time()
    
    model <- hlme(HDX_transformed ~ Time + condition + Time*condition,
                  subject = "Peptide",
                  ng = k,
                  data = data,
                  mixture = ~ Time,
                  B = lcga13)
    
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    
    results[[paste0("lcga", k)]] <- list(model = model, time_taken = time.taken)
  }
  return(results)
}


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
scenario4 <- fit_models2(min_k = 2, max_k = 10, data = sc6)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make table with results for the 3 models:
lcga14 <- scenario4$lcga2$model
lcga15 <- scenario4$lcga3$model
lcga16 <- scenario4$lcga4$model

summarytable(lcga13, lcga14, lcga15, lcga16)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
lcmm::postprob(lcga16)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
summary(lcga16)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#plot(lcga16)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# predicted trajectories
lcga161plot <- predictY(lcga16, sc6, var.time = "Time")
lcga162plot <- as.data.frame(lcga161plot$pred)
lcga162plot$Time <- lcga161plot$times[,1]
lcga162plot$HDX <- sc6$HDX_transformed
lcga162plot$Peptide <- sc6$Peptide
lcga162plot$Condition <- sc6$condition
# lcga162plot <- gather(lcga164plot, key = "Time", value = "Predicted")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
lcga162plot$HDX <- sc6$HDX_transformed
lcga162plot$Peptide <- sc6$Peptide
lcga162plot$Condition <- sc6$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# dataframe with postprob
lcga163plot <- lcga16$pprob %>% 
  mutate(max_prob = pmax(prob1, prob2, prob3, prob4))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(lcga163plot, 
       aes(x = factor(class), y = max_prob)) + 
  geom_boxplot() + 
  ggtitle("lcmm") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Compute percentages per cluster
per4 <- paste(round(100 * table(lcga163plot$class) / length(lcga163plot$class)), "%", sep = "")

# cluster labels
lcga163plot$cluster <- factor(lcga163plot$class,
                                labels = paste("Cluster ", 1:length(per4), " (", per4, ")", sep = ""))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Merge based on peptide column
lcga164plot <- inner_join(lcga162plot, lcga163plot, by = "Peptide")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = lcga164plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
lcga4c <- ggplot(data = lcga164plot, 
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
lcga4c
# Save the plot
ggsave("lcga4c.png", plot = lcga4c, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list4 <- lcga164plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list4


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
lpred4 <- gather(lcga164plot, key = "cluster", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
lpred4$cluster <- factor(lpred4$cluster, levels = c("Ypred_class1", "Ypred_class2", "Ypred_class3", "Ypred_class4"))

lpred4$Predicted <- as.numeric(lpred4$Predicted)

lpred4 <- na.omit(lpred4)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
lcmmpred4 <- ggplot(data = lpred4, 
       aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
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
  facet_grid(Condition ~ cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

lcmmpred4

# Save the plot
ggsave("lcmmpred4.png", plot = lcmmpred4, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

lcga17 <- hlme(HDX_transformed ~ Time + condition + Time*condition,
              subject = "Peptide", 
              ng = 1, 
              data = sc7)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# scenario 5
fit_models3 <- function(min_k, max_k, data) {
  results <- list()
  
  for (k in min_k:max_k) {
    set.seed(123)
    start.time <- Sys.time()
    
    model <- hlme(HDX_transformed ~ Time + condition + Time*condition,
                  subject = "Peptide",
                  ng = k,
                  data = data,
                  mixture = ~ Time,
                  B = lcga17)
    
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    
    results[[paste0("lcga", k)]] <- list(model = model, time_taken = time.taken)
  }
  return(results)
}


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
scenario5 <- fit_models3(min_k = 2, max_k = 10, data = sc7)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make table with results for the 3 models:
lcga18 <- scenario5$lcga2$model
lcga19 <- scenario5$lcga3$model
lcga20 <- scenario5$lcga4$model

summarytable(lcga17, lcga18, lcga19, lcga20)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
lcmm::postprob(lcga20)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
summary(lcga20)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# predicted trajectories
lcga201plot <- predictY(lcga20, sc7, var.time = "Time")
lcga202plot <- as.data.frame(lcga201plot$pred)
lcga202plot$Time <- lcga201plot$times[,1]
lcga202plot$HDX <- sc7$HDX_transformed
lcga202plot$Peptide <- sc7$Peptide
lcga202plot$Condition <- sc7$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# dataframe with postprob
lcga203plot <- lcga20$pprob %>% 
  mutate(max_prob = pmax(prob1, prob2, prob3, prob4))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(lcga203plot, 
       aes(x = factor(class), y = max_prob)) + 
  geom_boxplot() + 
  ggtitle("lcmm") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Compute percentages per cluster
per5 <- paste(round(100 * table(lcga203plot$class) / length(lcga203plot$class)), "%", sep = "")

# cluster labels
lcga203plot$cluster <- factor(lcga203plot$class,
                                labels = paste("Cluster ", 1:length(per5), " (", per5, ")", sep = ""))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Merge based on peptide column
lcga204plot <- inner_join(lcga202plot, lcga203plot, by = "Peptide")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = lcga204plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
lcga5c <- ggplot(data = lcga204plot, 
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
lcga5c
# Save the plot
ggsave("lcga5c.png", plot = lcga5c, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list5 <- lcga204plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list5


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
lpred5 <- gather(lcga204plot, key = "cluster", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
lpred5$cluster <- factor(lpred5$cluster, levels = c("Ypred_class1", "Ypred_class2", "Ypred_class3", "Ypred_class4"))

lpred5$Predicted <- as.numeric(lpred5$Predicted)

lpred5 <- na.omit(lpred5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
lcmmpred5 <- ggplot(data = lpred5, 
       aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
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
  facet_grid(Condition ~ cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

lcmmpred5

# Save the plot
ggsave("lcmmpred5.png", plot = lcmmpred5, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

lcga21 <- hlme(HDX_transformed ~ Time + condition + Time*condition,
              subject = "Peptide", 
              ng = 1, 
              data = sc9)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# scenario 6
fit_models4 <- function(min_k, max_k, data) {
  results <- list()
  
  for (k in min_k:max_k) {
    set.seed(123)
    start.time <- Sys.time()
    
    model <- hlme(HDX_transformed ~ Time + condition + Time*condition,
                  subject = "Peptide",
                  ng = k,
                  data = data,
                  mixture = ~ Time,
                  B = lcga21)
    
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    
    results[[paste0("lcga", k)]] <- list(model = model, time_taken = time.taken)
  }
  return(results)
}


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
scenario6 <- fit_models4(min_k = 2, max_k = 10, data = sc9)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make table with results for the 3 models:
lcga22 <- scenario6$lcga2$model
lcga23 <- scenario6$lcga3$model
lcga24 <- scenario6$lcga4$model

summarytable(lcga21, lcga22, lcga23, lcga24)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
lcmm::postprob(lcga24)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
summary(lcga24)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# predicted trajectories
lcga231plot <- predictY(lcga24, sc9, var.time = "Time")
lcga232plot <- as.data.frame(lcga231plot$pred)
lcga232plot$Time <- lcga231plot$times[,1]
lcga232plot$HDX <- sc9$HDX_transformed
lcga232plot$Peptide <- sc9$Peptide
lcga232plot$Condition <- sc9$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# dataframe with postprob
lcga233plot <- lcga24$pprob %>% 
  mutate(max_prob = pmax(prob1, prob2, prob3, prob4))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(lcga233plot, 
       aes(x = factor(class), y = max_prob)) + 
  geom_boxplot() + 
  ggtitle("lcmm") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Compute percentages per cluster
per6 <- paste(round(100 * table(lcga233plot$class) / length(lcga233plot$class)), "%", sep = "")

# cluster labels
lcga233plot$cluster <- factor(lcga233plot$class,
                                labels = paste("Cluster ", 1:length(per6), " (", per6, ")", sep = ""))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Merge based on peptide column
lcga234plot <- inner_join(lcga232plot, lcga233plot, by = "Peptide")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = lcga234plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
lcga6c <- ggplot(data = lcga234plot, 
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
lcga6c
# Save the plot
ggsave("lcga6c.png", plot = lcga6c, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list6 <- lcga234plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list6


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
lpred6 <- gather(lcga234plot, key = "cluster", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
lpred6$cluster <- factor(lpred6$cluster, levels = c("Ypred_class1", "Ypred_class2", "Ypred_class3", "Ypred_class4"))

lpred6$Predicted <- as.numeric(lpred6$Predicted)

lpred6 <- na.omit(lpred6)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
lcmmpred6 <- ggplot(data = lpred6, 
       aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
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
  facet_grid(Condition ~ cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

lcmmpred6

# Save the plot
ggsave("lcmmpred6.png", plot = lcmmpred6, width = 10, height = 8, dpi = 300)

