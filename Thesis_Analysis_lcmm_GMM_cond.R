## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
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


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
sc1 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc1.csv")
sc2 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc2.csv")
sc3 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc3.csv")
sc4 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc4.csv")
sc5 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc5.csv")
sc6 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc6.csv")
sc7 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc7.csv")
sc8 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc8.csv")
sc9 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc9.csv")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# data
data <- c(8, 6, 5, 13, 2, 8, 6, 13, 6, 7, 4, 5, 4, 3, 2, 4, 2, 3, 11, 
          5, 9, 13, 5, 4, 4, 13, 7, 10, 12, 9, 2, 6, 9, 5, 9, 7, 3, 11, 
          11, 5, 6, 9, 13, 13, 8, 13, 4, 9, 11, 3, 5, 8, 8, 9, 8, 11, 5, 
          5, 9, 10, 8, 6, 4, 9, 4, 5, 8, 6, 8, 13, 10, 3, 8, 2, 8, 3, 8, 
          11, 11, 11, 9, 3, 8, 11, 3, 10, 7, 4, 3, 5, 2, 8, 6, 11, 10, 12, 3, 8, 2, 9)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
length <- length(data)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# maximum number of exchanges
max_exchanges <- max(data)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# maximum number of exchanges + 2
max_exchange_vector <- rep(max_exchanges + 2, length)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
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


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
sc1$Time <- as.numeric(sc1$Time)
sc2$Time <- as.numeric(sc2$Time)
sc3$Time <- as.numeric(sc3$Time)
sc4$Time <- as.numeric(sc4$Time)
sc5$Time <- as.numeric(sc5$Time)
sc6$Time <- as.numeric(sc6$Time)
sc7$Time <- as.numeric(sc7$Time)
sc8$Time <- as.numeric(sc8$Time)
sc9$Time <- as.numeric(sc9$Time)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
sc1$Time[sc1$Time == 1] <- 0
sc2$Time[sc2$Time == 1] <- 0
sc3$Time[sc3$Time == 1] <- 0
sc4$Time[sc4$Time == 1] <- 0
sc5$Time[sc5$Time == 1] <- 0
sc6$Time[sc6$Time == 1] <- 0
sc7$Time[sc7$Time == 1] <- 0
sc8$Time[sc8$Time == 1] <- 0
sc9$Time[sc9$Time == 1] <- 0


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
sc1$Time <- as.factor(sc1$Time)
sc2$Time <- as.factor(sc2$Time)
sc3$Time <- as.factor(sc3$Time)
sc4$Time <- as.factor(sc4$Time)
sc5$Time <- as.factor(sc5$Time)
sc6$Time <- as.factor(sc6$Time)
sc7$Time <- as.factor(sc7$Time)
sc8$Time <- as.factor(sc8$Time)
sc9$Time <- as.factor(sc9$Time)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
tmp1 <- rep(data, each = 11)
tmp2 <- rep(data, each = 6)
tmp3 <- rep(data, each = 4)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
sc1$length <- rep(tmp1, 10)
sc2$length <- rep(tmp1, 5)
sc3$length <- rep(tmp1, 3)

sc4$length <- rep(tmp2, 10)
sc5$length <- rep(tmp2, 5)
sc6$length <- rep(tmp2, 3)

sc7$length <- rep(tmp3, 10)
sc8$length <- rep(tmp3, 5)
sc9$length <- rep(tmp3, 3)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
sc1$condition <- as.factor(sc1$condition)
sc2$condition <- as.factor(sc2$condition)
sc3$condition <- as.factor(sc3$condition)
sc4$condition <- as.factor(sc4$condition)
sc5$condition <- as.factor(sc5$condition)
sc6$condition <- as.factor(sc6$condition)
sc7$condition <- as.factor(sc7$condition)
sc8$condition <- as.factor(sc8$condition)
sc9$condition <- as.factor(sc9$condition)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmm1 <- hlme(HDX ~ Time + condition + Time*condition,
             subject = "Peptide", 
             random = ~ SampleID,
             ng = 1, 
             data = sc1) 

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmm2 <- hlme(HDX ~ Time + condition + Time*condition,
              subject = "Peptide",
              ng = 2,
              # statement indicates that we want to allow the variances of the random intercepts to vary across classes
              nwg=T, 
              random = ~ SampleID,
              mixture = ~ Time,
              data = sc1,
              B = gmm1) 

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmm3 <- hlme(HDX ~ Time + condition + Time*condition,
             subject = "Peptide",
             ng = 3,
             nwg=T, 
             random = ~ SampleID,
             mixture = ~ Time,
             data = sc1,
             B = gmm1)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmm4 <- hlme(HDX ~ Time + condition + Time*condition,
             subject = "Peptide",
             ng = 4,
             nwg=T, 
             random = ~ SampleID,
             mixture = ~ Time,
             data = sc1,
             B = gmm1) 

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# make table with results for the 3 models:
summarytable(gmm1, gmm2, gmm3, gmm4)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
summary(gmm4)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
lcmm::postprob(gmm4)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# predicted trajectories
gmm41plot <- predictY(gmm4, sc1, var.time = "Time")
gmm42plot <- as.data.frame(gmm41plot$pred)
gmm42plot$Time <- gmm41plot$times[,1]
gmm42plot$HDX <- sc1$HDX_transformed
gmm42plot$Peptide <- sc1$Peptide


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# dataframe with postprob
gmm43plot <- gmm4$pprob %>% 
  mutate(max_prob = pmax(prob1, prob2, prob3, prob4))


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(gmm43plot, 
       aes(x = factor(class), y = max_prob)) + 
  geom_boxplot() + 
  ggtitle("lcmm") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# Compute percentages per cluster
per1 <- paste(round(100 * table(gmm43plot$class) / length(gmm43plot$class)), "%", sep = "")

# cluster labels
gmm43plot$cluster <- factor(gmm43plot$class,
                                labels = paste("Cluster ", 1:length(per1), " (", per1, ")", sep = ""))


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# Merge based on peptide column
gmm44plot <- inner_join(gmm42plot, gmm43plot, by = "Peptide")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = gmm44plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("Smoothed HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +  # Adding smoothing for all peptides
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# cluster trajectories
lcga1c <- ggplot(data = gmm44plot, 
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


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# peptide in cluster
list1 <- gmm44plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list1


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------




## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmm5 <- hlme(HDX ~ Time + condition + Time*condition,
             subject = "Peptide", 
             random = ~ SampleID,
             ng = 1, 
             data = sc3) 

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# scenario 2
fit_models <- function(min_k, max_k, data) {
  results <- list()
  
  for (k in min_k:max_k) {
    set.seed(123)
    start.time <- Sys.time()
    
    model <- hlme(HDX ~ Time + condition + Time*condition,
                  subject = "Peptide",
                  ng = k,
                  nwg=T, 
                  random = ~ SampleID,
                  data = data,
                  mixture = ~ Time,
                  B = gmm5)
    
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    
    results[[paste0("gmm", k)]] <- list(model = model, time_taken = time.taken)
  }
  return(results)
}


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
scenario2 <- fit_models(min_k = 2, max_k = 10, data = sc3)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
scenario2$gmm2$time_taken
scenario2$gmm3$time_taken
scenario2$gmm4$time_taken


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# make table with results for the 3 models:
gmm6 <- scenario2$gmm2$model
gmm7 <- scenario2$gmm3$model
gmm8 <- scenario2$gmm4$model

summarytable(gmm5, gmm6, gmm7, gmm8)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
summary(gmm7)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
lcmm::postprob(gmm7)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# predicted trajectories
gmm81plot <- predictY(gmm7, sc3, var.time = "Time")
gmm82plot <- as.data.frame(gmm81plot$pred)
gmm82plot$Time <- gmm81plot$times[,1]
gmm82plot$HDX <- sc3$HDX_transformed
gmm82plot$Peptide <- sc3$Peptide


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# dataframe with postprob
gmm83plot <- gmm8$pprob %>% 
  mutate(max_prob = pmax(prob1, prob2, prob3, prob4))


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# Posterior cluster probability
lcgap2 <- ggplot(gmm83plot, 
       aes(x = factor(class), y = max_prob)) + 
  geom_boxplot() + 
  ggtitle("lcmm") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

lcgap2
# Save the plot
ggsave("lcgap2.png", plot = lcgap2, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# Compute percentages per cluster
per2 <- paste(round(100 * table(gmm83plot$class) / length(gmm83plot$class)), "%", sep = "")

# cluster labels
gmm83plot$cluster <- factor(gmm83plot$class,
                                labels = paste("Cluster ", 1:length(per2), " (", per2, ")", sep = ""))


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# Merge based on peptide column
gmm84plot <- inner_join(gmm82plot, gmm83plot, by = "Peptide")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = gmm84plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("Smoothed HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +  # Adding smoothing for all peptides
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# cluster trajectories
lcga2c <- ggplot(data = gmm84plot, 
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


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# peptide in cluster
list2 <- gmm84plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list2


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmm9 <- hlme(HDX ~ Time + condition + Time*condition,
             subject = "Peptide", 
             random = ~ SampleID,
             ng = 1, 
             data = sc4) 

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# scenario 3
fit_models1 <- function(min_k, max_k, data) {
  results <- list()
  
  for (k in min_k:max_k) {
    set.seed(123)
    start.time <- Sys.time()
    
    model <- hlme(HDX ~ Time + condition + Time*condition,
                  subject = "Peptide",
                  ng = k,
                  nwg=T, 
                  random = ~ SampleID,
                  data = data,
                  mixture = ~ Time,
                  B = gmm9)
    
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    
    results[[paste0("gmm", k)]] <- list(model = model, time_taken = time.taken)
  }
  return(results)
}


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
scenario3 <- fit_models1(min_k = 2, max_k = 10, data = sc4)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
scenario3$gmm2$time_taken
scenario3$gmm3$time_taken
scenario3$gmm4$time_taken


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# make table with results for the 3 models:
gmm10 <- scenario3$gmm2$model
gmm11 <- scenario3$gmm3$model
gmm12 <- scenario3$gmm4$model

summarytable(gmm9, gmm10, gmm11, gmm12)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
lcmm::postprob(gmm12)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
summary(gmm10)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# predicted trajectories
gmm121plot <- predictY(gmm12, sc4, var.time = "Time")
gmm122plot <- as.data.frame(gmm121plot$pred)
gmm122plot$Time <- gmm121plot$times[,1]
gmm122plot$HDX <- sc4$HDX_transformed
gmm122plot$Peptide <- sc4$Peptide


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# dataframe with postprob
gmm123plot <- gmm12$pprob %>% 
  mutate(max_prob = pmax(prob1, prob2, prob3, prob4))


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(gmm123plot, 
       aes(x = factor(class), y = max_prob)) + 
  geom_boxplot() + 
  ggtitle("lcmm") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# Compute percentages per cluster
per3 <- paste(round(100 * table(gmm123plot$class) / length(gmm123plot$class)), "%", sep = "")

# cluster labels
gmm123plot$cluster <- factor(gmm123plot$class,
                                labels = paste("Cluster ", 1:length(per3), " (", per3, ")", sep = ""))


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# Merge based on peptide column
gmm124plot <- inner_join(gmm122plot, gmm123plot, by = "Peptide")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = gmm124plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("Smoothed HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +  # Adding smoothing for all peptides
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# cluster trajectories
lcga3c <- ggplot(data = gmm124plot, 
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


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# peptide in cluster
list3 <- gmm124plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list3


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmm13 <- hlme(HDX_transformed ~ Time + condition + Time*condition,
             subject = "Peptide", 
             random = ~ SampleID,
             ng = 1, 
             data = sc6) 

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# scenario 4
fit_models2 <- function(min_k, max_k, data) {
  results <- list()
  
  for (k in min_k:max_k) {
    set.seed(123)
    start.time <- Sys.time()
    
    model <- hlme(HDX_transformed ~ Time + condition + Time*condition,
                  subject = "Peptide",
                  ng = k,
                  nwg=T, 
                  random = ~ SampleID,
                  data = data,
                  mixture = ~ Time,
                  B = gmm13)
    
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    
    results[[paste0("gmm", k)]] <- list(model = model, time_taken = time.taken)
  }
  return(results)
}


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
scenario4 <- fit_models2(min_k = 2, max_k = 10, data = sc6)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
scenario4$gmm2$time_taken
scenario4$gmm3$time_taken
scenario4$gmm4$time_taken


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# make table with results for the 3 models:
gmm14 <- scenario4$gmm2$model
gmm15 <- scenario4$gmm3$model
gmm16 <- scenario4$gmm4$model

summarytable(gmm13, gmm14, gmm15, gmm16)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
lcmm::postprob(gmm16)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
summary(gmm16)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# predicted trajectories
gmm161plot <- predictY(gmm16, sc6, var.time = "Time")
gmm162plot <- as.data.frame(gmm161plot$pred)
gmm162plot$Time <- gmm161plot$times[,1]
gmm162plot$HDX <- sc6$HDX_transformed
gmm162plot$Peptide <- sc6$Peptide
gmm162plot$Condition <- sc6$condition 


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# dataframe with postprob
gmm163plot <- gmm16$pprob %>% 
  mutate(max_prob = pmax(prob1, prob2, prob3, prob4))


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# Posterior cluster probability
lcgap4 <- ggplot(gmm163plot, 
       aes(x = factor(class), y = max_prob)) + 
  geom_boxplot() + 
  ggtitle("lcmm") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

lcgap4
# Save the plot
ggsave("lcgap4.png", plot = lcgap4, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# Compute percentages per cluster
per4 <- paste(round(100 * table(gmm163plot$class) / length(gmm163plot$class)), "%", sep = "")

# cluster labels
gmm163plot$cluster <- factor(gmm163plot$class,
                                labels = paste("Cluster ", 1:length(per4), " (", per4, ")", sep = ""))


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# Merge based on peptide column
gmm164plot <- inner_join(gmm162plot, gmm163plot, by = "Peptide")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = gmm164plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("Smoothed HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +  # Adding smoothing for all peptides
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# cluster trajectories
lcga4c <- ggplot(data = gmm164plot, 
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


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# peptide in cluster
list4 <- gmm164plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list4


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmm17 <- hlme(HDX_transformed ~ Time + condition + Time*condition,
             subject = "Peptide", 
             random = ~ SampleID,
             ng = 1, 
             data = sc7) 

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# scenario 5
fit_models3 <- function(min_k, max_k, data) {
  results <- list()
  
  for (k in min_k:max_k) {
    set.seed(123)
    start.time <- Sys.time()
    
    model <- hlme(HDX_transformed ~ Time + condition + Time*condition,
                  subject = "Peptide",
                  ng = k,
                  nwg=T, 
                  random = ~ SampleID,
                  data = data,
                  mixture = ~ Time,
                  B = gmm17)
    
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    
    results[[paste0("gmm", k)]] <- list(model = model, time_taken = time.taken)
  }
  return(results)
}


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
scenario5 <- fit_models3(min_k = 2, max_k = 10, data = sc7)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
scenario5$gmm2$time_taken
scenario5$gmm3$time_taken
scenario5$gmm4$time_taken


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# make table with results for the 3 models:
gmm18 <- scenario5$gmm2$model
gmm19 <- scenario5$gmm3$model
gmm20 <- scenario5$gmm4$model

summarytable(gmm17, gmm18, gmm19, gmm20)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
lcmm::postprob(gmm18)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
summary(gmm18)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# predicted trajectories
gmm201plot <- predictY(gmm18, sc7, var.time = "Time")
gmm202plot <- as.data.frame(gmm201plot$pred)
gmm202plot$Time <- gmm201plot$times[,1]
gmm202plot$HDX <- sc7$HDX_transformed
gmm202plot$Peptide <- sc7$Peptide


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# dataframe with postprob
gmm203plot <- gmm18$pprob %>% 
  mutate(max_prob = pmax(prob1, prob2))


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# Posterior cluster probability
lcgap5 <- ggplot(gmm203plot, 
       aes(x = factor(class), y = max_prob)) + 
  geom_boxplot() + 
  ggtitle("lcmm") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

lcgap5
# Save the plot
ggsave("lcgap5.png", plot = lcgap5, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# Compute percentages per cluster
per5 <- paste(round(100 * table(gmm203plot$class) / length(gmm203plot$class)), "%", sep = "")

# cluster labels
gmm203plot$cluster <- factor(gmm203plot$class,
                                labels = paste("Cluster ", 1:length(per5), " (", per5, ")", sep = ""))


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# Merge based on peptide column
gmm204plot <- inner_join(gmm202plot, gmm203plot, by = "Peptide")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = gmm204plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("Smoothed HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +  # Adding smoothing for all peptides
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# cluster trajectories
lcga5c <- ggplot(data = gmm204plot, 
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


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# peptide in cluster
list5 <- gmm204plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list5


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmm21 <- hlme(HDX_transformed ~ Time + condition + Time*condition,
             subject = "Peptide", 
             random = ~ SampleID,
             ng = 1, 
             data = sc9) 

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# scenario 6
fit_models4 <- function(min_k, max_k, data) {
  results <- list()
  
  for (k in min_k:max_k) {
    set.seed(123)
    start.time <- Sys.time()
    
    model <- hlme(HDX_transformed ~ Time + condition + Time*condition,
                  subject = "Peptide",
                  ng = k,
                  nwg=T, 
                  random = ~ SampleID,
                  data = data,
                  mixture = ~ Time,
                  B = gmm21)
    
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    
    results[[paste0("gmm", k)]] <- list(model = model, time_taken = time.taken)
  }
  return(results)
}


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
scenario6 <- fit_models4(min_k = 2, max_k = 10, data = sc9)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
scenario6$gmm2$time_taken
scenario6$gmm3$time_taken
scenario6$gmm4$time_taken


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# make table with results for the 3 models:
gmm22 <- scenario6$gmm2$model
gmm23 <- scenario6$gmm3$model
gmm24 <- scenario6$gmm4$model

summarytable(gmm21, gmm22, gmm23, gmm24)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
lcmm::postprob(gmm24)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
summary(gmm24)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# predicted trajectories
gmm231plot <- predictY(gmm24, sc9, var.time = "Time")
gmm232plot <- as.data.frame(gmm231plot$pred)
gmm232plot$Time <- gmm231plot$times[,1]
gmm232plot$HDX <- sc9$HDX_transformed
gmm232plot$Peptide <- sc9$Peptide
gmm232plot$Condition <- sc9$condition


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# dataframe with postprob
gmm233plot <- gmm24$pprob %>% 
  mutate(max_prob = pmax(prob1, prob2, prob3, prob4))


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# Posterior cluster probability
lcgap6 <- ggplot(gmm233plot, 
       aes(x = factor(class), y = max_prob)) + 
  geom_boxplot() + 
  ggtitle("lcmm") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

lcgap6
# Save the plot
ggsave("lcgap6.png", plot = lcgap6, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# Compute percentages per cluster
per6 <- paste(round(100 * table(gmm233plot$class) / length(gmm233plot$class)), "%", sep = "")

# cluster labels
gmm233plot$cluster <- factor(gmm233plot$class,
                                labels = paste("Cluster ", 1:length(per6), " (", per6, ")", sep = ""))


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# Merge based on peptide column
gmm234plot <- inner_join(gmm232plot, gmm233plot, by = "Peptide")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = gmm234plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("Smoothed HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +  # Adding smoothing for all peptides
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# cluster trajectories
lcga6c <- ggplot(data = gmm234plot, 
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


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
# peptide in cluster
list6 <- gmm234plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list6


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
#df1 <- gmm83plot %>% mutate(source = 'setting 1')
df2 <- gmm84plot %>% mutate(source = 'setting 2')
#df3 <- gmm203plot %>% mutate(source = 'setting 3')
df4 <- gmm164plot %>% mutate(source = 'setting 4')
df5 <- gmm204plot %>% mutate(source = 'setting 5')
df6 <- gmm234plot %>% mutate(source = 'setting 6')

# combine
lcmmmerge <- bind_rows(df2, df4, df5, df6)
lcmmmerge$source <- as.factor(lcmmmerge$source)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
lcmmbox <- ggplot(lcmmmerge, 
                  aes(x = factor(cluster), y = max_prob, color = source)) + 
  geom_boxplot() + 
  ggtitle("lcmm") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position = "right") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

lcmmbox

# Save the plot
ggsave("lcmmbox.png", plot = lcmmbox, width = 10, height = 8, dpi = 300)

