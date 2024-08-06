## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
#knitr::purl("lcmm_gmm.Rmd")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
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


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
sc1 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc1.csv")
sc2 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc2.csv")
sc3 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc3.csv")
sc4 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc4.csv")
sc5 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc5.csv")
sc6 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc6.csv")
sc7 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc7.csv")
sc8 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc8.csv")
sc9 <- read.csv("/Users/nangosyah/Desktop/MSc. Statistics & Data Science/2nd Year Semester 2/Master Thesis/Thesis_Project/Other methods/sc9.csv")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# data
llength <- c(8, 6, 5, 13, 2, 8, 6, 13, 6, 7, 4, 5, 4, 3, 2, 4, 2, 3, 11, 
          5, 9, 13, 5, 4, 4, 13, 7, 10, 12, 9, 2, 6, 9, 5, 9, 7, 3, 11, 
          11, 5, 6, 9, 13, 13, 8, 13, 4, 9, 11, 3, 5, 8, 8, 9, 8, 11, 5, 
          5, 9, 10, 8, 6, 4, 9, 4, 5, 8, 6, 8, 13, 10, 3, 8, 2, 8, 3, 8, 
          11, 11, 11, 9, 3, 8, 11, 3, 10, 7, 4, 3, 5, 2, 8, 6, 11, 10, 12, 3, 8, 2, 9)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# length
sc1length <- rep(llength, each = 11)
sc3length <- rep(llength, each = 11)
sc4length <- rep(llength, each = 6)
sc6length <- rep(llength, each = 6)
sc7length <- rep(llength, each = 4)
sc9length <- rep(llength, each = 4)

sc1$length <- sc1length
sc3$length <- sc3length
sc4$length <- sc4length
sc6$length <- sc6length
sc7$length <- sc7length
sc9$length <- sc9length

# deuterium content by exchange
sc1$HDX_transformed <- (sc1$HDX/sc1$length)
sc3$HDX_transformed <- (sc3$HDX/sc3$length)
sc4$HDX_transformed <- (sc4$HDX/sc4$length)
sc6$HDX_transformed <- (sc6$HDX/sc6$length)
sc7$HDX_transformed <- (sc7$HDX/sc7$length)
sc9$HDX_transformed <- (sc9$HDX/sc9$length)

# time
sc1$Time <- as.numeric(sc1$Time)
sc3$Time <- as.numeric(sc3$Time)
sc4$Time <- as.numeric(sc4$Time)
sc6$Time <- as.numeric(sc6$Time)
sc7$Time <- as.numeric(sc7$Time)
sc9$Time <- as.numeric(sc9$Time)

sc1$Time[sc1$Time == 1] <- 0
sc3$Time[sc3$Time == 1] <- 0
sc4$Time[sc4$Time == 1] <- 0
sc6$Time[sc6$Time == 1] <- 0
sc7$Time[sc7$Time == 1] <- 0
sc9$Time[sc9$Time == 1] <- 0

sc1$Time <- as.factor(sc1$Time)
sc3$Time <- as.factor(sc3$Time)
sc4$Time <- as.factor(sc4$Time)
sc6$Time <- as.factor(sc6$Time)
sc7$Time <- as.factor(sc7$Time)
sc9$Time <- as.factor(sc9$Time)

# condition
sc1$condition <- as.factor(sc1$condition)
sc3$condition <- as.factor(sc3$condition)
sc4$condition <- as.factor(sc4$condition)
sc6$condition <- as.factor(sc6$condition)
sc7$condition <- as.factor(sc7$condition)
sc9$condition <- as.factor(sc9$condition)

# Peptide
sc1$Peptide <- as.numeric(sc1$Peptide)
sc3$Peptide <- as.numeric(sc3$Peptide)
sc4$Peptide <- as.numeric(sc4$Peptide)
sc6$Peptide <- as.numeric(sc6$Peptide)
sc7$Peptide <- as.numeric(sc7$Peptide)
sc9$Peptide <- as.numeric(sc9$Peptide)

# sampleid
sc1$SampleID <- as.numeric(gsub("Sample ", "", sc1$SampleID))
sc3$SampleID <- as.numeric(gsub("Sample ", "", sc3$SampleID))
sc4$SampleID <- as.numeric(gsub("Sample ", "", sc4$SampleID))
sc6$SampleID <- as.numeric(gsub("Sample ", "", sc6$SampleID))
sc7$SampleID <- as.numeric(gsub("Sample ", "", sc7$SampleID))
sc9$SampleID <- as.numeric(gsub("Sample ", "", sc9$SampleID))


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmm1 <- hlme(HDX ~ Time + condition + Time*condition,
             subject = "Peptide", 
             random = ~ 1 + Time,
             ng = 1, 
             data = sc1) 

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmm2 <- hlme(HDX ~ Time + condition + Time*condition,
              subject = "Peptide",
              ng = 2,
              nwg=T, 
              random = ~ 1 + Time,
              mixture = ~ Time,
              data = sc1,
              B = gmm1) 

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmm3 <- hlme(HDX ~ Time + condition + Time*condition,
             subject = "Peptide",
             ng = 3,
             nwg=T, 
             random = ~ 1 + Time,
             mixture = ~ Time,
             data = sc1,
             B = gmm1)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmm4 <- hlme(HDX ~ Time + condition + Time*condition,
             subject = "Peptide",
             ng = 4,
             nwg=T, 
             random = ~ 1 + Time,
             mixture = ~ Time,
             data = sc1,
             B = gmm1) 

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
summarytable(gmm1, gmm2, gmm3, gmm4)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
summary(gmm4)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
lcmm::postprob(gmm4)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# predicted trajectories
gmm41plot <- predictY(gmm4, sc1, var.time = "Time")
gmm42plot <- as.data.frame(gmm41plot$pred)
gmm42plot$Time <- gmm41plot$times[,1]
gmm42plot$HDX <- sc1$HDX_transformed
gmm42plot$Peptide <- sc1$Peptide


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# posterior probabilities
gmm43plot <- gmm4$pprob %>% 
  mutate(max_prob = pmax(prob1, prob2, prob3, prob4))


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
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


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Compute percentages per cluster
per1 <- paste(round(100 * table(gmm43plot$class) / length(gmm43plot$class)), "%", sep = "")

# cluster labels
gmm43plot$cluster <- factor(gmm43plot$class,
                                labels = paste("Cluster ", 1:length(per1), " (", per1, ")", sep = ""))


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
gmm44plot <- inner_join(gmm42plot, gmm43plot, by = "Peptide")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = gmm44plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("Smoothed HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# cluster trajectories
lcga1c <- ggplot(data = gmm44plot, 
                 aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
          ggtitle("HDX Cluster Trajectories Over Time by Condition") +
          stat_summary(fun = mean, 
                       geom = "line", 
                       aes(group = cluster, color = cluster), 
                       linetype = "solid") +
          stat_summary(fun = mean, 
                       geom = "point", 
                       aes(group = cluster, color = cluster)) + 
          theme_bw() +
          theme(legend.position = "right",
                legend.title = element_text(size = 14),
                legend.text = element_text(size = 12),
                axis.title.x = element_text(size = 14, face = "bold"),
                axis.title.y = element_text(size = 14, face = "bold"),
                axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 12),
                plot.title = element_text(size = 16, face = "bold"),
                strip.text = element_text(size = 14)) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)

lcga1c
ggsave("lcga1c.png", plot = lcga1c, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# peptide in clusters
list1 <- gmm44plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list1


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmm5 <- hlme(HDX ~ Time + condition + Time*condition,
             subject = "Peptide", 
             random = ~ 1 + Time,
             ng = 1, 
             data = sc3) 

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
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
                  random = ~ 1 + Time,
                  data = data,
                  mixture = ~ Time,
                  B = gmm5)
    
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    
    results[[paste0("gmm", k)]] <- list(model = model, time_taken = time.taken)
  }
  return(results)
}


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
scenario2 <- fit_models(min_k = 2, max_k = 4, data = sc3)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
scenario2$gmm2$time_taken
scenario2$gmm3$time_taken
scenario2$gmm4$time_taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# make table with results for the 3 models:
gmm6 <- scenario2$gmm2$model
gmm7 <- scenario2$gmm3$model
gmm8 <- scenario2$gmm4$model

summarytable(gmm5, gmm6, gmm7, gmm8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
summary(gmm7)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
lcmm::postprob(gmm7)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# predicted trajectories
gmm81plot <- predictY(gmm7, sc3, var.time = "Time")
gmm82plot <- as.data.frame(gmm81plot$pred)
gmm82plot$Time <- gmm81plot$times[,1]
gmm82plot$HDX <- sc3$HDX_transformed
gmm82plot$Peptide <- sc3$Peptide


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# posterior probabilities
gmm83plot <- gmm8$pprob %>% 
  mutate(max_prob = pmax(prob1, prob2, prob3, prob4))


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
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
ggsave("lcgap2.png", plot = lcgap2, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Compute percentages per cluster
per2 <- paste(round(100 * table(gmm83plot$class) / length(gmm83plot$class)), "%", sep = "")

# cluster labels
gmm83plot$cluster <- factor(gmm83plot$class,
                                labels = paste("Cluster ", 1:length(per2), " (", per2, ")", sep = ""))


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
gmm84plot <- inner_join(gmm82plot, gmm83plot, by = "Peptide")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = gmm84plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("Smoothed HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# cluster trajectories
lcga2c <- ggplot(data = gmm84plot, 
                 aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
          ggtitle("HDX Cluster Trajectories Over Time by Condition") +
          stat_summary(fun = mean, 
                       geom = "line", 
                       aes(group = cluster, color = cluster), 
                       linetype = "solid") +
          stat_summary(fun = mean, 
                       geom = "point", 
                       aes(group = cluster, color = cluster)) +
          theme_bw() +
          theme(legend.position = "right",
                legend.title = element_text(size = 14),
                legend.text = element_text(size = 12),
                axis.title.x = element_text(size = 14, face = "bold"),
                axis.title.y = element_text(size = 14, face = "bold"),
                axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 12),
                plot.title = element_text(size = 16, face = "bold"),
                strip.text = element_text(size = 14)) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)

lcga2c
ggsave("lcga2c.png", plot = lcga2c, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# peptide in cluster
list2 <- gmm84plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list2


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmm9 <- hlme(HDX ~ Time + condition + Time*condition,
             subject = "Peptide", 
             random = ~ 1,
             ng = 1 + Time, 
             data = sc4) 

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
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
                  random = ~ 1 + Time,
                  data = data,
                  mixture = ~ Time,
                  B = gmm9)
    
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    
    results[[paste0("gmm", k)]] <- list(model = model, time_taken = time.taken)
  }
  return(results)
}


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
scenario3 <- fit_models1(min_k = 2, max_k = 4, data = sc4)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
scenario3$gmm2$time_taken
scenario3$gmm3$time_taken
scenario3$gmm4$time_taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# make table with results for the 3 models:
gmm10 <- scenario3$gmm2$model
gmm11 <- scenario3$gmm3$model
gmm12 <- scenario3$gmm4$model

summarytable(gmm9, gmm10, gmm11, gmm12)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
lcmm::postprob(gmm12)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
summary(gmm10)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# predicted trajectories
gmm121plot <- predictY(gmm12, sc4, var.time = "Time")
gmm122plot <- as.data.frame(gmm121plot$pred)
gmm122plot$Time <- gmm121plot$times[,1]
gmm122plot$HDX <- sc4$HDX_transformed
gmm122plot$Peptide <- sc4$Peptide


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# dataframe with postprob
gmm123plot <- gmm12$pprob %>% 
  mutate(max_prob = pmax(prob1, prob2, prob3, prob4))


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
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


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Compute percentages per cluster
per3 <- paste(round(100 * table(gmm123plot$class) / length(gmm123plot$class)), "%", sep = "")

# cluster labels
gmm123plot$cluster <- factor(gmm123plot$class,
                                labels = paste("Cluster ", 1:length(per3), " (", per3, ")", sep = ""))


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Merge based on peptide column
gmm124plot <- inner_join(gmm122plot, gmm123plot, by = "Peptide")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = gmm124plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("Smoothed HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# cluster trajectories
lcga3c <- ggplot(data = gmm124plot, 
                 aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
          ggtitle("HDX Cluster Trajectories Over Time by Condition") +
          stat_summary(fun = mean, 
                       geom = "line", 
                       aes(group = cluster, color = cluster), 
                       linetype = "solid") +
          stat_summary(fun = mean, 
                       geom = "point", 
                       aes(group = cluster, color = cluster)) +
          theme_bw() +
          theme(legend.position = "right",
                legend.title = element_text(size = 14),
                legend.text = element_text(size = 12),
                axis.title.x = element_text(size = 14, face = "bold"),
                axis.title.y = element_text(size = 14, face = "bold"),
                axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 12),
                plot.title = element_text(size = 16, face = "bold"),
                strip.text = element_text(size = 14)) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)

lcga3c
ggsave("lcga3c.png", plot = lcga3c, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# peptide in clusters
list3 <- gmm124plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list3


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmm13 <- hlme(HDX_transformed ~ Time + condition + Time*condition,
             subject = "Peptide", 
             random = ~ 1 + Time,
             ng = 1, 
             data = sc6) 

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
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
                  random = ~ 1 + Time,
                  data = data,
                  mixture = ~ Time,
                  B = gmm13)
    
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    
    results[[paste0("gmm", k)]] <- list(model = model, time_taken = time.taken)
  }
  return(results)
}


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
scenario4 <- fit_models2(min_k = 2, max_k = 4, data = sc6)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
scenario4$gmm2$time_taken
scenario4$gmm3$time_taken
scenario4$gmm4$time_taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# make table with results for the 3 models:
gmm14 <- scenario4$gmm2$model
gmm15 <- scenario4$gmm3$model
gmm16 <- scenario4$gmm4$model

summarytable(gmm13, gmm14, gmm15, gmm16)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
lcmm::postprob(gmm15)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
summary(gmm15)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# predicted trajectories
gmm161plot <- predictY(gmm15, sc6, var.time = "Time")
gmm162plot <- as.data.frame(gmm161plot$pred)
gmm162plot$Time <- gmm161plot$times[,1]
gmm162plot$HDX <- sc6$HDX_transformed
gmm162plot$Peptide <- sc6$Peptide
gmm162plot$Condition <- sc6$condition 


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# dataframe with postprob
gmm163plot <- gmm15$pprob %>% 
  mutate(max_prob = pmax(prob1, prob2, prob3))


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
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
ggsave("lcgap4.png", plot = lcgap4, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Compute percentages per cluster
per4 <- paste(round(100 * table(gmm163plot$class) / length(gmm163plot$class)), "%", sep = "")

# cluster labels
gmm163plot$cluster <- factor(gmm163plot$class,
                                labels = paste("Cluster ", 1:length(per4), " (", per4, ")", sep = ""))


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
gmm164plot <- inner_join(gmm162plot, gmm163plot, by = "Peptide")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = gmm164plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# cluster trajectories
lcga4c <- ggplot(data = gmm164plot, 
                 aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
          ggtitle("HDX Cluster Trajectories Over Time by Condition") +
          stat_summary(fun = mean, 
                       geom = "line", 
                       aes(group = cluster, color = cluster), 
                       linetype = "solid") +
          stat_summary(fun = mean, 
                       geom = "point", 
                       aes(group = cluster, color = cluster)) + 
          theme_bw() +
          theme(legend.position = "right",
                legend.title = element_text(size = 14),
                legend.text = element_text(size = 12),
                axis.title.x = element_text(size = 14, face = "bold"),
                axis.title.y = element_text(size = 14, face = "bold"),
                axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 12),
                plot.title = element_text(size = 16, face = "bold"),
                strip.text = element_text(size = 14)) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)

lcga4c
ggsave("lcga4c.png", plot = lcga4c, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# peptide in clusters
list4 <- gmm164plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list4


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmm17 <- hlme(HDX_transformed ~ Time + condition + Time*condition,
             subject = "Peptide", 
             random = ~ 1 + Time,
             ng = 1, 
             data = sc7) 

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
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
                  random = ~ 1 + Time,
                  data = data,
                  mixture = ~ Time,
                  B = gmm17)
    
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    
    results[[paste0("gmm", k)]] <- list(model = model, time_taken = time.taken)
  }
  return(results)
}


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
scenario5 <- fit_models3(min_k = 2, max_k = 4, data = sc7)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
scenario5$gmm2$time_taken
scenario5$gmm3$time_taken
scenario5$gmm4$time_taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# make table with results for the 3 models:
gmm18 <- scenario5$gmm2$model
gmm19 <- scenario5$gmm3$model
gmm20 <- scenario5$gmm4$model

summarytable(gmm17, gmm18, gmm19, gmm20)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
lcmm::postprob(gmm19)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
summary(gmm19)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# predicted trajectories
gmm201plot <- predictY(gmm19, sc7, var.time = "Time")
gmm202plot <- as.data.frame(gmm201plot$pred)
gmm202plot$Time <- gmm201plot$times[,1]
gmm202plot$HDX <- sc7$HDX_transformed
gmm202plot$Peptide <- sc7$Peptide
gmm202plot$Condition <- sc7$condition


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# posterior probabilities
gmm203plot <- gmm19$pprob %>% 
  mutate(max_prob = pmax(prob1, prob2, prob3))


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
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
ggsave("lcgap5.png", plot = lcgap5, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Compute percentages per cluster
per5 <- paste(round(100 * table(gmm203plot$class) / length(gmm203plot$class)), "%", sep = "")

# cluster labels
gmm203plot$cluster <- factor(gmm203plot$class,
                                labels = paste("Cluster ", 1:length(per5), " (", per5, ")", sep = ""))


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Merge based on peptide column
gmm204plot <- inner_join(gmm202plot, gmm203plot, by = "Peptide")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = gmm204plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# cluster trajectories
lcga5c <- ggplot(data = gmm204plot, 
                 aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
          ggtitle("HDX Cluster Trajectories Over Time by Condition") +
          stat_summary(fun = mean, 
                       geom = "line", 
                       aes(group = cluster, color = cluster), 
                       linetype = "solid") +
          stat_summary(fun = mean, 
                       geom = "point", 
                       aes(group = cluster, color = cluster)) +
          theme_bw() +
          theme(legend.position = "right",
                legend.title = element_text(size = 14),
                legend.text = element_text(size = 12),
                axis.title.x = element_text(size = 14, face = "bold"),
                axis.title.y = element_text(size = 14, face = "bold"),
                axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 12),
                plot.title = element_text(size = 16, face = "bold"),
                strip.text = element_text(size = 14)) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)

lcga5c
ggsave("lcga5c.png", plot = lcga5c, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# peptide in clusters
list5 <- gmm204plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list5


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmm21 <- hlme(HDX_transformed ~ Time + condition + Time*condition,
             subject = "Peptide", 
             random = ~ Time,
             ng = 1, 
             data = sc9) 

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
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
                  random = ~ Time,
                  data = data,
                  mixture = ~ Time,
                  B = gmm21)
    
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    
    results[[paste0("gmm", k)]] <- list(model = model, time_taken = time.taken)
  }
  return(results)
}


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
scenario6 <- fit_models4(min_k = 2, max_k = 4, data = sc9)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
scenario6$gmm2$time_taken
scenario6$gmm3$time_taken
scenario6$gmm4$time_taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# make table with results for the 3 models:
gmm22 <- scenario6$gmm2$model
gmm23 <- scenario6$gmm3$model
gmm24 <- scenario6$gmm4$model

summarytable(gmm21, gmm22, gmm23, gmm24)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
lcmm::postprob(gmm22)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
summary(gmm22)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# predicted trajectories
gmm231plot <- predictY(gmm22, sc9, var.time = "Time")
gmm232plot <- as.data.frame(gmm231plot$pred)
gmm232plot$Time <- gmm231plot$times[,1]
gmm232plot$HDX <- sc9$HDX_transformed
gmm232plot$Peptide <- sc9$Peptide
gmm232plot$Condition <- sc9$condition


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# posterior probabilities
gmm233plot <- gmm22$pprob %>% 
  mutate(max_prob = pmax(prob1, prob2))


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
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
ggsave("lcgap6.png", plot = lcgap6, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Compute percentages per cluster
per6 <- paste(round(100 * table(gmm233plot$class) / length(gmm233plot$class)), "%", sep = "")

# cluster labels
gmm233plot$cluster <- factor(gmm233plot$class,
                                labels = paste("Cluster ", 1:length(per6), " (", per6, ")", sep = ""))


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
gmm234plot <- inner_join(gmm232plot, gmm233plot, by = "Peptide")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = gmm234plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# cluster trajectories
lcga6c <- ggplot(data = gmm234plot, 
                 aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
          ggtitle("HDX Cluster Trajectories Over Time by Condition") +
          stat_summary(fun = mean, 
                       geom = "line", 
                       aes(group = cluster, color = cluster), 
                       linetype = "solid") +
          stat_summary(fun = mean, 
                       geom = "point", 
                       aes(group = cluster, color = cluster)) + 
          theme_bw() +
          theme(legend.position = "right",
                legend.title = element_text(size = 14),
                legend.text = element_text(size = 12),
                axis.title.x = element_text(size = 14, face = "bold"),
                axis.title.y = element_text(size = 14, face = "bold"),
                axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 12),
                plot.title = element_text(size = 16, face = "bold"),
                strip.text = element_text(size = 14)) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)

lcga6c
ggsave("lcga6c.png", plot = lcga6c, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# peptide in clusters
list6 <- gmm234plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list6


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
df1 <- lcga44plot  %>% mutate(source = 'setting 1')
df2 <- lcga84plot  %>% mutate(source = 'setting 2')
df3 <- lcga124plot %>% mutate(source = 'setting 3')
df4 <- lcga164plot %>% mutate(source = 'setting 4')
df5 <- lcga204plot %>% mutate(source = 'setting 5')
df6 <- lcga234plot %>% mutate(source = 'setting 6')

# combine
lcmmgmmmerge <- bind_rows(df1, df2, df3, df4, df5, df6)
lcmmgmmmerge$source <- as.factor(lcmmgmmmmerge$source)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
lcmmgmmmbox <- ggplot(lcmmgmmmerge, 
                  aes(x = factor(cluster), y = max_prob, color = source)) + 
  geom_boxplot() + 
  ggtitle("lcmm") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position = "right") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~source)

lcmmgmmmbox
ggsave("lcmmgmmmbox.png", plot = lcmmgmmmbox, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
library(matrixStats)
library(assertthat)

# metrics
appa <- function(pp) {
  rowMaxs(pp) %>% mean()
}

entropy <- function(pp) {
  assert_that(is.matrix(pp), min(pp) >= 0, max(pp) <= 1)
  pp = pmax(pp, .Machine$double.xmin)
  -sum(rowSums(pp * log(pp)))
}

relativeEntropy <- function(pp) {
  N = nrow(pp)
  K = ncol(pp)
  1 - entropy(pp) / (N * log(K))
}

confusionMatrix <- function(pp) {
  IMIFA::post_conf_mat(pp)
}


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Extract posterior probabilities
pp1 <- lcmm::postprob(lcga4)
pp2 <- lcmm::postprob(lcga8)
pp3 <- lcmm::postprob(lcga12)
pp4 <- lcmm::postprob(lcga16)
pp5 <- lcmm::postprob(lcga20)
pp6 <- lcmm::postprob(lcga24)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# matrices for each classification table
pp1 <- matrix(c(0.9196, 0.0000, 0.0804, 0.0000,
                0.0000, 0.9719, 0.0281, 0.0000,
                0.0027, 0.0175, 0.9798, 0.0000,
                0.0000, 0.0081, 0.0000, 0.9919), 
              nrow = 4, byrow = TRUE)

pp2 <- matrix(c(0.9765, 0.0235, 0.0000, 0.0000,
                0.0361, 0.8970, 0.0669, 0.0000,
                0.0000, 0.0342, 0.9306, 0.0352,
                0.0000, 0.0000, 0.0231, 0.9769), 
              nrow = 4, byrow = TRUE)

pp3 <- matrix(c(1.0000, 0.0000, 0.0000, 0.0000,
                0.0000, 0.9468, 0.0532, 0.0000,
                0.0000, 0.0513, 0.9487, 0.0000,
                0.0000, 0.0000, 0.0000, 1.0000), 
              nrow = 4, byrow = TRUE)

pp4 <- matrix(c(0.8543, 0.0003, 0.1454, 0.0000,
                0.0000, 0.8742, 0.0953, 0.0304,
                0.0633, 0.1052, 0.8315, 0.0000,
                0.0000, 0.0486, 0.0000, 0.9514), 
              nrow = 4, byrow = TRUE)

pp5 <- matrix(c(1.0000, 0.0000, 0.0000, 0.0000,
                0.0000, 0.9084, 0.0916, 0.0000,
                0.0000, 0.0921, 0.9079, 0.0000,
                0.0000, 0.0003, 0.0000, 0.9997), 
              nrow = 4, byrow = TRUE)

pp6 <- matrix(c(0.8066, 0.0013, 0.1921, 0.0000,
                0.0002, 0.7714, 0.1726, 0.0557,
                0.0824, 0.1685, 0.7491, 0.0000,
                0.0000, 0.0581, 0.0005, 0.9413), 
              nrow = 4, byrow = TRUE)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Calculate APPA
appa1 <- appa(pp1)
appa2 <- appa(pp2)
appa3 <- appa(pp3)
appa4 <- appa(pp4)
appa5 <- appa(pp5)
appa6 <- appa(pp6)

print(paste("APPA1:", round((appa1),2)))
print(paste("APPA2:", round((appa2),2)))
print(paste("APPA3:", round((appa3),2)))
print(paste("APPA4:", round((appa4),2)))
print(paste("APPA5:", round((appa5),2)))
print(paste("APPA6:", round((appa6),2)))


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Calculate Entropy
entropy1 <- entropy(pp1)
entropy2 <- entropy(pp2)
entropy3 <- entropy(pp3)
entropy4 <- entropy(pp4)
entropy5 <- entropy(pp5)
entropy6 <- entropy(pp6)

print(paste("entropy1:", round((entropy1),2)))
print(paste("entropy2:", round((entropy2),2)))
print(paste("entropy3:", round((entropy3),2)))
print(paste("entropy4:", round((entropy4),2)))
print(paste("entropy5:", round((entropy5),2)))
print(paste("entropy6:", round((entropy6),2)))


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Calculate Entropy
Rentropy1 <- relativeEntropy(pp1)
Rentropy2 <- relativeEntropy(pp2)
Rentropy3 <- relativeEntropy(pp3)
Rentropy4 <- relativeEntropy(pp4)
Rentropy5 <- relativeEntropy(pp5)
Rentropy6 <- relativeEntropy(pp6)

print(paste("Rentropy1:", round((entropy1),2)))
print(paste("Rentropy2:", round((entropy2),2)))
print(paste("Rentropy3:", round((entropy3),2)))
print(paste("Rentropy4:", round((entropy4),2)))
print(paste("Rentropy5:", round((entropy5),2)))
print(paste("Rentropy6:", round((entropy6),2)))


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------


