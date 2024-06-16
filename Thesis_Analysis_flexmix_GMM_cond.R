## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table) 
library(Rsubread)
library(lcmm)
library(latrend)
library(kableExtra)
library(flexmix)


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


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmmfflex1 <- stepFlexmix(.~ .|Peptide, k = 1:10, 
                         nrep = 1, 
                         model = FLXMRlmm(HDX_transformed ~ Time + condition + Time*condition, 
                                          random = ~ SampleID, 
                                          varFix=c(Random=F, Residual=T)), 
                         data = sc1, 
                         control=list(iter.max = 100, minprior = 0))

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# model-fit results
gmmfflex1


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#plot
plot(gmmfflex1)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# best model
gmmfflex_scene1 <- getModel(gmmfflex1, which=5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# parameters
parameters(gmmfflex_scene1)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# model summary
summary(gmmfflex_scene1)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# compute and plot the posterior cluster prob
gmmfflex1pp <- apply(posterior(gmmfflex_scene1),1,max)
gmmfflex1cluster <-  apply(posterior(gmmfflex_scene1),1,which.max)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# dataframe with postprob
gmmfflex1plot <- data.frame(Peptide = sc1$Peptide,
                            cluster = gmmfflex1cluster,
                            postprob = gmmfflex1pp)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
flexp1 <- ggplot(gmmfflex1plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

flexp1
# Save the plot
ggsave("flexp1.png", plot = flexp1, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Compute percentages per cluster
per1 <- paste(round(100 * table(gmmfflex1cluster) / length(gmmfflex1cluster)), "%", sep = "")

# cluster labels
gmmfflex1plot$cluster <- factor(gmmfflex1plot$cluster,
                                labels = paste("Cluster ", 1:length(per1), " (", per1, ")", sep = ""))

# add time and HDX
gmmfflex1plot$Time <- sc1$Time
gmmfflex1plot$HDX <- sc1$HDX_transformed
gmmfflex1plot$Condition <- sc1$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = gmmfflex1plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex1c <- ggplot(data = gmmfflex1plot,
                aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
          ggtitle("HDX Cluster Trajectories Over Time by Condition") +
          stat_summary(fun = mean, 
                       geom = "line", 
                       aes(group = cluster), 
                       linetype = "dashed", 
                       color = "black") + # Mean trajectory per cluster
          stat_summary(fun = mean, 
                       geom = "point", 
                       aes(group = cluster), 
                       color = "blue") + # Mean point per cluster
          stat_summary(fun.data = mean_se, 
                       geom = "errorbar", 
                       width = 0.3) + # Mean points per cluster with error bars
          theme_bw() +
          theme(legend.position = "right",
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)
flex1c
# Save the plot
ggsave("flex1c.png", plot = flex1c, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list1 <- gmmfflex1plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list1


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
pred1 <- predict(gmmfflex_scene1, sc1)
pred1 <- as.data.frame(pred1)
predicted1 <- apply(pred1, 1, which.max)
pred1$Time <- sc1$Time
pred1$HDX <- sc1$HDX_transformed
pred1$Peptide <- sc1$Peptide
pred1$Condition <- sc1$condition

# convert predictions to long format
predict1 <- gather(pred1, key = "Component", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

predict1$Predicted[predict1$Predicted < 0] <- 0

# Ensure the Component column is treated as a factor for better plotting
predict1$Cluster <- factor(predict1$Component,
                           levels = c("Comp.1", "Comp.2", "Comp.3", "Comp.4", "Comp.5"),
                           labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
gmmflexpred1 <- ggplot(data = predict1, 
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
  scale_color_manual(name = "HDX", values = c("Actual HDX" = "blue", "Predicted HDX" = "red")) +  # Customize color scale
  ggtitle("Actual vs. Predicted HDX Trajectories Over Time") +
  xlab("Time (secs)") +
  ylab("HDX") +
  theme_bw() +
  facet_grid(Condition ~ Component) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gmmflexpred1

# Save the plot
ggsave("gmmflexpred1.png", plot = gmmflexpred1, width = 10, height = 8, dpi = 300)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmmfflex2 <- stepFlexmix(.~ .|Peptide, k = 1:10, 
                         nrep = 1, 
                         model = FLXMRlmm(HDX_transformed ~ Time + condition + Time*condition, 
                                          random = ~ SampleID, 
                                          varFix=c(Random=F, Residual=T)), 
                         data = sc3, 
                         control=list(iter.max = 1000, minprior = 0))

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# model-fit results
gmmfflex2


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#plot
plot(gmmfflex2)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# best model
gmmfflex_scene2 <- getModel(gmmfflex2, which=5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# parameters
parameters(gmmfflex_scene2)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# summary
summary(gmmfflex_scene2)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# compute and plot the posterior cluster probability
gmmfflex2pp <- apply(posterior(gmmfflex_scene2),1,max)
gmmfflex2cluster <-  apply(posterior(gmmfflex_scene2),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
gmmfflex2plot <- data.frame(Peptide = sc3$Peptide,
                            cluster = gmmfflex2cluster,
                            postprob = gmmfflex2pp)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
flexp2 <- ggplot(gmmfflex2plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

flexp2
# Save the plot
ggsave("flexp2.png", plot = flexp2)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Compute percentages
per2 <- paste(round(100 * table(gmmfflex2cluster) / length(gmmfflex2cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
gmmfflex2plot$cluster <- factor(gmmfflex2plot$cluster,
                                labels = paste("Cluster ", 1:length(per2), " (", per2, ")", sep = ""))

# add columns 
gmmfflex2plot$Time <- sc3$Time
gmmfflex2plot$HDX <- sc3$HDX_transformed
gmmfflex2plot$Condition <- sc3$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = gmmfflex2plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex2c <- ggplot(data = gmmfflex2plot,
                aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
          ggtitle("HDX Cluster Trajectories Over Time by Condition") +
          stat_summary(fun = mean, 
                       geom = "line", 
                       aes(group = cluster), 
                       linetype = "dashed", 
                       color = "black") + # Mean trajectory per cluster
          stat_summary(fun = mean, 
                       geom = "point", 
                       aes(group = cluster), 
                       color = "blue") + # Mean point per cluster
          stat_summary(fun.data = mean_se, 
                       geom = "errorbar", 
                       width = 0.3) + # Mean points per cluster with error bars
          theme_bw() +
          theme(legend.position = "right",
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)
flex2c
# Save the plot
ggsave("flex2c.png", plot = flex2c, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list2 <- gmmfflex2plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list2


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
pred2 <- predict(gmmfflex_scene2, sc3)
pred2 <- as.data.frame(pred2)
predicted2 <- apply(pred2, 1, which.max)
pred2$Time <- sc3$Time
pred2$HDX <- sc3$HDX_transformed
pred2$Peptide <- sc3$Peptide
pred2$Condition <- sc3$condition

# convert predictions to long format
predict2 <- gather(pred2, key = "Component", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
predict2$Cluster <- factor(predict2$Component,
                           levels = c("Comp.1", "Comp.2", "Comp.3", "Comp.4", "Comp.5"),
                           labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
gmmflexpred2 <- ggplot(data = predict2, 
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
  scale_color_manual(name = "HDX", values = c("Actual HDX" = "blue", "Predicted HDX" = "red")) +  # Customize color scale
  ggtitle("Actual vs. Predicted HDX Trajectories Over Time") +
  xlab("Time (secs)") +
  ylab("HDX") +
  theme_bw() +
  facet_grid(Condition ~ Component) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gmmflexpred2

# Save the plot
ggsave("gmmflexpred2.png", plot = gmmflexpred2, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmmfflex3 <- stepFlexmix(.~ .|Peptide, k = 1:10, 
                         nrep = 1, 
                         model = FLXMRlmm(HDX_transformed ~ Time + condition + Time*condition, 
                                          random = ~ SampleID, 
                                          varFix=c(Random=F, Residual=T)), 
                         data = sc4, 
                         control=list(iter.max = 100, minprior = 0))

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# model-fit results
gmmfflex3


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#plot
plot(gmmfflex3)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# best model
gmmfflex_scene3 <- getModel(gmmfflex3, which=5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# parameters
parameters(gmmfflex_scene3)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# summary
summary(gmmfflex_scene3)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# compute and plot the posterior cluster probability
gmmfflex3pp <- apply(posterior(gmmfflex_scene3),1,max)
gmmfflex3cluster <-  apply(posterior(gmmfflex_scene3),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
gmmfflex3plot <- data.frame(Peptide = sc4$Peptide,
                            cluster = gmmfflex3cluster,
                            postprob = gmmfflex3pp)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Compute percentages
per3 <- paste(round(100 * table(gmmfflex3cluster) / length(gmmfflex3cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
gmmfflex3plot$cluster <- factor(gmmfflex3plot$cluster,
                                labels = paste("Cluster ", 1:length(per3), " (", per3, ")", sep = ""))

# add columns 
gmmfflex3plot$Time <- sc4$Time
gmmfflex3plot$HDX <- sc4$HDX_transformed
gmmfflex3plot$Condition <- sc4$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
flexp3 <- ggplot(gmmfflex3plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

flexp3
# Save the plot
ggsave("flexp3.png", plot = flexp3)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = gmmfflex3plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex3c <- ggplot(data = gmmfflex3plot,
                aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
          ggtitle("HDX Cluster Trajectories Over Time by Condition") +
          stat_summary(fun = mean, 
                       geom = "line", 
                       aes(group = cluster), 
                       linetype = "dashed", 
                       color = "black") + # Mean trajectory per cluster
          stat_summary(fun = mean, 
                       geom = "point", 
                       aes(group = cluster), 
                       color = "blue") + # Mean point per cluster
          stat_summary(fun.data = mean_se, 
                       geom = "errorbar", 
                       width = 0.3) + # Mean points per cluster with error bars
          theme_bw() +
          theme(legend.position = "right",
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)
flex3c
# Save the plot
ggsave("flex3c.png", plot = flex3c, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list3 <- gmmfflex3plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list3


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
pred3 <- predict(gmmfflex_scene3, sc4)
pred3 <- as.data.frame(pred3)
predicted3 <- apply(pred3, 1, which.max)
pred3$Time <- sc4$Time
pred3$HDX <- sc4$HDX_transformed
pred3$Peptide <- sc4$Peptide
pred3$Condition <- sc4$condition

# convert predictions to long format
predict3 <- gather(pred3, key = "Component", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
predict3$Cluster <- factor(predict3$Component,
                           levels = c("Comp.1", "Comp.2", "Comp.3", "Comp.4", "Comp.5"),
                           labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
gmmflexpred3 <- ggplot(data = predict3, 
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
  scale_color_manual(name = "HDX", values = c("Actual HDX" = "blue", "Predicted HDX" = "red")) +  # Customize color scale
  ggtitle("Actual vs. Predicted HDX Trajectories Over Time") +
  xlab("Time (secs)") +
  ylab("HDX") +
  theme_bw() +
  facet_grid(Condition ~ Component) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gmmflexpred3

# Save the plot
ggsave("gmmflexpred3.png", plot = gmmflexpred3, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmmfflex4 <- stepFlexmix(.~ .|Peptide, k = 1:10, 
                         nrep = 1, 
                         model = FLXMRlmm(HDX_transformed ~ Time + condition + Time*condition, 
                                          random = ~ SampleID, 
                                          varFix=c(Random=F, Residual=T)), 
                         data = sc6, 
                         control=list(iter.max = 1000, minprior = 0))

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# model-fit results
gmmfflex4


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#plot
plot(gmmfflex4)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# best model
gmmfflex_scene4 <- getModel(gmmfflex4, which=5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# parameters
parameters(gmmfflex_scene4)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# summary
summary(gmmfflex_scene4)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# compute and plot the posterior cluster probability
gmmfflex4pp <- apply(posterior(gmmfflex_scene4),1,max)
gmmfflex4cluster <-  apply(posterior(gmmfflex_scene4),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
gmmfflex4plot <- data.frame(Peptide = sc6$Peptide,
                            cluster = gmmfflex4cluster,
                            postprob = gmmfflex4pp)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Compute percentages
per4 <- paste(round(100 * table(gmmfflex4cluster) / length(gmmfflex4cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
gmmfflex4plot$cluster <- factor(gmmfflex4plot$cluster,
                                labels = paste("Cluster ", 1:length(per4), " (", per4, ")", sep = ""))

# add columns 
gmmfflex4plot$Time <- sc6$Time
gmmfflex4plot$HDX <- sc6$HDX_transformed
gmmfflex4plot$Condition <- sc6$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
flexp4 <- ggplot(gmmfflex4plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

flexp4
# Save the plot
ggsave("flexp4.png", plot = flexp4)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = gmmfflex4plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex4c <- ggplot(data = gmmfflex4plot,
                aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
          ggtitle("HDX Cluster Trajectories Over Time by Condition") +
          stat_summary(fun = mean, 
                       geom = "line", 
                       aes(group = cluster), 
                       linetype = "dashed", 
                       color = "black") + # Mean trajectory per cluster
          stat_summary(fun = mean, 
                       geom = "point", 
                       aes(group = cluster), 
                       color = "blue") + # Mean point per cluster
          stat_summary(fun.data = mean_se, 
                       geom = "errorbar", 
                       width = 0.3) + # Mean points per cluster with error bars
          theme_bw() +
          theme(legend.position = "right",
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)
flex4c
# Save the plot
ggsave("flex4c.png", plot = flex4c, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list4 <- gmmfflex4plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list4


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
pred4 <- predict(gmmfflex_scene4, sc6)
pred4 <- as.data.frame(pred4)
predicted4 <- apply(pred4, 1, which.max)
pred4$Time <- sc6$Time
pred4$HDX <- sc6$HDX_transformed
pred4$Peptide <- sc6$Peptide
pred4$Condition <- sc6$condition

# convert predictions to long format
predict4 <- gather(pred4, key = "Component", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
predict4$Cluster <- factor(predict4$Component,
                           levels = c("Comp.1", "Comp.2", "Comp.3", "Comp.4", "Comp.5"),
                           labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
gmmflexpred4 <- ggplot(data = predict4, 
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
  scale_color_manual(name = "HDX", values = c("Actual HDX" = "blue", "Predicted HDX" = "red")) +  # Customize color scale
  ggtitle("Actual vs. Predicted HDX Trajectories Over Time") +
  xlab("Time (secs)") +
  ylab("HDX") +
  theme_bw() +
  facet_grid(Condition ~ Component) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gmmflexpred4

# Save the plot
ggsave("gmmflexpred4.png", plot = gmmflexpred4, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmmfflex5 <- stepFlexmix(.~ .|Peptide, k = 1:10, 
                         nrep = 1, 
                         model = FLXMRlmm(HDX_transformed ~ Time + condition + Time*condition, 
                                          random = ~ SampleID, 
                                          varFix=c(Random=F, Residual=T)), 
                         data = sc7, 
                         control=list(iter.max = 1000, minprior = 0))

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# model-fit results
gmmfflex5


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#plot
plot(gmmfflex5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# best model
gmmfflex_scene5 <- getModel(gmmfflex5, which=5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# parameters
parameters(gmmfflex_scene5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# summary
summary(gmmfflex_scene5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# compute and plot the posterior cluster probability
gmmfflex5pp <- apply(posterior(gmmfflex_scene5),1,max)
gmmfflex5cluster <-  apply(posterior(gmmfflex_scene5),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
gmmfflex5plot <- data.frame(Peptide = sc7$Peptide,
                            cluster = gmmfflex5cluster,
                            postprob = gmmfflex5pp)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Compute percentages
per5 <- paste(round(100 * table(gmmfflex5cluster) / length(gmmfflex5cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
gmmfflex5plot$cluster <- factor(gmmfflex5plot$cluster,
                                labels = paste("Cluster ", 1:length(per5), " (", per5, ")", sep = ""))

# add columns 
gmmfflex5plot$Time <- sc7$Time
gmmfflex5plot$HDX <- sc7$HDX_transformed
gmmfflex5plot$Condition <- sc7$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
flexp5 <- ggplot(gmmfflex5plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

flexp5
# Save the plot
ggsave("flexp5.png", plot = flexp5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = gmmfflex5plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex5c <- ggplot(data = gmmfflex5plot,
                aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
          ggtitle("HDX Cluster Trajectories Over Time by Condition") +
          stat_summary(fun = mean, 
                       geom = "line", 
                       aes(group = cluster), 
                       linetype = "dashed", 
                       color = "black") + # Mean trajectory per cluster
          stat_summary(fun = mean, 
                       geom = "point", 
                       aes(group = cluster), 
                       color = "blue") + # Mean point per cluster
          stat_summary(fun.data = mean_se, 
                       geom = "errorbar", 
                       width = 0.3) + # Mean points per cluster with error bars
          theme_bw() +
          theme(legend.position = "right",
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)
flex5c
# Save the plot
ggsave("flex5c.png", plot = flex5c, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list5 <- gmmfflex5plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list5


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
pred5 <- predict(gmmfflex_scene5, sc7)
pred5 <- as.data.frame(pred5)
predicted5 <- apply(pred5, 1, which.max)
pred5$Time <- sc7$Time
pred5$HDX <- sc7$HDX_transformed
pred5$Peptide <- sc7$Peptide
pred5$Condition <- sc7$condition

# convert predictions to long format
predict5 <- gather(pred5, key = "Component", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
predict5$Cluster <- factor(predict5$Component,
                           levels = c("Comp.1", "Comp.2", "Comp.3", "Comp.4", "Comp.5"),
                           labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
gmmflexpred5 <- ggplot(data = predict5, 
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
  scale_color_manual(name = "HDX", values = c("Actual HDX" = "blue", "Predicted HDX" = "red")) +  # Customize color scale
  ggtitle("Actual vs. Predicted HDX Trajectories Over Time") +
  xlab("Time (secs)") +
  ylab("HDX") +
  theme_bw() +
  facet_grid(Condition ~ Component) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gmmflexpred5

# Save the plot
ggsave("gmmflexpred5.png", plot = gmmflexpred5, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gmmfflex6 <- stepFlexmix(.~ .|Peptide, k = 1:10, 
                         nrep = 1, 
                         model = FLXMRlmm(HDX_transformed ~ Time + condition + Time*condition, 
                                          random = ~ SampleID, 
                                          varFix=c(Random=F, Residual=T)), 
                         data = sc9, 
                         control=list(iter.max = 1000, minprior = 0))

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# model-fit results
gmmfflex6


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#plot
plot(gmmfflex6)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# best model
gmmfflex_scene6 <- getModel(gmmfflex6, which=4)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# parameters
parameters(gmmfflex_scene6)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# summary
summary(gmmfflex_scene6)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# compute and plot the posterior cluster probability
gmmfflex6pp <- apply(posterior(gmmfflex_scene6),1,max)
gmmfflex6cluster <-  apply(posterior(gmmfflex_scene6),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
gmmfflex6plot <- data.frame(Peptide = sc9$Peptide,
                            cluster = gmmfflex6cluster,
                            postprob = gmmfflex6pp)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Compute percentages
per6 <- paste(round(100 * table(gmmfflex6cluster) / length(gmmfflex6cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
gmmfflex6plot$cluster <- factor(gmmfflex6plot$cluster,
                                labels = paste("Cluster ", 1:length(per6), " (", per6, ")", sep = ""))

# add columns 
gmmfflex6plot$Time <- sc9$Time
gmmfflex6plot$HDX <- sc9$HDX_transformed
gmmfflex6plot$Condition <- sc9$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
flexp6 <- ggplot(gmmfflex6plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

flexp6
# Save the plot
ggsave("flexp6.png", plot = flexp6)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = gmmfflex6plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex6c <- ggplot(data = gmmfflex6plot,
                aes(x = Time, y = HDX, group = interaction(cluster, Condition), color = cluster)) +
          ggtitle("HDX Cluster Trajectories Over Time by Condition") +
          stat_summary(fun = mean, 
                       geom = "line", 
                       aes(group = cluster), 
                       linetype = "dashed", 
                       color = "black") + # Mean trajectory per cluster
          stat_summary(fun = mean, 
                       geom = "point", 
                       aes(group = cluster), 
                       color = "blue") + # Mean point per cluster
          stat_summary(fun.data = mean_se, 
                       geom = "errorbar", 
                       width = 0.3) + # Mean points per cluster with error bars
          theme_bw() +
          theme(legend.position = "right",
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)
flex6c
# Save the plot
ggsave("flex6c.png", plot = flex6c, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list6 <- gmmfflex6plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list6


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
pred6 <- predict(gmmfflex_scene6, sc9)
pred6 <- as.data.frame(pred6)
predicted6 <- apply(pred6, 1, which.max)
pred6$Time <- sc9$Time
pred6$HDX <- sc9$HDX_transformed
pred6$Peptide <- sc9$Peptide
pred6$Condition <- sc9$condition

# convert predictions to long format
predict6 <- gather(pred6, key = "Component", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
predict6$Cluster <- factor(predict6$Component,
                           levels = c("Comp.1", "Comp.2", "Comp.3", "Comp.4", "Comp.5"),
                           labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
gmmflexpred6 <- ggplot(data = predict6, 
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
      scale_color_manual(name = "HDX", values = c("Actual HDX" = "blue", "Predicted HDX" = "red")) +  # Customize color scale
      ggtitle("Actual vs. Predicted HDX Trajectories Over Time") +
      xlab("Time (secs)") +
      ylab("HDX") +
      theme_bw() +
      facet_grid(Condition ~ Component) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

gmmflexpred6

# Save the plot
ggsave("gmmflexpred6.png", plot = gmmflexpred6, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
df1 <- gmmfflex1plot %>% mutate(source = 'setting 1')
df2 <- gmmfflex2plot %>% mutate(source = 'setting 2')
df3 <- gmmfflex3plot %>% mutate(source = 'setting 3')
df4 <- gmmfflex4plot %>% mutate(source = 'setting 4')
df5 <- gmmfflex5plot %>% mutate(source = 'setting 5')
df6 <- gmmfflex6plot %>% mutate(source = 'setting 6')

# combine
gmmmerge <- bind_rows(df1, df2, df3, df4, df5, df6)
gmmmerge$source <- as.factor(gmmmerge$source)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
gmmbox <- ggplot(gmmmerge, 
                  aes(x = factor(cluster), y = postprob, color = source)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

gmmbox

# Save the plot
ggsave("gmmbox.png", plot = gmmbox, width = 10, height = 8, dpi = 300)

