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
data <- c(8, 6, 5, 13, 2, 8, 6, 13, 6, 7, 4, 5, 4, 3, 2, 4, 2, 3, 11, 5, 9, 
          13, 5, 4, 4, 13, 7, 10, 12, 9, 2, 6, 9, 5, 9, 7, 3, 11, 11, 5, 6, 
          9, 13, 13, 8, 13, 4, 9, 11, 3, 5, 8, 8, 9, 8, 11, 5, 5, 9, 10, 8, 
          6, 4, 9, 4, 5, 8, 6, 8, 13, 10, 3, 8, 2, 8, 3, 8, 11, 11, 11, 9, 
          3, 8, 11, 3, 10, 7, 4, 3, 5, 2, 8, 6, 11, 10, 12, 3, 8, 2, 9)


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

gbtmfflex1 <- stepFlexmix(~ Time | Peptide, k = 1:10,
                       model = FLXMRglm(HDX_transformed ~ Time + condition + Time*condition),
                       data = sc1)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# model-fit results
gbtmfflex1 


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#plot
plot(gbtmfflex1)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# best model
gbtmfflex_scene1 <- getModel(gbtmfflex1, which=6)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# parameters
parameters(gbtmfflex_scene1)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
print(plot(refit(gbtmfflex_scene1)))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# model summary
summary(gbtmfflex_scene1)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# compute and plot the posterior cluster prob
gbtmfflex1pp <- apply(posterior(gbtmfflex_scene1),1,max)
gbtmfflex1cluster <-  apply(posterior(gbtmfflex_scene1),1,which.max)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# dataframe with postprob
gbtmfflex1plot <- data.frame(Peptide = sc1$Peptide,
                            cluster = gbtmfflex1cluster,
                            postprob = gbtmfflex1pp)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(gbtmfflex1plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Compute percentages per cluster
per1 <- paste(round(100 * table(gbtmfflex1cluster) / length(gbtmfflex1cluster)), "%", sep = "")

# cluster labels
gbtmfflex1plot$cluster <- factor(gbtmfflex1plot$cluster,
                                labels = paste("Cluster ", 1:length(per1), " (", per1, ")", sep = ""))

# add time and HDX
gbtmfflex1plot$Time <- sc1$Time
gbtmfflex1plot$HDX <- sc1$HDX_transformed
gbtmfflex1plot$Condition <- sc1$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = gbtmfflex1plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex1ct <- ggplot(data = gbtmfflex1plot, 
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
flex1ct
# Save the plot
ggsave("flex1ct.png", plot = flex1ct, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list1 <- gbtmfflex1plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list1


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
pred1 <- predict(gbtmfflex_scene1, sc1)
pred1 <- as.data.frame(pred1)
predicted <- apply(pred1, 1, which.max)
pred1$Time <- sc1$Time
pred1$HDX <- sc1$HDX_transformed
pred1$Peptide <- sc1$Peptide
pred1$Condition <- sc1$condition

# convert predictions to long format
predict1 <- gather(pred1, key = "Component", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
predict1$Component <- factor(predict1$Component, levels = c("Comp.1", "Comp.2", "Comp.3", "Comp.4", "Comp.5"))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
flexpred1 <- ggplot(data = predict1, 
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

flexpred1

# Save the plot
ggsave("flexpred1.png", plot = flexpred1, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gbtmfflex2 <- stepFlexmix(~ Time | Peptide, k = 1:10,
                       model = FLXMRglm(HDX_transformed ~ Time + condition + Time*condition ),
                       data = sc3)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# model-fit results
gbtmfflex2


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#plot
plot(gbtmfflex2)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# best model
gbtmfflex_scene2 <- getModel(gbtmfflex2, which=6)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# parameters
parameters(gbtmfflex_scene2)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
print(plot(refit(gbtmfflex_scene2)))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# summary
summary(gbtmfflex_scene2)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# compute and plot the posterior cluster probability
gbtmfflex2pp <- apply(posterior(gbtmfflex_scene2),1,max)
gbtmfflex2cluster <-  apply(posterior(gbtmfflex_scene2),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
gbtmfflex2plot <- data.frame(Peptide = sc3$Peptide,
                            cluster = gbtmfflex2cluster,
                            postprob = gbtmfflex2pp)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(gbtmfflex2plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Compute percentages
per2 <- paste(round(100 * table(gbtmfflex2cluster) / length(gbtmfflex2cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
gbtmfflex2plot$cluster <- factor(gbtmfflex2plot$cluster,
                                labels = paste("Cluster ", 1:length(per2), " (", per2, ")", sep = ""))

# add columns 
gbtmfflex2plot$Time <- sc3$Time
gbtmfflex2plot$HDX <- sc3$HDX_transformed
gbtmfflex2plot$Condition <- sc3$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = gbtmfflex2plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex2ct <- ggplot(data = gbtmfflex2plot, 
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
flex2ct
# Save the plot
ggsave("flex2ct.png", plot = flex2ct, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list2 <- gbtmfflex2plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list2


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
pred2 <- predict(gbtmfflex_scene2, sc3)
pred2 <- as.data.frame(pred2)
predicted1 <- apply(pred2, 1, which.max)
pred2$Time <- sc3$Time
pred2$HDX <- sc3$HDX_transformed
pred2$Peptide <- sc3$Peptide
pred2$Condition <- sc3$condition

# convert predictions to long format
predict2 <- gather(pred2, key = "Component", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
predict2$Component <- factor(predict2$Component, levels = c("Comp.1", "Comp.2", "Comp.3", "Comp.4", "Comp.5", "Comp.6"))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
flexpred2 <- ggplot(data = predict2, 
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

flexpred2

# Save the plot
ggsave("flexpred2.png", plot = flexpred2, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gbtmfflex3 <- stepFlexmix(~ Time | Peptide, k = 1:10,
                       model = FLXMRglm(HDX_transformed ~ Time + condition + Time*condition),
                       data = sc4)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# model-fit results
gbtmfflex3


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#plot
plot(gbtmfflex3)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# best model
gbtmfflex_scene3 <- getModel(gbtmfflex3, which=5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# parameters
parameters(gbtmfflex_scene3) 


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#print(plot(refit(gbtmfflex_scene3)))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# summary
summary(gbtmfflex_scene3) 


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# compute and plot the posterior cluster probability
gbtmfflex3pp <- apply(posterior(gbtmfflex_scene3),1,max)
gbtmfflex3cluster <-  apply(posterior(gbtmfflex_scene3),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
gbtmfflex3plot <- data.frame(Peptide = sc4$Peptide,
                            cluster = gbtmfflex3cluster,
                            postprob = gbtmfflex3pp)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Compute percentages
per3 <- paste(round(100 * table(gbtmfflex3cluster) / length(gbtmfflex3cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
gbtmfflex3plot$cluster <- factor(gbtmfflex3plot$cluster,
                                labels = paste("Cluster ", 1:length(per3), " (", per3, ")", sep = ""))

# add columns 
gbtmfflex3plot$Time <- sc4$Time
gbtmfflex3plot$HDX <- sc4$HDX_transformed
gbtmfflex3plot$Condition <- sc4$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(gbtmfflex3plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = gbtmfflex3plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex3ct <- ggplot(data = gbtmfflex3plot, 
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
flex3ct
# Save the plot
ggsave("flex3ct.png", plot = flex3ct, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list3 <- gbtmfflex3plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list3


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
pred3 <- predict(gbtmfflex_scene3, sc4)
pred3 <- as.data.frame(pred3)
predicted2 <- apply(pred3, 1, which.max)
pred3$Time <- sc4$Time
pred3$HDX <- sc4$HDX_transformed
pred3$Peptide <- sc4$Peptide
pred3$Condition <- sc4$condition

# convert predictions to long format
predict3 <- gather(pred3, key = "Component", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
predict3$Component <- factor(predict3$Component, levels = c("Comp.1", "Comp.2", "Comp.3", "Comp.4", "Comp.5"))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
flexpred3 <- ggplot(data = predict3, 
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

flexpred3

# Save the plot
ggsave("flexpred3.png", plot = flexpred3, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gbtmfflex4 <- stepFlexmix(~ Time | Peptide, k = 1:10,
                       model = FLXMRglm(HDX_transformed ~ Time + condition + Time*condition),
                       data = sc6)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# model-fit results
gbtmfflex4


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#plot
plot(gbtmfflex4)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# best model
gbtmfflex_scene4 <- getModel(gbtmfflex4, which=7)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# parameters
parameters(gbtmfflex_scene4)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# summary
summary(gbtmfflex_scene4)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#print(plot(refit(gbtmfflex_scene4)))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# compute and plot the posterior cluster probability
gbtmfflex4pp <- apply(posterior(gbtmfflex_scene4),1,max)
gbtmfflex4cluster <-  apply(posterior(gbtmfflex_scene4),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
gbtmfflex4plot <- data.frame(Peptide = sc6$Peptide,
                            cluster = gbtmfflex4cluster,
                            postprob = gbtmfflex4pp)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Compute percentages
per4 <- paste(round(100 * table(gbtmfflex4cluster) / length(gbtmfflex4cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
gbtmfflex4plot$cluster <- factor(gbtmfflex4plot$cluster,
                                labels = paste("Cluster ", 1:length(per4), " (", per4, ")", sep = ""))

# add columns 
gbtmfflex4plot$Time <- sc6$Time
gbtmfflex4plot$HDX <- sc6$HDX_transformed
gbtmfflex4plot$Condition <- sc6$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(gbtmfflex4plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = gbtmfflex4plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex4ct <- ggplot(data = gbtmfflex4plot, 
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
flex4ct
# Save the plot
ggsave("flex4ct.png", plot = flex4ct, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list4 <- gbtmfflex4plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list4


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
pred4 <- predict(gbtmfflex_scene4, sc6)
pred4 <- as.data.frame(pred4)
predicted3 <- apply(pred4, 1, which.max)
pred4$Time <- sc6$Time
pred4$HDX <- sc6$HDX_transformed
pred4$Peptide <- sc6$Peptide
pred4$Condition <- sc6$condition

# convert predictions to long format
predict4 <- gather(pred4, key = "Component", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
predict4$Component <- factor(predict4$Component, levels = c("Comp.1", "Comp.2", "Comp.3", "Comp.4", "Comp.5"))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
flexpred4 <- ggplot(data = predict4, 
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

flexpred4

# Save the plot
ggsave("flexpred4.png", plot = flexpred4, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gbtmfflex5 <- stepFlexmix(~ Time | Peptide, k = 1:10,
                       model = FLXMRglm(HDX_transformed ~ Time + condition + Time*condition),
                       data = sc7)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# model-fit results
gbtmfflex5


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#plot
plot(gbtmfflex5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# best model
gbtmfflex_scene5 <- getModel(gbtmfflex5, which=8)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# parameters
parameters(gbtmfflex_scene5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#print(plot(refit(gbtmfflex_scene5)))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# summary
summary(gbtmfflex_scene5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# compute and plot the posterior cluster probability
gbtmfflex5pp <- apply(posterior(gbtmfflex_scene5),1,max)
gbtmfflex5cluster <-  apply(posterior(gbtmfflex_scene5),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
gbtmfflex5plot <- data.frame(Peptide = sc7$Peptide,
                            cluster = gbtmfflex5cluster,
                            postprob = gbtmfflex5pp)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Compute percentages
per5 <- paste(round(100 * table(gbtmfflex5cluster) / length(gbtmfflex5cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
gbtmfflex5plot$cluster <- factor(gbtmfflex5plot$cluster,
                                labels = paste("Cluster ", 1:length(per5), " (", per5, ")", sep = ""))

# add columns 
gbtmfflex5plot$Time <- sc7$Time
gbtmfflex5plot$HDX <- sc7$HDX_transformed
gbtmfflex5plot$Condition <- sc7$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(gbtmfflex5plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = gbtmfflex5plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex5ct <- ggplot(data = gbtmfflex5plot, 
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
flex5ct
# Save the plot
ggsave("flex5ct.png", plot = flex5ct, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list5 <- gbtmfflex5plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list5


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
pred5 <- predict(gbtmfflex_scene5, sc7)
pred5 <- as.data.frame(pred5)
predicted4 <- apply(pred5, 1, which.max)
pred5$Time <- sc7$Time
pred5$HDX <- sc7$HDX_transformed
pred5$Peptide <- sc7$Peptide
pred5$Condition <- sc7$condition

# convert predictions to long format
predict5 <- gather(pred5, key = "Component", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
predict5$Component <- factor(predict5$Component, levels = c("Comp.1", "Comp.2", "Comp.3", "Comp.4", "Comp.5"))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
flexpred5 <- ggplot(data = predict5, 
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

flexpred5

# Save the plot
ggsave("flexpred5.png", plot = flexpred5, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

gbtmfflex6 <- stepFlexmix(~ Time | Peptide, k = 1:10,
                       model = FLXMRglm(HDX_transformed ~ Time + condition + Time*condition),
                       data = sc9)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# model-fit results
gbtmfflex6


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
#plot
plot(gbtmfflex6)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# best model
gbtmfflex_scene6 <- getModel(gbtmfflex6, which=6)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# parameters
parameters(gbtmfflex_scene6)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
print(plot(refit(gbtmfflex_scene6)))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# summary
summary(gbtmfflex_scene6)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# compute and plot the posterior cluster probability
gbtmfflex6pp <- apply(posterior(gbtmfflex_scene6),1,max)
gbtmfflex6cluster <-  apply(posterior(gbtmfflex_scene6),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
gbtmfflex6plot <- data.frame(Peptide = sc9$Peptide,
                            cluster = gbtmfflex6cluster,
                            postprob = gbtmfflex6pp)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Compute percentages
per6 <- paste(round(100 * table(gbtmfflex6cluster) / length(gbtmfflex6cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
gbtmfflex6plot$cluster <- factor(gbtmfflex6plot$cluster,
                                labels = paste("Cluster ", 1:length(per6), " (", per6, ")", sep = ""))

# add columns 
gbtmfflex6plot$Time <- sc9$Time
gbtmfflex6plot$HDX <- sc9$HDX_transformed
gbtmfflex6plot$Condition <- sc9$condition


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(gbtmfflex6plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# all peptides
ggplot(data = gbtmfflex6plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex6ct <- ggplot(data = gbtmfflex6plot, 
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
flex6ct
# Save the plot
ggsave("flex6ct.png", plot = flex6ct, width = 10, height = 8, dpi = 300)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# peptide in cluster
list6 <- gbtmfflex6plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list6


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
# make predictions
pred6 <- predict(gbtmfflex_scene6, sc9)
pred6 <- as.data.frame(pred6)
predicted5 <- apply(pred6, 1, which.max)
pred6$Time <- sc9$Time
pred6$HDX <- sc9$HDX_transformed
pred6$Peptide <- sc9$Peptide
pred6$Condition <- sc9$condition

# convert predictions to long format
predict6 <- gather(pred6, key = "Component", value = "Predicted", -Time, -HDX, -Peptide, -Condition)

# Ensure the Component column is treated as a factor for better plotting
predict6$Component <- factor(predict6$Component, levels = c("Comp.1", "Comp.2", "Comp.3", "Comp.4", "Comp.5"))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
flexpred6 <- ggplot(data = predict6, 
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

flexpred6

# Save the plot
ggsave("flexpred6.png", plot = flexpred6, width = 10, height = 8, dpi = 300)

