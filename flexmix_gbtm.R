## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
#knitr::purl("flexmix_gbtm.Rmd")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
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
llength <- c(8, 6, 5, 13, 2, 8, 6, 13, 6, 7, 4, 5, 4, 3, 2, 4, 2, 3, 11, 5, 9, 
          13, 5, 4, 4, 13, 7, 10, 12, 9, 2, 6, 9, 5, 9, 7, 3, 11, 11, 5, 6, 
          9, 13, 13, 8, 13, 4, 9, 11, 3, 5, 8, 8, 9, 8, 11, 5, 5, 9, 10, 8, 
          6, 4, 9, 4, 5, 8, 6, 8, 13, 10, 3, 8, 2, 8, 3, 8, 11, 11, 11, 9, 
          3, 8, 11, 3, 10, 7, 4, 3, 5, 2, 8, 6, 11, 10, 12, 3, 8, 2, 9)


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

cgbtm1 <- stepFlexmix(.~.|Peptide, k = 1:10,
                       model = FLXMRglm(HDX_transformed ~ Time*condition),
                       data = sc1)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# model-fit results
cgbtm1


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# plot
plot(cgbtm1)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# best model
cgbtmflex1 <- getModel(cgbtm1, "BIC")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# parameters
parameters(cgbtmflex1)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# posterior probabilities and cluster probabilities
cgbtmflex1pp <- apply(posterior(cgbtmflex1),1,max)
cgbtmflex1cluster <-  apply(posterior(cgbtmflex1),1,which.max)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
cgbtmflex1plot <- data.frame(Peptide = sc1$Peptide,
                            cluster = cgbtmflex1cluster,
                            postprob = cgbtmflex1pp)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(cgbtmflex1plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Compute percentages per cluster
per1 <- paste(round(100 * table(cgbtmflex1cluster) / length(cgbtmflex1cluster)), "%", sep = "")

# cluster labels
cgbtmflex1plot$cluster <- factor(cgbtmflex1plot$cluster,
                                labels = paste("Cluster ", 1:length(per1), " (", per1, ")", sep = ""))

# add time, HDX & condition
cgbtmflex1plot$Time <- sc1$Time
cgbtmflex1plot$HDX <- sc1$HDX_transformed
cgbtmflex1plot$Condition <- sc1$condition


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = cgbtmflex1plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex1ct <- ggplot(data = cgbtmflex1plot, 
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
                strip.text = element_text(size = 14)
                ) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)
flex1ct
ggsave("flex1ct.png", plot = flex1ct, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# peptide in clusters
list1 <- cgbtmflex1plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list1


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

cgbtm2 <- stepFlexmix(.~.|Peptide, k = 1:10,
                       model = FLXMRglm(HDX_transformed ~ Time + Time*condition ),
                       data = sc3)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# model-fit results
cgbtm2


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# plot
plot(cgbtm2)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# best model
cgbtmflex2 <- getModel(cgbtm2, "BIC")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# parameters
parameters(cgbtmflex2)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# summary
summary(cgbtmflex2)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# posterior probabilities and cluster probabilities
cgbtmflex2pp <- apply(posterior(cgbtmflex2),1,max)
cgbtmflex2cluster <-  apply(posterior(cgbtmflex2),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
cgbtmflex2plot <- data.frame(Peptide = sc3$Peptide,
                            cluster = cgbtmflex2cluster,
                            postprob = cgbtmflex2pp)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(cgbtmflex2plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Compute percentages
per2 <- paste(round(100 * table(cgbtmflex2cluster) / length(cgbtmflex2cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
cgbtmflex2plot$cluster <- factor(cgbtmflex2plot$cluster,
                                labels = paste("Cluster ", 1:length(per2), " (", per2, ")", sep = ""))

# add columns time, HDX & condition
cgbtmflex2plot$Time <- sc3$Time
cgbtmflex2plot$HDX <- sc3$HDX_transformed
cgbtmflex2plot$Condition <- sc3$condition


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = cgbtmflex2plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
  ggtitle("HDX Trajectories Over Time") +
  geom_smooth(method = "loess", se = FALSE, size = 0.5) +  # Adding smoothing for all peptides
  theme_bw() +
  theme(legend.position = "right") +
  xlab("Time (secs)") +
  ylab("HDX") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Condition)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# cluster trajectories
flex2ct <- ggplot(data = cgbtmflex2plot, 
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
                strip.text = element_text(size = 14)
                ) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)

flex2ct
ggsave("flex2ct.png", plot = flex2ct, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# peptide in clusters
list2 <- cgbtmflex2plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list2


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

cgbtm3 <- stepFlexmix(.~.|Peptide, k = 1:10,
                       model = FLXMRglm(HDX_transformed ~ Time*condition),
                       data = sc4)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# model-fit results
cgbtm3


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# plot
plot(cgbtm3)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# best model
cgbtmflex3 <- getModel(cgbtm3, "BIC")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# parameters
parameters(cgbtmflex3) 


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# summary
summary(cgbtmflex3) 


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# posterior probabilities and cluster probabilities
cgbtmflex3pp <- apply(posterior(cgbtmflex3),1,max)
cgbtmflex3cluster <-  apply(posterior(cgbtmflex3),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
cgbtmflex3plot <- data.frame(Peptide = sc4$Peptide,
                            cluster = cgbtmflex3cluster,
                            postprob = cgbtmflex3pp)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Compute percentages
per3 <- paste(round(100 * table(cgbtmflex3cluster) / length(cgbtmflex3cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
cgbtmflex3plot$cluster <- factor(cgbtmflex3plot$cluster,
                                labels = paste("Cluster ", 1:length(per3), " (", per3, ")", sep = ""))

# add columns 
cgbtmflex3plot$Time <- sc4$Time
cgbtmflex3plot$HDX <- sc4$HDX_transformed
cgbtmflex3plot$Condition <- sc4$condition


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(cgbtmflex3plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = cgbtmflex3plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex3ct <- ggplot(data = cgbtmflex3plot, 
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
                strip.text = element_text(size = 14)
                ) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)

flex3ct
ggsave("flex3ct.png", plot = flex3ct, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# peptide in clusters
list3 <- cgbtmflex3plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list3


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

cgbtm4 <- stepFlexmix(.~.|Peptide, k = 1:10,
                       model = FLXMRglm(HDX_transformed ~ Time*condition),
                       data = sc6)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# model-fit results
cgbtm4


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# plot
plot(cgbtm4)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# best model
cgbtmflex4 <- getModel(cgbtm4, "BIC")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# parameters
parameters(cgbtmflex4)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# posterior probabilities and cluster probabilities
cgbtmflex4pp <- apply(posterior(cgbtmflex4),1,max)
cgbtmflex4cluster <-  apply(posterior(cgbtmflex4),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
cgbtmflex4plot <- data.frame(Peptide = sc6$Peptide,
                            cluster = cgbtmflex4cluster,
                            postprob = cgbtmflex4pp)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Compute percentages
per4 <- paste(round(100 * table(cgbtmflex4cluster) / length(cgbtmflex4cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
cgbtmflex4plot$cluster <- factor(cgbtmflex4plot$cluster,
                                labels = paste("Cluster ", 1:length(per4), " (", per4, ")", sep = ""))

# add columns 
cgbtmflex4plot$Time <- sc6$Time
cgbtmflex4plot$HDX <- sc6$HDX_transformed
cgbtmflex4plot$Condition <- sc6$condition


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(cgbtmflex4plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = cgbtmflex4plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex4ct <- ggplot(data = cgbtmflex4plot, 
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
                strip.text = element_text(size = 14)
                ) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)

flex4ct
ggsave("flex4ct.png", plot = flex4ct, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# peptide in clusters
list4 <- cgbtmflex4plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list4


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

cgbtm5 <- stepFlexmix(.~.|Peptide, k = 1:10,
                       model = FLXMRglm(HDX_transformed ~ Time*condition),
                       data = sc7)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# model-fit results
cgbtm5


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# plot
plot(cgbtm5)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# best model
cgbtmflex5 <- getModel(cgbtm5, "BIC")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# parameters
parameters(cgbtmflex5)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# posterior probabilities and cluster probabilities
cgbtmflex5pp <- apply(posterior(cgbtmflex5),1,max)
cgbtmflex5cluster <-  apply(posterior(cgbtmflex5),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
cgbtmflex5plot <- data.frame(Peptide = sc7$Peptide,
                            cluster = cgbtmflex5cluster,
                            postprob = cgbtmflex5pp)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Compute percentages
per5 <- paste(round(100 * table(cgbtmflex5cluster) / length(cgbtmflex5cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
cgbtmflex5plot$cluster <- factor(cgbtmflex5plot$cluster,
                                labels = paste("Cluster ", 1:length(per5), " (", per5, ")", sep = ""))

# add columns 
cgbtmflex5plot$Time <- sc7$Time
cgbtmflex5plot$HDX <- sc7$HDX_transformed
cgbtmflex5plot$Condition <- sc7$condition


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(cgbtmflex5plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = cgbtmflex5plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex5ct <- ggplot(data = cgbtmflex5plot, 
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
                strip.text = element_text(size = 14)
                ) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)

flex5ct
ggsave("flex5ct.png", plot = flex5ct, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# peptide in clusters
list5 <- cgbtmflex5plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list5


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

cgbtm6 <- stepFlexmix(.~.| Peptide, 
                      k = 1:10,
                      model = FLXMRglm(HDX_transformed ~ Time*condition),
                      data = sc9)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# model-fit results
cgbtm6


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# plot
plot(cgbtm6)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# best model
cgbtmflex6 <- getModel(cgbtm6, "BIC")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# parameters
parameters(cgbtmflex6 )


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# posterior probabilities and cluster probabilities
cgbtmflex6pp <- apply(posterior(cgbtmflex6),1,max)
cgbtmflex6cluster <-  apply(posterior(cgbtmflex6),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
cgbtmflex6plot <- data.frame(Peptide = sc9$Peptide,
                            cluster = cgbtmflex6cluster,
                            postprob = cgbtmflex6pp)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Compute percentages
per6 <- paste(round(100 * table(cgbtmflex6cluster) / length(cgbtmflex6cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
cgbtmflex6plot$cluster <- factor(cgbtmflex6plot$cluster,
                                labels = paste("Cluster ", 1:length(per6), " (", per6, ")", sep = ""))

# add time, HDX & condition
cgbtmflex6plot$Time <- sc9$Time
cgbtmflex6plot$HDX <- sc9$HDX_transformed
cgbtmflex6plot$Condition <- sc9$condition


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Posterior cluster probability
ggplot(cgbtmflex6plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = cgbtmflex6plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex6ct <- ggplot(data = cgbtmflex6plot, 
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
                strip.text = element_text(size = 14)
                ) +
          xlab("Time (secs)") +
          ylab("HDX") + 
          facet_wrap(.~Condition, ncol = 2)

flex6ct
ggsave("flex6ct.png", plot = flex6ct, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# peptide in clusters
list6 <- cgbtmflex6plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list6


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
df1 <- cgbtmflex1plot %>% mutate(source = 'setting 1')
df2 <- cgbtmflex2plot %>% mutate(source = 'setting 2')
df3 <- cgbtmflex3plot %>% mutate(source = 'setting 3')
df4 <- cgbtmflex4plot %>% mutate(source = 'setting 4')
df5 <- cgbtmflex5plot %>% mutate(source = 'setting 5')
df6 <- cgbtmflex6plot %>% mutate(source = 'setting 6')

# combine
gmmmerge <- bind_rows(df1, df2, df3, df4, df5, df6)
gmmmerge$source <- as.factor(gmmmerge$source)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
gbtmbox <- ggplot(gmmmerge, 
                  aes(x = factor(cluster), y = postprob, color = source)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position = "right") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~source)

gbtmbox
ggsave("gbtmbox.png", plot = gbtmbox, width = 10, height = 8)


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
pp1 <- posterior(cgbtmflex1)
pp2 <- posterior(cgbtmflex2)
pp3 <- posterior(cgbtmflex3)
pp4 <- posterior(cgbtmflex4)
pp5 <- posterior(cgbtmflex5)
pp6 <- posterior(cgbtmflex6)


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



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------


