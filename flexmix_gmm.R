## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
#knitr::purl("flexmix_gmm.Rmd")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table) 
library(Rsubread)
library(lme4)
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
llength <- c(8, 6, 5, 13, 2, 8, 6, 13, 6, 7, 4, 5, 4, 3, 2, 4, 2, 3, 11, 
          5, 9, 13, 5, 4, 4, 13, 7, 10, 12, 9, 2, 6, 9, 5, 9, 7, 3, 11, 
          11, 5, 6, 9, 13, 13, 8, 13, 4, 9, 11, 3, 5, 8, 8, 9, 8, 11, 5, 
          5, 9, 10, 8, 6, 4, 9, 4, 5, 8, 6, 8, 13, 10, 3, 8, 2, 8, 3, 8, 
          11, 11, 11, 9, 3, 8, 11, 3, 10, 7, 4, 3, 5, 2, 8, 6, 11, 10, 12, 3, 8, 2, 9)
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

# time numeric
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

# time factor
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

cgmm1 <- stepFlexmix(.~.|Peptide, 
                     k = 1:10, 
                     nrep = 5, 
                     model = FLXMRlmm(HDX_transformed ~ Time * condition, 
                                      random = ~ 1 + Time, 
                                      varFix = c(Random = FALSE,Residual = TRUE)),
                     data = sc1)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# model-fit results
cgmm1


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
#plot
plot(cgmm1)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# best model
cgmmflex1 <- getModel(cgmm1, "BIC")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# parameters
parameters(cgmmflex1)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# posterior and cluster probabilities
cgmmflex1pp <- apply(posterior(cgmmflex1),1,max)
cgmmflex1cluster <-  apply(posterior(cgmmflex1),1,which.max)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# posterior probabilities
cgmmflex1plot <- data.frame(Peptide = sc1$Peptide,
                            cluster = cgmmflex1cluster,
                            postprob = cgmmflex1pp)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Posterior cluster probability
flexp1 <- ggplot(cgmmflex1plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

flexp1
ggsave("flexp1.png", plot = flexp1, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# compute percentages per cluster
per1 <- paste(round(100 * table(cgmmflex1cluster) / length(cgmmflex1cluster)), "%", sep = "")

# cluster labels
cgmmflex1plot$cluster <- factor(cgmmflex1plot$cluster,
                                labels = paste("Cluster ", 1:length(per1), " (", per1, ")", sep = ""))

# add time, HDX & condition
cgmmflex1plot$Time <- sc1$Time
cgmmflex1plot$HDX <- sc1$HDX_transformed
cgmmflex1plot$Condition <- sc1$condition


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = cgmmflex1plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex1c <- ggplot(data = cgmmflex1plot,
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
flex1c
ggsave("flex1c.png", plot = flex1c, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# peptides in clusters
list1 <- cgmmflex1plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list1


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

cgmm2 <- stepFlexmix(.~.|Peptide, 
                     k = 1:10, 
                     nrep = 5, 
                     model = FLXMRlmm(HDX_transformed ~ Time * condition, 
                                      random = ~ Time, 
                                      varFix = c(Random = FALSE,Residual = TRUE)),
                     data = sc3)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# model-fit results
cgmm2


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# plot
plot(cgmm2)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# best model
cgmmflex2 <- getModel(cgmm2, "BIC")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# parameters
parameters(cgmmflex2)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# posterior probabilities and cluster probabilities
cgmmflex2pp <- apply(posterior(cgmmflex2),1,max)
cgmmflex2cluster <-  apply(posterior(cgmmflex2),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
cgmmflex2plot <- data.frame(Peptide = sc3$Peptide,
                            cluster = cgmmflex2cluster,
                            postprob = cgmmflex2pp)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Posterior cluster probability
flexp2 <- ggplot(cgmmflex2plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

flexp2
ggsave("flexp2.png", plot = flexp2)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Compute percentages
per2 <- paste(round(100 * table(cgmmflex2cluster) / length(cgmmflex2cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
cgmmflex2plot$cluster <- factor(cgmmflex2plot$cluster,
                                labels = paste("Cluster ", 1:length(per2), " (", per2, ")", sep = ""))

# add columns time, HDX & condition
cgmmflex2plot$Time <- sc3$Time
cgmmflex2plot$HDX <- sc3$HDX_transformed
cgmmflex2plot$Condition <- sc3$condition


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = cgmmflex2plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex2c <- ggplot(data = cgmmflex2plot,
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
flex2c
ggsave("flex2c.png", plot = flex2c, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# peptide in cluster
list2 <- cgmmflex2plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list2


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

cgmm3 <- stepFlexmix(.~.|Peptide, 
                     k = 1:10, 
                     nrep = 5, 
                     model = FLXMRlmm(HDX_transformed ~ Time * condition, 
                                      random = ~ Time, 
                                      varFix = c(Random = FALSE,Residual = TRUE)),
                     data = sc4)
end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# model-fit results
cgmm3


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# plot
plot(cgmm3)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# best model
cgmmflex3 <- getModel(cgmm3, "BIC")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# parameters
parameters(cgmmflex3)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# posterior probabilities and cluster probabilities
cgmmflex3pp <- apply(posterior(cgmmflex3),1,max)
cgmmflex3cluster <-  apply(posterior(cgmmflex3),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
cgmmflex3plot <- data.frame(Peptide = sc4$Peptide,
                            cluster = cgmmflex3cluster,
                            postprob = cgmmflex3pp)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Compute percentages
per3 <- paste(round(100 * table(cgmmflex3cluster) / length(cgmmflex3cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
cgmmflex3plot$cluster <- factor(cgmmflex3plot$cluster,
                                labels = paste("Cluster ", 1:length(per3), " (", per3, ")", sep = ""))

# add columns time, HDX & condition
cgmmflex3plot$Time <- sc4$Time
cgmmflex3plot$HDX <- sc4$HDX_transformed
cgmmflex3plot$Condition <- sc4$condition


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Posterior cluster probability
flexp3 <- ggplot(cgmmflex3plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

flexp3
ggsave("flexp3.png", plot = flexp3)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = cgmmflex3plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex3c <- ggplot(data = cgmmflex3plot,
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
flex3c
ggsave("flex3c.png", plot = flex3c, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# peptide in cluster
list3 <- cgmmflex3plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list3


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

cgmm4 <- stepFlexmix(.~.|Peptide, 
                     k = 1:10, 
                     nrep = 5, 
                     model = FLXMRlmm(HDX_transformed ~ Time * condition, 
                                      random = ~ Time, 
                                      varFix = c(Random = FALSE,Residual = TRUE)),
                     data = sc6)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# model-fit results
cgmm4


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# plot
plot(cgmm4)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# best model
cgmmflex4 <- getModel(cgmm4, "BIC")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# parameters
parameters(cgmmflex4)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# posterior probabilities cluster probabilities
cgmmflex4pp <- apply(posterior(cgmmflex4),1,max)
cgmmflex4cluster <-  apply(posterior(cgmmflex4),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
cgmmflex4plot <- data.frame(Peptide = sc6$Peptide,
                            cluster = cgmmflex4cluster,
                            postprob = cgmmflex4pp)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Compute percentages
per4 <- paste(round(100 * table(cgmmflex4cluster) / length(cgmmflex4cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
cgmmflex4plot$cluster <- factor(cgmmflex4plot$cluster,
                                labels = paste("Cluster ", 1:length(per4), " (", per4, ")", sep = ""))

# add columns time, HDX & condition
cgmmflex4plot$Time <- sc6$Time
cgmmflex4plot$HDX <- sc6$HDX_transformed
cgmmflex4plot$Condition <- sc6$condition


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Posterior cluster probability
flexp4 <- ggplot(cgmmflex4plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

flexp4
ggsave("flexp4.png", plot = flexp4)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = cgmmflex4plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex4c <- ggplot(data = cgmmflex4plot,
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
flex4c
ggsave("flex4c.png", plot = flex4c, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# peptide in cluster
list4 <- cgmmflex4plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list4


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

cgmm5 <- stepFlexmix(.~.|Peptide, 
                     k = 1:10, 
                     nrep = 5, 
                     model = FLXMRlmm(HDX_transformed ~ Time * condition, 
                                      random = ~ Time, 
                                      varFix = c(Random = FALSE,Residual = TRUE)),
                     data = sc7)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# model-fit results
cgmm5


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# plot
plot(cgmm5)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# best model
cgmmflex5 <- getModel(cgmm5, "BIC")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# parameters
parameters(cgmmflex5)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# posterior probabilities and cluster probabilities
cgmmflex5pp <- apply(posterior(cgmmflex5),1,max)
cgmmflex5cluster <-  apply(posterior(cgmmflex5),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
cgmmflex5plot <- data.frame(Peptide = sc7$Peptide,
                            cluster = cgmmflex5cluster,
                            postprob = cgmmflex5pp)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Compute percentages
per5 <- paste(round(100 * table(cgmmflex5cluster) / length(cgmmflex5cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
cgmmflex5plot$cluster <- factor(cgmmflex5plot$cluster,
                                labels = paste("Cluster ", 1:length(per5), " (", per5, ")", sep = ""))

# add columns time, HDX & condition
cgmmflex5plot$Time <- sc7$Time
cgmmflex5plot$HDX <- sc7$HDX_transformed
cgmmflex5plot$Condition <- sc7$condition


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Posterior cluster probability
flexp5 <- ggplot(cgmmflex5plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

flexp5
ggsave("flexp5.png", plot = flexp5)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = cgmmflex5plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex5c <- ggplot(data = cgmmflex5plot,
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
flex5c
ggsave("flex5c.png", plot = flex5c, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# peptide in clusters
list5 <- cgmmflex5plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list5


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(123)
start.time <- Sys.time()

cgmm6 <- stepFlexmix(.~.|Peptide, 
                     k = 1:10, 
                     nrep = 5, 
                     model = FLXMRlmm(HDX_transformed ~ Time*condition, 
                                      random = ~ Time, 
                                      varFix = c(Random = FALSE,Residual = TRUE)),
                     data = sc9)

end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# model-fit results
cgmm6


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# plot
plot(cgmm6)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# best model
cgmmflex6 <- getModel(cgmm6, "BIC")


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# parameters
parameters(cgmmflex6)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# compute and plot the posterior cluster probability
cgmmflex6pp <- apply(posterior(cgmmflex6),1,max)
cgmmflex6cluster <-  apply(posterior(cgmmflex6),1,which.max);


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
cgmmflex6plot <- data.frame(Peptide = sc9$Peptide,
                            cluster = cgmmflex6cluster,
                            postprob = cgmmflex6pp)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Compute percentages
per6 <- paste(round(100 * table(cgmmflex6cluster) / length(cgmmflex6cluster)), "%", sep = "")

# Update labels for 'cluster' in gbtmflex1plot
cgmmflex6plot$cluster <- factor(cgmmflex6plot$cluster,
                                labels = paste("Cluster ", 1:length(per6), " (", per6, ")", sep = ""))

# add time, HDX and condition 
cgmmflex6plot$Time <- sc9$Time
cgmmflex6plot$HDX <- sc9$HDX_transformed
cgmmflex6plot$Condition <- sc9$condition


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Posterior cluster probability
flexp6 <- ggplot(cgmmflex6plot, 
       aes(x = factor(cluster), y = postprob)) + 
  geom_boxplot() + 
  ggtitle("flexmix") +
  xlab("Clusters") + 
  ylab("Posterior Cluster Probability") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position =  "none")

flexp6
ggsave("flexp6.png", plot = flexp6)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# all peptides
ggplot(data = cgmmflex6plot, aes(x = Time, y = HDX, group = Peptide, color = cluster)) +
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
flex6c <- ggplot(data = cgmmflex6plot,
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
flex6c
ggsave("flex6c.png", plot = flex6c, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# peptide in clusters
list6 <- cgmmflex6plot %>%
  group_by(cluster) %>%
  summarize(unique_peptides = toString(unique(Peptide)))
list6


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# Cluster probabilities across the settings
df1 <- cgmmflex1plot %>% mutate(source = 'setting 1')
df2 <- cgmmflex2plot %>% mutate(source = 'setting 2')
df3 <- cgmmflex3plot %>% mutate(source = 'setting 3')
df4 <- cgmmflex4plot %>% mutate(source = 'setting 4')
df5 <- cgmmflex5plot %>% mutate(source = 'setting 5')
df6 <- cgmmflex6plot %>% mutate(source = 'setting 6')

# combine
gmmmerge <- bind_rows(df1, df2, df3, df4, df5, df6)
gmmmerge$source <- as.factor(gmmmerge$source)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------
# plot of posterior probabilities
gmmbox <- ggplot(gmmmerge, 
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

gmmbox
ggsave("gmmbox.png", plot = gmmbox, width = 10, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------



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
pp1 <- posterior(cgmmflex1)
pp2 <- posterior(cgmmflex2)
pp3 <- posterior(cgmmflex3)
pp4 <- posterior(cgmmflex4)
pp5 <- posterior(cgmmflex5)
pp6 <- posterior(cgmmflex6)


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


