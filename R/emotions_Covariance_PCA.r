rm(list=ls(all=T))
dev.off()

##Ajani Blackwood

##This was an earlier written script to see correlations between random pairs of 
##AU values to see if there were any covariance relationships.
library("faraway")

library("dplyr")                                                  # Load dplyr package
library("plyr")                                                   # Load plyr package
library("readr") 
library(SciViews)
pacman::p_load(pacman, rio) 


#The next line only looks for the sadmale data, but it can be substituted with the
#other folders for emotion men, sadwomen setc... to see the relationships

data_all <- list.files(path = "C:/Users/ajani/Documents/Bayesian ML/model data/facial tracking/SadMale",     # Identify all csv files in folder
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                                            # Store all files in list
  bind_rows                                                       # Combine data sets into one data set 

#keep only the columns with facial tracking units

data_emotion <- data_all[,-c(3:677, 695:712)]      
#data_emotion = t(data_emotion)

#129 frames per actor
labels_actor = rep(seq(1,12), each = 112)
data_emotionlabel <- data.frame(data_emotion[1:length(labels_actor),], labels_actor)
#data_emotionlabel <- t(data_emotionlabel)
data_emotionlabel <- data_emotionlabel[3:19]
data_emotionlabel[data_emotionlabel <= 0.5] = 0 
data_emotionlabel <- data_emotionlabel[complete.cases(data_emotionlabel), ]
summary(data_emotionlabel)

cols_select = sample(seq(3,length(data_emotionlabel)), 5)
pairs(data_emotionlabel[,cols_select], diag.panel = panel.hist)
pairs(data_emotionlabel[,c(7,8)], diag.panel = panel.hist)
pairs(data_emotionlabel[,c(5,9)], diag.panel = panel.hist)
pairs(data_emotionlabel[,c(5,6)], diag.panel = panel.hist)
cor(data_emotionlabel)

## Principal components happiness

emotion_pca<- prcomp(data_emotionlabel, scale = FALSE, center = TRUE)
summary(emotion_pca)
emotion_pca$rotation # this contains the rotation matrix
emotion_pca$x  # this contains the four columns of PCs
emotion_pca
## set up the margins of the canvas
par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(1.5, 0.3, 0), las=0)  
screeplot(emotion_pca, col='red', npcs = 7,
          type = c("bar"),
          main = "Principal Component Variance",
          ylim = (c(0, 5)))

xtick = seq(0.7, 9*1.2, by = 1.2)
xmark = paste("PC", seq(1,9))
axis(side=1, at=xtick, labels=xmark, col='red', cex.axis=1., tck=-0.02)



library(boot)
pca.boot <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample 
  testpca <- prcomp(d, scale. = FALSE, center = TRUE)
  return(sum(testpca$sdev[1:2]^2)/sum(testpca$sdev^2)*100.)
}
#bootstrap
boot.pca <- boot(data=data_emotionlabel, statistic=pca.boot, 
                 R=1000) 
par(mfrow=c(1,1))
tout=quantile(boot.pca$t, probs = c(0.025,0.5,0.975))
print(tout)
hist(boot.pca$t, xlab='Explained variance (%)', main="Histogram") # this looks like a lognormal distribution
lines(c(tout[1], tout[1]),c(0, 1000), col='red')
lines(c(tout[3], tout[3]),c(0, 1000), col='red')
lines(c(tout[2], tout[2]),c(0, 1000), col='blue')

pca.perm <- function(data, n, indices) {
  for (i in 1:n){
    data[,i]<- sample(data[,i], replace = F)
  }
  pca.perm<- prcomp(data, scale. = F)
  return(pca.perm$sdev^2/sum(pca.perm$sdev^2))
}

#perm.pca <- boot(data=data_ann, statistic=pca.perm, n = 523,
#                 R=1000) 

perm.pca <- boot(data=data_emotionlabel, statistic=pca.perm, n = 17,R=1000) 
perm.mat = matrix(data=perm.pca$t, nrow=1344, ncol=17)
perm.bnds = apply(perm.mat, 1, quantile, probs = c(0.95))
par(new=F)
sdev.perm = data.frame(emotion_pca$sdev^2/sum(emotion_pca$sdev^2), perm.bnds)
barplot(t(as.matrix(sdev.perm[1:5,]*100)), beside=T, col=c("aquamarine3","coral"), ylim=c(0, 60))
title(xlab='PC index')
title(ylab='Explained variance (%)')

