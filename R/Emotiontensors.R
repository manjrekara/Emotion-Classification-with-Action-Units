
##Ajani Blackwood

##This is the script used to take speech trials extracted from the RAVDESS dataset
##And transform them into single observations with time averaged AUs.

rm(list=ls(all=T))
dev.off()

library(data.table)


#1*2*2*1*2*12 = 192 total observations with 50% slit for all classes of emotions
#and genders

#1 repetition
#2 emotions
#2 intensities
#1 statement
#2 genders
#12 actors

###setwd('C:/Users/ajani/Documents/Bayesian ML/model data/facial tracking/very_sad_women')

##Navigate to the extracted folder data labelled 
##"SadWoman", "SadMale", "HappyMale" and "HappyWoman"
##open them up so that the raw data can be processed

# Identify all csv files in the folders folder and store them in lists
#The variables ending with "_files store the names of individual speech trials"

Sadwomen_files= list.files(path = "C:/Users/ajani/Documents/Bayesian ML/model data/facial tracking/SadWoman",     
           pattern = "*.csv", full.names = TRUE)
Sadmen_files= list.files(path = "C:/Users/ajani/Documents/Bayesian ML/model data/facial tracking/SadMale",     
                           pattern = "*.csv", full.names = TRUE)
Happymen_files= list.files(path = "C:/Users/ajani/Documents/Bayesian ML/model data/facial tracking/HappyMale",     
                         pattern = "*.csv", full.names = TRUE)
Happywomen_files= list.files(path = "C:/Users/ajani/Documents/Bayesian ML/model data/facial tracking/HappyWoman",     
                         pattern = "*.csv", full.names = TRUE)


#Holds the labels for all sad women
#F for Woman, M for male
sad_label = c()
Happy_label = c()
Woman_label = c()
Male_label = c()

#For as many items in the folder add an S label denoting sadness
#Also add an F label denoting Woman
#Do this for Happy and men too
for(i in 1:length(Sadwomen_files)){ #same length for all emotions 
  #Woman_label[i] = paste("F",as.character(i))
  sad_label[i] = "S"
  Happy_label[i] = "H"
  Woman_label[i] = "F"
  Male_label[i] = "M"
  
}

#Making empty dataframes to be filled later
sadwomen = data.frame()
happywomen = data.frame()

sadmen = data.frame()
happymen = data.frame()




#Make a subdata frame for sadwomen

for(i in 1:length(Sadwomen_files)){

  sadwoman = read.csv(Sadwomen_files[i])#Read files in order loaded from directory
  sadwoman_AU = sadwoman[,-c(1:677, 695:712)] #Keep only the columns with AU values
  sadwoman_obs = data.frame(colMeans(sadwoman_AU))#Take the average of all Au variables
  sadwoman_obs = data.frame(t(sadwoman_obs))#Transpose and put into a data frame
  sadwomen = rbind(sadwomen,sadwoman_obs)#Bind the rows to make individual speech events
  
}

#Add a new column of sadness labels for these sad speech events for the women
sadwomen = cbind(as.factor(sad_label),Woman_label,sadwomen)

#sub data frame for happy women

for(i in 1:length(Happywomen_files)){
  
  Happywoman = read.csv(Happywomen_files[i])#Read files in order loaded from directory
  Happywoman_AU = Happywoman[,-c(1:677, 695:712)] #Keep only the columns with AU values
  Happywoman_obs = data.frame(colMeans(Happywoman_AU))#Take the average of all Au variables
  Happywoman_obs = data.frame(t(Happywoman_obs))#Transpose and put into a data frame
  happywomen = rbind(happywomen,Happywoman_obs)#Bind the rows to make individual speech events
  
}

#Add a new column of labels for happiness speech events for the women
happywomen = cbind(as.factor(Happy_label),Woman_label,happywomen)



###For the males

#sub dataframe for sad males

for(i in 1:length(Sadmen_files)){
  
  sadman = read.csv(Sadmen_files[i])#Read files in order loaded from directory
  sadman_AU = sadman[,-c(1:677, 695:712)] #Keep only the columns with AU values
  sadman_obs = data.frame(colMeans(sadman_AU))#Take the average of all Au variables
  sadman_obs = data.frame(t(sadman_obs))#Transpose and put into a data frame
  sadmen = rbind(sadmen,sadman_obs)#Bind the rows to make individual speech events
  
}

#Add a new column of labels for sad speech events for the men
sadmen = cbind(as.factor(sad_label),Male_label,sadmen)

#sub data frame for happy males

for(i in 1:length(Happymen_files)){
  
  happyman = read.csv(Happymen_files[i])#Read files in order loaded from directory
  happyman_AU = happyman[,-c(1:677, 695:712)] #Keep only the columns with AU values
  happyman_obs = data.frame(colMeans(happyman_AU))#Take the average of all Au variables
  happyman_obs = data.frame(t(happyman_obs))#Transpose and put into a data frame
  happymen = rbind(happymen,happyman_obs)#Bind the rows to make individual speech events
  
}

#Add a new column of labels for happy speech events for the men
happymen = cbind(as.factor(Happy_label),Male_label,happymen)


#Final dataset for men

#rename individual columns to match both dataframes for happymen and sadmen
colnames(happymen)[1] = "Emotion"
colnames(happymen)[2] = "Sex"
colnames(sadmen)[1] = "Emotion"
colnames(sadmen)[2] = "Sex"
maleemotion = rbind(happymen,sadmen)
#rename all rows 1 through 48
numrows = seq(1:length(maleemotion[,1]))
rownames(maleemotion) = numrows



#Final dataset for women

#rename individual columns to match both dataframes for happywomen and sad women
colnames(happywomen)[1] = "Emotion"
colnames(happywomen)[2] = "Sex"
colnames(sadwomen)[1] = "Emotion"
colnames(sadwomen)[2] = "Sex"
womenemotion = rbind(happywomen,sadwomen)
#rename all rows 1 through 48
numrows = seq(1:length(womenemotion[,1]))
rownames(womenemotion) = numrows


#A pooled data set showing labels of men and women

#Group all men and women together both happy and sad
pooledemotion = rbind(happywomen,happymen,sadwomen,sadmen)
poolednumrows = seq(1:length(pooledemotion[,1]))
rownames(pooledemotion) = poolednumrows


#These lines just existed to write the new datasets for future use

#writing to files
#setwd('C:/Users/ajani/Documents/Bayesian ML/model data/')
#fwrite(maleemotion, "MaleSadHappy.csv")
#fwrite(womenemotion, "WomenSadHappy.csv")
#fwrite(pooledemotion, "AllsexesSadHappy.csv")

