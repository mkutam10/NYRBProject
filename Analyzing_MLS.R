### Foundations.
library(tidyverse)
library(ggplot2)
library(ggpmisc)
library(viridisLite)
library(stringi)

### TESTING. 
setwd('~/Documents/PYTHON 2021/SCRAPING')
MLS_2020 = read.csv('MLS_2020.csv')
MLS_2019 = read.csv('MLS_2019.csv')
MLS_2018 = read.csv('MLS_2018.csv')

class(MLS_2020$GF) # AS CHARACTER. 
class(MLS_2019$GF) # AS INTEGER (due to lack of penalty shootouts that year). 
class(MLS_2018$GF) # AS CHARACTER. 

### FINDING NAs. 
  # lapply() returns a list; unlist() returns a vector (with 'use.names = FALSE', the naming parts); unique() gets all unique values.
unique(unlist(lapply(MLS_2020, function (column) which (is.na (column))), use.names = FALSE))  # VECTORS can have NAMING parts. lapply() goes by COLUMN, equivalent to apply({data}, 2, {function})
unique(unlist(lapply(MLS_2019, function (column) which (is.na (column))), use.names = FALSE))  # unlist() also helps to remove the 'integer(0)' part of lapply/sapply. 
unique(unlist(lapply(MLS_2018, function (column) which (is.na (column))), use.names = FALSE))  # 'function(data) which (is.na (data))' DEFINES function for finding indexes of NA rows. 

# KEY, KEY NOTE: EITHER MATCH CANCELLED (MLS_2020) or "Advanced data not yet available. It is usually available within 24-48 hours after match completion." from 
# https://fbref.com/en/matches/62750029/New-York-City-FC-Chicago-Fire-April-24-2019-Major-League-Soccer (MLS_2019); https://fbref.com/en/matches/13529c5e/FC-Dallas-Sporting-KC-October-21-2018-Major-League-Soccer (MLS_2018); and https://fbref.com/en/matches/93322c84/Los-Angeles-FC-New-York-City-FC-May-13-2018-Major-League-Soccer (MLS_2018). 


### DELETING FAULTY ROWS. 
MLS_2020 = MLS_2020[-c(281, 598),]  # NOW 648. 
MLS_2019 = MLS_2019[-c(45, 496),]  # NOW 840.  
MLS_2018 = MLS_2018[-c(212, 292, 468, 743),]  # NOW 812. 

### CREATING BINARY PARTS. 
MLS_2020$WinOrNot = ifelse(MLS_2020$Result == 'W', 1, 0)
MLS_2019$WinOrNot = ifelse(MLS_2019$Result == 'W', 1, 0)
MLS_2018$WinOrNot = ifelse(MLS_2018$Result == 'W', 1, 0)

### SORTING OUT PENALTY ISSUES. 
MLS_2020_GOALSANDPENALTIES = MLS_2020[,'GF']
MLS_2019_GOALSANDPENALTIES = MLS_2019[,'GF']
MLS_2018_GOALSANDPENALTIES = MLS_2018[,'GF']

MLS_2020_GOALSAGAINSTANDPENALTIES = MLS_2020[,'GA']
MLS_2019_GOALSAGAINSTANDPENALTIES = MLS_2019[,'GA']
MLS_2018_GOALSAGAINSTANDPENALTIES = MLS_2018[,'GA']
  
### ITERATING THROUGH LOOPS TO GET PENALTIES. 
# 2020.

MLS_2020_GOALSFOR = c()
MLS_2020_PENALTIESFOR = c()

for (GAME in MLS_2020_GOALSANDPENALTIES) {
  if (stri_length(GAME) == 1) {
    MLS_2020_GOALSFOR = append(MLS_2020_GOALSFOR, GAME)
    MLS_2020_PENALTIESFOR = append(MLS_2020_PENALTIESFOR, NA)
  }
  else {  # '\' is ALWAYS OUR ESCAPE SEQUENCE. 
    DIGITS = str_extract_all(GAME, '\\d') # NEED the '\\d' since \t, \n, and etc. in string notation.
    # print(GAME)  # 14 PEN SHOOTOUTS?
    MLS_2020_GOALSFOR = append(MLS_2020_GOALSFOR, DIGITS[[1]][1])
    MLS_2020_PENALTIESFOR = append(MLS_2020_PENALTIESFOR, DIGITS[[1]][2])   
  }
}

MLS_2020$GF = as.integer(MLS_2020_GOALSFOR)
MLS_2020$PensFor = as.integer(MLS_2020_PENALTIESFOR)

# - #. 

MLS_2020_GOALSAGAINST = c()
MLS_2020_PENALTIESAGAINST = c()

for (GAME in MLS_2020_GOALSAGAINSTANDPENALTIES) {
  if (stri_length(GAME) == 1) {
    MLS_2020_GOALSAGAINST = append(MLS_2020_GOALSAGAINST, GAME)
    MLS_2020_PENALTIESAGAINST = append(MLS_2020_PENALTIESAGAINST, NA)
  }
  else {
    # '\' is ALWAYS OUR ESCAPE SEQUENCE. 
    DIGITS = str_extract_all(GAME, '\\d') # NEED the '\\d' since \t, \n, and etc. in string notation.
    # print(GAME)  # 14 PEN SHOOTOUTS?
    MLS_2020_GOALSAGAINST = append(MLS_2020_GOALSAGAINST, DIGITS[[1]][1])
    MLS_2020_PENALTIESAGAINST = append(MLS_2020_PENALTIESAGAINST, DIGITS[[1]][2])   
  }
}

MLS_2020$GA = as.integer(MLS_2020_GOALSAGAINST)
MLS_2020$PensAgainst = as.integer(MLS_2020_PENALTIESAGAINST)

MLS_2020 = MLS_2020[, c(1:9, 88, 10, 89, 11:87)]   # DO ONLY ONCE!. 

# MLS_2020 = MLS_2020[, -10] # USE for REMOVING POORLY LABELLED PENALTIES COLUMN. 

# ----- #. 
# 2019. 

MLS_2019_GOALSFOR = c()
MLS_2019_PENALTIESFOR = c()


for (GAME in MLS_2019_GOALSANDPENALTIES) {
  if (stri_length(GAME) == 1) {
    MLS_2019_GOALSFOR = append(MLS_2019_GOALSFOR, GAME)
    MLS_2019_PENALTIESFOR = append(MLS_2019_PENALTIESFOR, NA)
  }
  else {  # '\' is ALWAYS OUR ESCAPE SEQUENCE. 
    DIGITS = str_extract_all(GAME, '\\d') # NEED the '\\d' since \t, \n, and etc. can be STRING NOTATION CHARACTERS. 
    # print(GAME) # 0 PEN SHOOTOUTS?
    MLS_2019_GOALSFOR = append(MLS_2019_GOALSFOR, DIGITS[[1]][1])  # as.character() is key for concatenation. 
    MLS_2019_PENALTIESFOR = append(MLS_2019_PENALTIESFOR, DIGITS[[1]][2])  # as.character() is key for concatenation. 
  }
}

MLS_2019$GF = as.integer(MLS_2019_GOALSFOR)
MLS_2019$PensFor = as.integer(MLS_2019_PENALTIESFOR)

# - #. 

MLS_2019_GOALSAGAINST = c()
MLS_2019_PENALTIESAGAINST = c()

for (GAME in MLS_2019_GOALSAGAINSTANDPENALTIES) {
  if (stri_length(GAME) == 1) {
    MLS_2019_GOALSAGAINST = append(MLS_2019_GOALSAGAINST, GAME)
    MLS_2019_PENALTIESAGAINST = append(MLS_2019_PENALTIESAGAINST, NA)
  }
  else {
    # '\' is ALWAYS OUR ESCAPE SEQUENCE. 
    DIGITS = str_extract_all(GAME, '\\d') # NEED the '\\d' since \t, \n, and etc. in string notation.
    # print(GAME)  # 14 PEN SHOOTOUTS?
    MLS_2019_GOALSAGAINST = append(MLS_2019_GOALSAGAINST, DIGITS[[1]][1])
    MLS_2019_PENALTIESAGAINST = append(MLS_2019_PENALTIESAGAINST, DIGITS[[1]][2])   
  }
}

MLS_2019$GA = as.integer(MLS_2019_GOALSAGAINST)  # as.character() is key for concatenation. 
MLS_2019$PensAgainst = as.integer(MLS_2019_PENALTIESAGAINST)  # as.character() is key for concatenation.

MLS_2019 = MLS_2019[, c(1:9, 88, 10, 89, 11:87)]   # DO ONLY ONCE!. 

# MLS_2019 = MLS_2019[, -10] # USE for REMOVING POORLY LABELLED PENALTIES COLUMN. 

# ----- #. 
# 2018.

MLS_2018_GOALSFOR = c()
MLS_2018_PENALTIESFOR = c()

for (GAME in MLS_2018_GOALSANDPENALTIES) {
  if (stri_length(GAME) == 1) {
    MLS_2018_GOALSFOR = append(MLS_2018_GOALSFOR, GAME)
    MLS_2018_PENALTIESFOR = append(MLS_2018_PENALTIESFOR, NA)
  }
  else {  # '\' is ALWAYS OUR ESCAPE SEQUENCE. 
    DIGITS = str_extract_all(GAME, '\\d') # NEED the '\\d' since \t, \n, and etc. in string notation.
    # print(GAME)  # 14 PEN SHOOTOUTS?
    MLS_2018_GOALSFOR = append(MLS_2018_GOALSFOR, DIGITS[[1]][1])
    MLS_2018_PENALTIESFOR = append(MLS_2018_PENALTIESFOR, DIGITS[[1]][2])   
  }
}

MLS_2018$GF = as.integer(MLS_2018_GOALSFOR)
MLS_2018$PensFor = as.integer(MLS_2018_PENALTIESFOR)

# - #. 

MLS_2018_GOALSAGAINST = c()
MLS_2018_PENALTIESAGAINST = c()

for (GAME in MLS_2018_GOALSAGAINSTANDPENALTIES) {
  if (stri_length(GAME) == 1) {
    MLS_2018_GOALSAGAINST = append(MLS_2018_GOALSAGAINST, GAME)
    MLS_2018_PENALTIESAGAINST = append(MLS_2018_PENALTIESAGAINST, NA)
  }
  else {
    # '\' is ALWAYS OUR ESCAPE SEQUENCE. 
    DIGITS = str_extract_all(GAME, '\\d') # NEED the '\\d' since \t, \n, and etc. in string notation.
    # print(GAME)  # 14 PEN SHOOTOUTS?
    MLS_2018_GOALSAGAINST = append(MLS_2018_GOALSAGAINST, DIGITS[[1]][1])
    MLS_2018_PENALTIESAGAINST = append(MLS_2018_PENALTIESAGAINST, DIGITS[[1]][2])   
  }
}

MLS_2018$GA = as.integer(MLS_2018_GOALSAGAINST)
MLS_2018$PensAgainst = as.integer(MLS_2018_PENALTIESAGAINST)

MLS_2018 = MLS_2018[, c(1:9, 88, 10, 89, 11:87)]  # DO ONLY ONCE!. 

# MLS_2018 = MLS_2018[, -10] # USE for REMOVING POORLY LABELLED PENALTIES COLUMN.  


### A TOTAL MLS DATAFRAME!
MLS_TOTAL = bind_rows(MLS_2018, MLS_2019, MLS_2020)
MLS_REDBULLS = MLS_TOTAL[MLS_TOTAL$Team == 'New York Red Bulls',]

# ------------------------------------------------------- #. 
##### HISTOGRAMS WINNING & GOALS FOR. 

### Winning. 
# HISTOGRAM OF GOALS FOR SKEWED LEFT as EXPECTED. HIGHEST FREQUENCY IS _ GF. 

# NOTE: The histogram EXTENDS BEYOND LIMITS by 1/2 a BREAK AMOUNT. 
ggplot(MLS_TOTAL, aes(x = WinOrNot)) + geom_histogram(binwidth = 1, color = 'black', fill = 'lightblue') + 
  scale_x_continuous(name = "WinOrNot", limits = c(-0.5, 1.5), breaks = seq(0, 1)) + # EXPAND BEYOND FOR CLEAN LOOK. 
  scale_y_continuous(name = "COUNT", limits = c(0, 1500), breaks = seq(0, 1500, 100))

### Goals For. 
# Histogram of GF SKEWED LEFT as EXPECTED. HIGHEST FREQUENCY IS 1 GF. 
# hist(MLS_TOTAL$GF, main = 'Matthew Kutam, Goals For Distribuion', xlab = 'Goals For', ylab = "Count", xlim = c(0, 7), ylim = c(0, 1500), breaks = c(0:7))

# NOTE: The histogram EXTENDS BEYOND LIMITS by 1/2 a BREAK AMOUNT. 
ggplot(MLS_TOTAL, aes(x = GF)) + geom_histogram(binwidth = 1, color = 'black', fill = 'lightblue') + 
  scale_x_continuous(name = "Home Runs", limits = c(-0.5, 7.5), breaks = seq(0, 7)) + # EXPAND BEYOND FOR CLEAN LOOK. 
  scale_y_continuous(name = "COUNT", limits = c(0, 800), breaks = seq(0, 800, 100))

# MLS_TOTAL_GFTALLY = MLS_TOTAL[MLS_TOTAL$GF == 7,] 

# ------------------------------------------------------- #. 
##### LINEAR REGRESSIONS. 

### Poss on GF. 
# MLS_2020. 
POSSESSIONandGOALSFORlinreg_2020 = lm(MLS_2020$GF ~ MLS_2020$Poss)
summary(POSSESSIONandGOALSFORlinreg_2020)  # y = -0.002304x + 1.553484, 0.0002382 R-Squared, p-value of 0.695...

# MLS_2019. 
POSSESSIONandGOALSFORlinreg_2019 = lm(MLS_2019$GF ~ MLS_2019$Poss)
summary(POSSESSIONandGOALSFORlinreg_2019)  # y = 0.006678x + 1.205374, 0.002371 R-Squared, p-value of 0.1586...

# MLS_2018. 
POSSESSIONandGOALSFORlinreg_2018 = lm(MLS_2018$GF ~ MLS_2018$Poss)
summary(POSSESSIONandGOALSFORlinreg_2018)  # y = 0.008212x + 1.180537, 0.003409 R-Squared, p-value of 0.0964...

# MLS_TOTAL. 
POSSESSIONandGOALSFORlinreg_TOTAL = lm(MLS_TOTAL$GF ~ MLS_TOTAL$Poss)
summary(POSSESSIONandGOALSFORlinreg_TOTAL)  # y = 0.004939x + 1.282167, 0.001216 R-Squared, p-value of 0.09459...

# MLS_REDBULLS. 
POSSESSIONandGOALSFORlinreg_REDBULLS = lm(MLS_REDBULLS$GF ~ MLS_REDBULLS$Poss)
summary(POSSESSIONandGOALSFORlinreg_REDBULLS)  # y = 0.004939x + 1.282167, 0.001216 R-Squared, p-value of 0.09459...

# NEAT GRAPHS. Little correlation THOUGH, almost a FLAT LINE!
PossOnGF_TOTAL = ggplot(data = MLS_REDBULLS, aes(x = Poss, y = GF, color = 'orange3')) + 
   geom_point(size = 1.2, color = 'orange3', show.legend = FALSE) + 
   geom_smooth(method = lm, se = FALSE, size = 1.75, linetype = 'dashed', color= 'navyblue')
PossOnGF_TOTAL

# ----- #. 

### Completed Passes on GF. 
# MLS_2020. 
CMPandGOALSFORlinreg_2020 = lm(MLS_2020$GF ~ MLS_2020$Cmp)
summary(CMPandGOALSFORlinreg_2020)  # y = -0.0002729x + 1.5460215, 0.0004304 R-Squared, p-value of 0.5981...

# MLS_2019. 
CMPandGOALSFORlinreg_2019 = lm(MLS_2019$GF ~ MLS_2019$Cmp)
summary(CMPandGOALSFORlinreg_2019)  # y = 0.0008117x + 1.2211955, 0.004192 R-Squared, p-value of 0.06069...

# MLS_2018. 
CMPandGOALSFORlinreg_2018 = lm(MLS_2018$GF ~ MLS_2018$Cmp)
summary(CMPandGOALSFORlinreg_2018)  # y = 0.0007986x + 1.2801227, 0.004227 R-Squared, p-value of 0.06406...

# MLS_TOTAL. 
CMPandGOALSFORlinreg_TOTAL = lm(MLS_TOTAL$GF ~ MLS_TOTAL$Cmp)
summary(CMPandGOALSFORlinreg_TOTAL)  # y = 0.0005142x + 1.3276374, 0.00166 R-Squared, p-value of 0.05075...

# NEAT GRAPHS. Little correlation THOUGH, almost a FLAT LINE!
CMP.OnGF_TOTAL = ggplot(data = MLS_REDBULLS, aes(x = Cmp., y = WinOrNot, color = 'orange3')) + 
  geom_point(size = 1.2, color = 'orange3', show.legend = FALSE) + 
  geom_smooth(method = lm, se = FALSE, size = 1.75, linetype = 'dashed', color= 'navyblue')
CMP.OnGF_TOTAL

# ----- #. 

### Completed Passes on GF. 
# MLS_2020. 
PrgDistandGOALSFORlinreg_2020 = lm(MLS_2020$GF ~ MLS_2020$PrgDist)
summary(PrgDistandGOALSFORlinreg_2020)  # y = 1.306e-05x + 1.403, 2.35e-05 R-Squared, p-value of 0.902...

# MLS_2019. 
PrgDistandGOALSFORlinreg_2019 = lm(MLS_2019$GF ~ MLS_2019$PrgDist)
summary(PrgDistandGOALSFORlinreg_2019)  # y = 1.505e-04x + 1.124, 0.003263 R-Squared, p-value of 0.09802...

# MLS_2018. 
PrgDistandGOALSFORlinreg_2018 = lm(MLS_2018$GF ~ MLS_2018$PrgDist)
summary(PrgDistandGOALSFORlinreg_2018)  # y = 6.676e-05x + 1.404, 0.0007343 R-Squared, p-value of 0.4406...

# MLS_TOTAL. 
PrgDistandGOALSFORlinreg_TOTAL = lm(MLS_TOTAL$GF ~ MLS_TOTAL$PrgDist)
summary(PrgDistandGOALSFORlinreg_TOTAL)  # y = 9.475e-05x + 1.268, 0.00135 R-Squared, p-value of 0.07814...

# NEAT GRAPHS. Little correlation THOUGH, almost a FLAT LINE!
PrgDistOnGF_TOTAL = ggplot(data = MLS_TOTAL, aes(x = PrgDist, y = GF, color = 'orange3')) + 
  geom_point(size = 1.2, color = 'orange3', show.legend = FALSE) + 
  geom_smooth(method = lm, se = FALSE, size = 1.75, linetype = 'dashed', color= 'navyblue')
PrgDistOnGF_TOTAL
