### Foundations.
library(tidyverse)
library(ggplot2)
library(ggpmisc)
library(viridisLite)
library(stringi)
library(plyr)

### TESTING. 
setwd('~/Documents/ANALYZING 2021/MLS 2021/MLS CSVs')
MLS_2020 = read.csv('MLS_2020.csv')
MLS_2019 = read.csv('MLS_2019.csv')
MLS_2018 = read.csv('MLS_2018.csv')

class(MLS_2020$GF) # AS CHARACTER. 
class(MLS_2019$GF) # AS INTEGER (due to lack of penalty shootouts that year). 
class(MLS_2018$GF) # AS CHARACTER. 

### FINDING NAs. 
  # lapply() returns a list; unlist() returns a vector (without 'use.names = FALSE', the naming parts); unique() gets all unique values.
unique(unlist(lapply(MLS_2020, function (column) which (is.na (column))), use.names = FALSE))  # VECTORS can have NAMING parts. lapply() goes by COLUMN, equivalent to apply({data}, 2, {function})
unique(unlist(lapply(MLS_2019, function (column) which (is.na (column))), use.names = FALSE))  # unlist() also helps to remove the 'integer(0)' part of lapply/sapply. 
unique(unlist(lapply(MLS_2018, function (column) which (is.na (column))), use.names = FALSE))  # 'function(data) which (is.na (data))' DEFINES function for finding indexes of NA columns. 

# KEY, KEY NOTE: EITHER MATCH CANCELLED (MLS_2020) or "Advanced data not yet available. It is usually available within 24-48 hours after match completion." from 
# https://fbref.com/en/matches/62750029/New-York-City-FC-Chicago-Fire-April-24-2019-Major-League-Soccer (MLS_2019); https://fbref.com/en/matches/13529c5e/FC-Dallas-Sporting-KC-October-21-2018-Major-League-Soccer (MLS_2018); and https://fbref.com/en/matches/93322c84/Los-Angeles-FC-New-York-City-FC-May-13-2018-Major-League-Soccer (MLS_2018). 


### DELETING FAULTY ROWS. 
# MLS_2020 = MLS_2020[-c(281, 598),]  # NOW 648. POST: DON'T LEAVE IN ROWS BLANK AS A MORON, and *THEN* DELETE. DELETE IN NUMBERS APP.
MLS_2019 = MLS_2019[-c(45, 496),]  # NOW 840.  
MLS_2018 = MLS_2018[-c(212, 292, 468, 743),]  # NOW 812. 

### CREATING BINARY PARTS. 
MLS_2020$WinOrNotNum = ifelse(MLS_2020$Result == 'W', 1, 0)
MLS_2019$WinOrNotNum = ifelse(MLS_2019$Result == 'W', 1, 0)
MLS_2018$WinOrNotNum = ifelse(MLS_2018$Result == 'W', 1, 0)

# (FOR PURPOSES OF SVMs and NNs): 

MLS_2020$WinOrNotChar = ifelse(MLS_2020$Result == 'W', 'W', 'NW')
MLS_2019$WinOrNotChar = ifelse(MLS_2019$Result == 'W', 'W', 'NW')
MLS_2018$WinOrNotChar = ifelse(MLS_2018$Result == 'W', 'W', 'NW')

MLS_2020$LoseOrNot = ifelse(MLS_2020$Result == 'L', 1, 0)
MLS_2019$LoseOrNot = ifelse(MLS_2019$Result == 'L', 1, 0)
MLS_2018$LoseOrNot = ifelse(MLS_2018$Result == 'L', 1, 0)

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

MLS_2020 = MLS_2020[, c(1:9, 89, 10, 90, 11:88)]   # DO ONLY ONCE! INCLUSIVE ":". 

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

MLS_2019 = MLS_2019[, c(1:9, 89, 10, 90, 11:88)]   # DO ONLY ONCE! INCLUSIVE ":". 

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

MLS_2018 = MLS_2018[, c(1:9, 89, 10, 90, 11:88)]  # DO ONLY ONCE! INCLUSIVE ":". 

# MLS_2018 = MLS_2018[, -10] # USE for REMOVING POORLY LABELLED PENALTIES COLUMN.  


### MORE MLS DATAFRAMES!
MLS_TOTAL = bind_rows(MLS_2018, MLS_2019, MLS_2020)
MLS_REDBULLS = MLS_TOTAL[MLS_TOTAL$Team == 'New York Red Bulls',]
MLS_NONREDBULLS = MLS_TOTAL[MLS_TOTAL$Team != 'New York Red Bulls',]

# NUMERIC FINAGALING. 
MLS_REDBULLS$Attendance = lapply(MLS_REDBULLS$Attendance, function (cell) if (!is.na(cell)) {sub(',', '', cell)})
MLS_REDBULLS$Attendance = as.numeric(MLS_REDBULLS$Attendance) # AUTOMATICALLY INSERTS NAs! 
MLS_NONREDBULLS$Attendance = lapply(MLS_NONREDBULLS$Attendance, function (cell) if (!is.na(cell)) {sub(',', '', cell)})
MLS_NONREDBULLS$Attendance = as.numeric(MLS_NONREDBULLS$Attendance) # AUTOMATICALLY INSERTS NAs! 

# # to CSVs. 
write.csv(MLS_REDBULLS, 'REDBULLS.csv', row.names = FALSE)  # WAS REDBULLS2 for WinOrNot switch with SVMs and NNs. 
write.csv(MLS_NONREDBULLS, 'NONREDBULLS.csv', row.names = FALSE)

# ------------------------------------------------------- #. 

################## WORKSPACE ####################

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
  scale_x_continuous(name = 'Goals For', limits = c(-0.5, 7.5), breaks = seq(0, 7)) + # EXPAND BEYOND FOR CLEAN LOOK. 
  scale_y_continuous(name = "Frequency", limits = c(0, 800), breaks = seq(0, 800, 100))

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

# MLS_REDBULLS. NEGATIVE?
POSSESSIONandGOALSFORlinreg_REDBULLS = lm(MLS_REDBULLS$GF ~ MLS_REDBULLS$Poss)
summary(POSSESSIONandGOALSFORlinreg_REDBULLS)  # y = -0.0362x + 3.2978, 0.06238 R-Squared, p-value of 0.01362...

# MLS_NONREDBULLS. POSITIVE?
POSSESSIONandGOALSFORlinreg_NONREDBULLS = lm(MLS_NONREDBULLS$GF ~ MLS_NONREDBULLS$Poss)
summary(POSSESSIONandGOALSFORlinreg_NONREDBULLS)  # y = 0.006471x + 1.202738, 0.002083 R-Squared, p-value of 0.0322...

# NEAT GRAPHS. Little correlation THOUGH, almost a FLAT LINE!
PossOnGF_REDBULLS = ggplot(data = MLS_REDBULLS, aes(x = Poss, y = GF, color = 'orange3')) + 
   geom_point(size = 1.6, color = 'orange3', show.legend = FALSE) + 
   geom_smooth(method = lm, se = FALSE, size = 1.75, linetype = 'dashed', color= 'navyblue') + 
   scale_x_continuous(name = 'Possession %', limits = c(30, 75), breaks = seq(30, 75, 5)) +
   scale_y_continuous(name = 'Goals For', limits = c(0, 4), breaks = seq(0, 4, 1)) + 
   ggtitle('Matthew Kutam, 2018-2020 Red Bulls Linear Reg. of Possession on Goals For')

PossOnGF_REDBULLS + 
  theme(plot.title = element_text(face = 'bold')) +
  annotate('text', fontface = 4, x = 66.13, y = 2.8, label = 'r^2 = 0.062, p-value = 0.014', color = 'navyblue') +
  annotate('text', fontface = 4, x = 65.1, y = 2.5, label = 'y = -0.036x + 3.298', color = 'navyblue') + 
  geom_segment(mapping = aes(x = 31, xend = 50, y = 0.1, yend = 2), linetype = 2, size = 1.1, color = 'navyblue')
  # annotate('segment', x = 59.5, xend = 73, y = 2.95, yend = 2.95, color = 'navyblue') + 
  # annotate('segment', x = 59.5, xend = 59.5, y = 2.35, yend = 2.95, color = 'navyblue') + 
  # annotate('segment', x = 73, xend = 73, y = 2.35, yend = 2.95, color = 'navyblue')

PossOnGF_NONREDBULLS = ggplot(data = MLS_NONREDBULLS, aes(x = Poss, y = GF, color = 'orange3')) + 
  geom_point(size = 1.6, color = 'orange3', show.legend = FALSE) + 
  geom_smooth(method = lm, se = FALSE, size = 1.75, linetype = 'dashed', color= 'navyblue') + 
  scale_x_continuous(name = 'Possession %', limits = c(19.5, 80.5), breaks = seq(20, 80, 5)) +
  scale_y_continuous(name = 'Goals For', limits = c(0, 7), breaks = seq(0, 7, 1)) + 
  ggtitle('Matthew Kutam, 2018-2020 Non-Red Bulls Linear Reg. of Possession on Goals For')
PossOnGF_NONREDBULLS

PossOnGF_NONREDBULLS + 
  theme(plot.title = element_text(face = 'bold')) +
  annotate('text', fontface = 4, x = 28.98, y = 4.95, label = 'r = 0.0456, p-value = 0.0322', color = 'navyblue') + 
  geom_segment(mapping = aes(x = 0, xend = 50, y = 0.1, yend = 2, linetype = 'dashed'))
  annotate('segment', x = 19.5, xend = 38.275, y = 4.6, yend = 4.6, color = 'navyblue') + 
  annotate('segment', x = 19.5, xend = 38.275, y = 5.65, yend = 5.65, color = 'navyblue') + 
  annotate('segment', x = 19.5, xend = 19.5, y = 4.6, yend = 5.65, color = 'navyblue') + 
  annotate('segment', x = 38.275, xend = 38.275, y = 4.6, yend = 5.65, color = 'navyblue')


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

# MLS_REDBULLS. NEGATIVE?
CMPandGOALSFORlinreg_REDBULLS = lm(MLS_REDBULLS$GF ~ MLS_REDBULLS$Cmp)
summary(CMPandGOALSFORlinreg_REDBULLS)  # y = -0.003096x + 2.529210, 0.04288 R-Squared, p-value of 0.04184...

# MLS_NONREDBULLS. POSITIVE?
CMPandGOALSFORlinreg_NONREDBULLS = lm(MLS_NONREDBULLS$GF ~ MLS_NONREDBULLS$Cmp)
summary(CMPandGOALSFORlinreg_NONREDBULLS)  # y = 0.0006469x + 1.2711165, 0.002569 R-Squared, p-value of 0.01735...

# NEAT GRAPHS. Little correlation THOUGH, almost a FLAT LINE!
CMPOnGF_REDBULLS = ggplot(data = MLS_REDBULLS, aes(x = Cmp, y = GF, color = 'orange3')) + 
  geom_point(size = 1.2, color = 'orange3', show.legend = FALSE) + 
  geom_smooth(method = lm, se = FALSE, size = 1.75, linetype = 'dashed', color= 'navyblue')
CMPOnGF_REDBULLS

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

# MLS_REDBULLS. NEGATIVE?
PrgDistandGOALSFORlinreg_REDBULLS = lm(MLS_REDBULLS$GF ~ MLS_REDBULLS$PrgDist)
summary(PrgDistandGOALSFORlinreg_REDBULLS)  # y = -0.0002960x + 2.3208188, 0.01267 R-Squared, p-value of 0.2722...

# MLS_NONREDBULLS. POSITIVE?
PrgDistandGOALSFORlinreg_NONREDBULLS = lm(MLS_NONREDBULLS$GF ~ MLS_NONREDBULLS$PrgDist)
summary(PrgDistandGOALSFORlinreg_NONREDBULLS)  # y = 1.120e-04x + 1.218, 0.001867 R-Squared, p-value of 0.04257...

# NEAT GRAPHS. Little correlation THOUGH, almost a FLAT LINE!
PrgDistOnGF_REDBULLS = ggplot(data = MLS_REDBULLS, aes(x = PrgDist, y = GF, color = 'orange3')) + 
  geom_point(size = 1.2, color = 'orange3', show.legend = FALSE) + 
  geom_smooth(method = lm, se = FALSE, size = 1.75, linetype = 'dashed', color= 'navyblue')
PrgDistOnGF_REDBULLS

# ------------------------------------------------------- #. 

### RED BULLS 1-VARIABLE LINEAR REGRESSIONS "FOR" LOOP, written into "RedBulls_LM1.txt". 
LM1_REDBULLS_IVARIABLES = colnames(MLS_REDBULLS)[14:85] # for do.NULL will create names for column names that are NULL. # VECTOR INDEXING in R BEGINS @ 1. 

# RUN ONCE?
for (ivariable in LM1_REDBULLS_IVARIABLES) { 
  
  # OPEN UP a txt.file in the working directory. DOING "sink" documentation!
  sink('RedBulls_GF_LM1.txt', append = TRUE)
  
  # WRITING THE LINES of IVARIABLES. DIVERTING THE OUTPUT CONNECTION of STDOUT to FILE...SO, SO KEY! 
  print(paste(c(ivariable, ':'), collapse = ''))
  
  # ANALYSIS AND SUMMARY! 
  ivarableonGF_linreg = lm(MLS_REDBULLS$GF ~ unlist(MLS_REDBULLS[ivariable]))  # NEED A VECTOR, NOT A 1 COLUMN-DATAFRAME that indexing returns. 
  print(summary(ivarableonGF_linreg))
  cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
  cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
  cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
  
  # CLOSING THE FILE. 
  sink()
}

### NON RED BULLS 1-VARIABLE LINEAR REGRESSIONS "FOR" LOOP, written into "RedBulls_LM1.txt". 
LM1_NONREDBULLS_IVARIABLES = colnames(MLS_NONREDBULLS)[14:85] # for do.NULL will create names for column names that are NULL. # VECTOR INDEXING in R BEGINS @ 1. 

# RUN ONCE?
for (ivariable in LM1_NONREDBULLS_IVARIABLES) { 
  
  # OPEN UP a txt.file in the working directory. DOING "sink" documentation!
  sink('NonRedBulls_GF_LM1.txt', append = TRUE)
  
  # WRITING THE LINES of IVARIABLES. DIVERTING THE OUTPUT CONNECTION of STDOUT to FILE...SO, SO KEY! 
  print(paste(c(ivariable, ':'), collapse = ''))
  
  # ANALYSIS AND SUMMARY! 
  ivariableonGF_linreg = lm(MLS_NONREDBULLS$GF ~ unlist(MLS_NONREDBULLS[ivariable]))  # NEED A VECTOR, NOT A 1 COLUMN-DATAFRAME that indexing returns. 
  print(summary(ivariableonGF_linreg))
  cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
  cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
  cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
  
  # CLOSING THE FILE. 
  sink()
}

# %>% used to FORWARD A RESULT, A VALUE, AN OBJECT into the NEXT EXPRESSION (SEE: https://uc-r.github.io/pipe for reexplanation.)


# ------------------------------------------------------- #. 

##### LOGISTIC REGRESSION. 

### RED BULLS 1-VARIABLE LINEAR REGRESSIONS "FOR" LOOP, written into "RedBulls_LM1.txt". 
LOG1_REDBULLS_IVARIABLES = colnames(MLS_REDBULLS)[14:85] # for do.NULL will create names for column names that are NULL. # VECTOR INDEXING in R BEGINS @ 1. 

# RUN ONCE?
for (ivariable in LOG1_REDBULLS_IVARIABLES) { 
  
  # OPEN UP a txt.file in the working directory. DOING "sink" documentation!
  sink('RedBulls_WIN_LOG1.txt', append = TRUE)
  
  # WRITING THE LINES of IVARIABLES. DIVERTING THE OUTPUT CONNECTION of STDOUT to FILE...SO, SO KEY! 
  print(paste(c(ivariable, ':'), collapse = ''))  # collapse MERGES strings together. 
  
  # ANALYSIS AND SUMMARY! 
  ivariableonWinOrNot_logreg = glm(MLS_REDBULLS$WinOrNot ~ unlist(MLS_REDBULLS[ivariable]), data = MLS_REDBULLS, family = 'binomial')  # NEED A VECTOR, NOT A 1 COLUMN-DATAFRAME that indexing returns. 
  print(summary(ivariableonWinOrNot_logreg))
  
  # McFadden's Pseudo R-squared: 
  ivariableonWinOrNot_null = ivariableonWinOrNot_logreg$null.deviance/-2
  ivariableonWinOrNot_proposed = ivariableonWinOrNot_logreg$deviance/-2
  print(paste(c("McFadden's Psuedo R-squared", ': ', (ivariableonWinOrNot_null - ivariableonWinOrNot_proposed)/ivariableonWinOrNot_null), collapse = ''))
  cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
  cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
  cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
  
  # CLOSING THE FILE. 
  sink()

}

### NON RED BULLS 1-VARIABLE LINEAR REGRESSIONS "FOR" LOOP, written into "RedBulls_LM1.txt". 
LOG1_NONREDBULLS_IVARIABLES = colnames(MLS_NONREDBULLS)[14:85] # for do.NULL will create names for column names that are NULL. # VECTOR INDEXING in R BEGINS @ 1. 

# RUN ONCE?
for (ivariable in LOG1_NONREDBULLS_IVARIABLES) { 
  
  # OPEN UP a txt.file in the working directory. DOING "sink" documentation!
  sink('NonRedBulls_WIN_LOG1.txt', append = TRUE)
  
  # WRITING THE LINES of IVARIABLES. DIVERTING THE OUTPUT CONNECTION of STDOUT to FILE...SO, SO KEY! 
  print(paste(c(ivariable, ':'), collapse = ''))  # collapse MERGES strings together. 
  
  # ANALYSIS AND SUMMARY! 
  ivariableonWinOrNot_logreg = glm(MLS_NONREDBULLS$WinOrNot ~ unlist(MLS_NONREDBULLS[ivariable]), data = MLS_NONREDBULLS, family = 'binomial')  # NEED A VECTOR, NOT A 1 COLUMN-DATAFRAME that indexing returns. 
  print(summary(ivariableonWinOrNot_logreg))
  
  # McFadden's Pseudo R-squared: 
  ivariableonWinOrNot_null = ivariableonWinOrNot_logreg$null.deviance/-2
  ivariableonWinOrNot_proposed = ivariableonWinOrNot_logreg$deviance/-2
  print(paste(c("McFadden's Psuedo R-squared", ': ', (ivariableonWinOrNot_null - ivariableonWinOrNot_proposed)/ivariableonWinOrNot_null), collapse = ''))
  cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
  cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
  cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
  
  # CLOSING THE FILE. 
  sink()
  
}

# QUICK LOGISTIC PLOTS. 
PossOnWinOrNot_REDBULLS = ggplot(data = MLS_REDBULLS, aes(x = Poss, y = WinOrNot, color = 'orange3')) + 
  geom_point(size = 1.2, color = 'orange3', show.legend = FALSE) + 
  geom_smooth(method = glm, se = FALSE, size = 1.75, linetype = 'dashed', color= 'navyblue', method.args = list(family = 'binomial'), fullrange = TRUE) + # method.args gets a LIST of PARAMETERS ATTACHED to the method previously specified. 
  scale_x_continuous(name = 'Possession %', limits = c(30, 80), breaks = seq(30, 80, 5)) +
  ggtitle('Matthew Kutam, 2018-2020 Red Bulls Logistic Reg. of Possession on Goals For')

PossOnWinOrNot_REDBULLS + 
  theme(plot.title = element_text(face = 'bold')) +
  annotate('text', fontface = 4, x = 65.83, y = 0.7, label = 'r^2 = 0.066, p-value = 0.006', color = 'navyblue') + 
  annotate('text', fontface = 4, x = 67.50, y = 0.62, label = 'y = 1 / (1 + e^(0.082x - 3.798))', color = 'navyblue')

# ------------------------------------------------------- #. 

##### LOSING? USEFUL? NAH!

### RED BULLS LM1, MAKING THEM CONCEDE...

# RUN ONCE?
for (ivariable in LM1_REDBULLS_IVARIABLES) { 
  
  # OPEN UP a txt.file in the working directory. DOING "sink" documentation!
  sink('RedBulls_GA_LM1.txt', append = TRUE)
  
  # WRITING THE LINES of IVARIABLES. DIVERTING THE OUTPUT CONNECTION of STDOUT to FILE...SO, SO KEY! 
  print(paste(c(ivariable, ':'), collapse = ''))
  
  # ANALYSIS AND SUMMARY! 
  ivarableonGF_linreg = lm(MLS_REDBULLS$GA ~ unlist(MLS_REDBULLS[ivariable]))  # NEED A VECTOR, NOT A 1 COLUMN-DATAFRAME that indexing returns. 
  print(summary(ivarableonGF_linreg))
  cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
  cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
  cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
  
  # CLOSING THE FILE. 
  sink()
}

### RED BULLS LOG1, MAKING THEM LOSE...

# RUN ONCE?
for (ivariable in LOG1_REDBULLS_IVARIABLES) { 
  
  # OPEN UP a txt.file in the working directory. DOING "sink" documentation!
  sink('RedBulls_LOSE_LOG1.txt', append = TRUE)
  
  # WRITING THE LINES of IVARIABLES. DIVERTING THE OUTPUT CONNECTION of STDOUT to FILE...SO, SO KEY! 
  print(paste(c(ivariable, ':'), collapse = ''))  # collapse MERGES strings together. 
  
  # ANALYSIS AND SUMMARY! 
  ivariableonWinOrNot_logreg = glm(MLS_REDBULLS$LoseOrNot ~ unlist(MLS_REDBULLS[ivariable]), data = MLS_REDBULLS, family = 'binomial')  # NEED A VECTOR, NOT A 1 COLUMN-DATAFRAME that indexing returns. 
  print(summary(ivariableonWinOrNot_logreg))
  
  # McFadden's Pseudo R-squared: 
  ivariableonWinOrNot_null = ivariableonWinOrNot_logreg$null.deviance/-2
  ivariableonWinOrNot_proposed = ivariableonWinOrNot_logreg$deviance/-2
  print(paste("McFadden's Psuedo R-squared:", (ivariableonWinOrNot_null - ivariableonWinOrNot_proposed)/ivariableonWinOrNot_null), sep = ' ')
  cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
  cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
  cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
  
  # CLOSING THE FILE. 
  sink()
  
}

# ------------------------------------------------------- #. 

##### MULTI, MULTI, MULTI! 

# READING AND PROCESSING REDBULLS_LM COMBOS LIST. 
REDBULLS_LM_COMBOS_CHAR = readChar('REDBULLS_LM_COMBOS_COPY.txt', file.info('REDBULLS_LM_COMBOS_COPY.txt')$size) # GIVE WHOLE FILE SIZE...
REDBULLS_LM_COMBOS_LIST = strsplit(REDBULLS_LM_COMBOS_CHAR, ',\\s\\(')
REDBULLS_LM_COMBOS_LIST = lapply(REDBULLS_LM_COMBOS_LIST, function (ELEMENT) ELEMENT = paste('(', ELEMENT, sep = ''))
REDBULLS_LM_COMBOS_LIST = lapply(REDBULLS_LM_COMBOS_LIST, function (ELEMENT) ELEMENT = gsub(pattern = '\\(', replacement = '', ELEMENT)) # UNNECESSARY! but in line with other changes.
REDBULLS_LM_COMBOS_LIST = lapply(REDBULLS_LM_COMBOS_LIST, function (ELEMENT) ELEMENT = gsub(pattern = '\\[', replacement = '', ELEMENT)) 
REDBULLS_LM_COMBOS_LIST = lapply(REDBULLS_LM_COMBOS_LIST, function (ELEMENT) ELEMENT = gsub(pattern = '\\]', replacement = '', ELEMENT)) 
REDBULLS_LM_COMBOS_LIST = lapply(REDBULLS_LM_COMBOS_LIST, function (ELEMENT) ELEMENT = sub(pattern = '\\)', replacement = '', ELEMENT)) # NECESSARY!, on the other hand.
REDBULLS_LM_COMBOS_LIST = lapply(REDBULLS_LM_COMBOS_LIST, function (ELEMENT) ELEMENT = gsub(pattern = "\\'", replacement = '', ELEMENT)) 

REDBULLS_LM_COMBOS = list()
for (INDEX in 1:length(REDBULLS_LM_COMBOS_LIST[[1]])) {  # SPLITTNG PART. 
  REDBULLS_LM_COMBOS[[INDEX]] = strsplit(REDBULLS_LM_COMBOS_LIST[[1]][INDEX], ',')[[1]] # strsplit() ALREADY returns a VECTOR. 
}

# REDBULLS_LM1_COMBOS[[1000]]  # NOTE: [[]] RETURNS element, [] returns list of ELEMENT CONTENTS. QUITE a SMALL, SMALL DISTINCTION!

REDBULLS_LM_COMBOS = lapply(REDBULLS_LM_COMBOS, function (ELEMENT) if (length(ELEMENT) != 1) {paste(ELEMENT, collapse = ' +')} else {ELEMENT = ELEMENT})
REDBULLS_LM_COMBOS = lapply(REDBULLS_LM_COMBOS, function (ELEMENT) ELEMENT = paste('GF ~ ', ELEMENT, sep = '')) 
# REDBULLS_LM1_COMBOS = lapply(REDBULLS_LM1_COMBOS, function (ELEMENT) ELEMENT = as.character(ELEMENT))


# for (COMBO in REDBULLS_LM1_COMBOS) {
  
#   # OPENING FILE UP. 
#   sink('REDBULLS_LM1_COMBOS.txt', append = TRUE)
  
#   # PRINTING COMBO NAME. 
#   print(paste(as.character(COMBO)[3], ':', sep = ''))

#   # ANALYSIS. 
#   REDBlULLS_LM1_MULTI = lm(COMBO, data = MLS_REDBULLS)
#   print(summary(REDBULLS_LM1_MULTI))
#   cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
#   cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).
#   cat('\n')  # AS PER DOCUMENTATION: "CHARACTER STRINGS ARE OUTPUT 'AS IS',", not escaping/'taking literally after the backslash (\).

#   # CLOSING FILE. 
#   sink()
  
}

## OR, MUCH BETTER! ###. 

# OPENING FILE UP. 
sink('REDBULLS_MULTILM.txt', append = TRUE)

COMBOSLM = function (COMBO) {
  print(paste(COMBO, ':', sep = ''))
  print(summary(lm(formula = as.formula(COMBO), data = MLS_REDBULLS)))
  cat('\n')
  cat('\n')
  cat('\n')
}

# LAPPLYING. 
system.time(lapply(REDBULLS_LM_COMBOS, FUN = COMBOSLM))
sink()

# SHORT = REDBULLS_LM1_COMBOS[c(1:100)]
# SHORTISH = REDBULLS_LM1_COMBOS[c(1:200)]
# MEDIUM = REDBULLS_LM1_COMBOS[c(1:1000)]
# MEDIUMISH = REDBULLS_LM1_COMBOS[c(1:2000)]
# LARGE = REDBULLS_LM1_COMBOS[c(1:10000)]

# TIMESHORT = system.time(lapply(SHORT, FUN = COMBOSFUN))
# TIMESHORTISH = system.time(lapply(SHORTISH, FUN = COMBOSFUN))
# TIMEMEDIUM = system.time(lapply(MEDIUM, FUN = COMBOSFUN))
# TIMEMEDIUMISH = system.time(lapply(MEDIUMISH, FUN = COMBOSFUN))
# TIMELARGE = system.time(lapply(LARGE, FUN = COMBOSFUN))

##### ----------- #####. LESS EXPERIMENTING THAN LM (at least too much)!

# READING AND PROCESSING REDBULLS_LOG COMBOS LIST. 
REDBULLS_LOG_COMBOS_CHAR = readChar('REDBULLS_LOG_COMBOS_COPY.txt', file.info('REDBULLS_LOG_COMBOS_COPY.txt')$size) # GIVE WHOLE FILE SIZE...
REDBULLS_LOG_COMBOS_LIST = strsplit(REDBULLS_LOG_COMBOS_CHAR, ',\\s\\(')
REDBULLS_LOG_COMBOS_LIST = lapply(REDBULLS_LOG_COMBOS_LIST, function (ELEMENT) ELEMENT = paste('(', ELEMENT, sep = ''))
REDBULLS_LOG_COMBOS_LIST = lapply(REDBULLS_LOG_COMBOS_LIST, function (ELEMENT) ELEMENT = gsub(pattern = '\\(', replacement = '', ELEMENT)) # UNNECESSARY! but in line with other changes.
REDBULLS_LOG_COMBOS_LIST = lapply(REDBULLS_LOG_COMBOS_LIST, function (ELEMENT) ELEMENT = gsub(pattern = '\\[', replacement = '', ELEMENT)) 
REDBULLS_LOG_COMBOS_LIST = lapply(REDBULLS_LOG_COMBOS_LIST, function (ELEMENT) ELEMENT = gsub(pattern = '\\]', replacement = '', ELEMENT))
REDBULLS_LOG_COMBOS_LIST = lapply(REDBULLS_LOG_COMBOS_LIST, function (ELEMENT) ELEMENT = sub(pattern = '\\)', replacement = '', ELEMENT)) # NECESSARY!, on the other hand.
REDBULLS_LOG_COMBOS_LIST = lapply(REDBULLS_LOG_COMBOS_LIST, function (ELEMENT) ELEMENT = gsub(pattern = "\\'", replacement = '', ELEMENT))

REDBULLS_LOG_COMBOS = list()
for (INDEX in 1:length(REDBULLS_LOG_COMBOS_LIST[[1]])) {
  REDBULLS_LOG_COMBOS[[INDEX]] = strsplit(REDBULLS_LOG_COMBOS_LIST[[1]][INDEX], ',')[[1]] # strsplit() ALREADY returns a VECTOR. WILL ONLY SPLIT if FUNCTION SEES ',', meaning that THUMBS-UP for SINGLE VARIABLE. 
}

REDBULLS_LOG_COMBOS = lapply(REDBULLS_LOG_COMBOS, function (ELEMENT) if (length(ELEMENT) != 1) {paste(ELEMENT, collapse = ' +')} else {ELEMENT = ELEMENT})
REDBULLS_LOG_COMBOS = lapply(REDBULLS_LOG_COMBOS, function (ELEMENT) ELEMENT = paste('WinOrNot ~ ', ELEMENT, sep = '')) 

# OPENING FILE UP. 
sink('REDBULLS_MULTILOG.txt', append = TRUE)

COMBOSLOG = function (COMBO) {
  # REGULAR FLINGS. 
  print(paste(COMBO, ':', sep = ''))
  LOG_REGRESSION_WinOrNot = glm(formula = as.formula(COMBO), data = MLS_REDBULLS, family = 'binomial')
  print(summary(LOG_REGRESSION_WinOrNot))
  
  # R-SQUARED.
  LOG_REGRESSION_WinOrNot_null = LOG_REGRESSION_WinOrNot$null.deviance/-2
  LOG_REGRESSION_WinOrNot_proposed = LOG_REGRESSION_WinOrNot$deviance/-2
  print(paste("McFadden's Psuedo R-squared:", (LOG_REGRESSION_WinOrNot_null - LOG_REGRESSION_WinOrNot_proposed)/LOG_REGRESSION_WinOrNot_null), sep = ' ')
  cat('\n')
  cat('\n')
  cat('\n')
}

# LAPPLYING.
lapply(REDBULLS_LOG_COMBOS, FUN = COMBOSLOG)
sink()

# ------------------------------------------------------- #. 

##### POISSON DISTRIBUTION. 

### START WITH RB. 
RBGF_POISSON_MODEL = rpois(MLS_REDBULLS$GF, lambda = mean(MLS_REDBULLS$GF))
RBGF_POISSON_FREQ = count(MLS_REDBULLS, 'GF')  # NEEDS 'plyr' from 'dplyr'. 
RBGF_POISSON_EXP = list()
for (NUMBER in 0:max(RBGF_POISSON_FREQ$GF)) {
  RBGF_POISSON_EXP = append(RBGF_POISSON_EXP, round(sum(RBGF_POISSON_FREQ$freq) * dpois(NUMBER, lambda = mean(MLS_REDBULLS$GF)), 5))
}
RBGF_POISSON_EXP = unlist(RBGF_POISSON_EXP)
barplot(matrix(c(RBGF_POISSON_FREQ$freq, RBGF_POISSON_EXP), nr = 2, byrow = TRUE), beside = TRUE, col = c("aquamarine3","coral"), names.arg = RBGF_POISSON_FREQ$GF)
legend("topright", c("Observed","Expected Poisson"), pch=15, col=c("aquamarine3","coral"), bty="n")

### MOVE TO ALL TEAMS. 
TOTALGF_POISSON_MODEL = rpois(MLS_TOTAL$GF, lambda = mean(MLS_TOTAL$GF))
TOTALGF_POISSON_FREQ = count(MLS_TOTAL, 'GF')  # NEEDS 'plyr' from 'dplyr'. 
TOTALGF_POISSON_EXP = list()
for (NUMBER in 0:max(TOTALGF_POISSON_FREQ$GF)) {
  TOTALGF_POISSON_EXP = append(TOTALGF_POISSON_EXP, round(sum(TOTALGF_POISSON_FREQ$freq) * dpois(NUMBER, lambda = mean(MLS_TOTAL$GF)), 5))
}
TOTALGF_POISSON_EXP = unlist(TOTALGF_POISSON_EXP)
barplot(matrix(c(TOTALGF_POISSON_FREQ$freq, TOTALGF_POISSON_EXP), nr = 2, byrow = TRUE), beside = TRUE, col = c("aquamarine3","coral"), names.arg = TOTALGF_POISSON_FREQ$GF)
legend("topright", c("Observed","Expected Poisson"), pch=15, col=c("aquamarine3","coral"), bty="n")
