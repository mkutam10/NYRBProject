### Foundations.
library(tidyverse)
library(ggplot2)
library(ggpmisc)
library(viridisLite)
library(stringi)
library(plotly)
library(gplots)
library(reshape2)
library(rgl)
library(scatterplot3d)
library(grid)
library(olsrr)

### TESTING. 
setwd('~/Documents/ANALYZING 2021/MLS 2021/MLS CSVs')
MLS_2020 = read.csv('MLS_2020.csv')
MLS_2019 = read.csv('MLS_2019.csv')
MLS_2018 = read.csv('MLS_2018.csv')

class(MLS_2020$GF) # AS CHARACTER. 
class(MLS_2019$GF) # AS INTEGER (due to lack of penalty shootouts that year!). 
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
MLS_2020$WinOrNot = ifelse(MLS_2020$Result == 'W', 1, 0)  # AS NUMBERS, not CHAR as used in ML. 
MLS_2019$WinOrNot = ifelse(MLS_2019$Result == 'W', 1, 0)  # AS NUMBERS, not CHAR as used in ML.
MLS_2018$WinOrNot = ifelse(MLS_2018$Result == 'W', 1, 0)  # AS NUMBERS, not CHAR as used in ML.

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

##### ------ ######. 

##### 1VAR GRAPHING. 
### LM1. RED BULLS SPECIFIC! 
# Crs. 
CrsOnGFLM = lm(GF ~ Crs, data = MLS_REDBULLS)
summary(CrsOnGFLM)

CrsOnGF_GRAPH = ggplot(data = MLS_REDBULLS, aes(x = Crs, y = GF, color = 'darkgoldenrod2')) + 
  geom_point(shape = 21, size = 3, fill = 'darkgoldenrod2', colour = 'black', stroke = 0.4, alpha = 0.9, show.legend = FALSE) + # SHAPE IMPORTANT!
  geom_smooth(method = lm, se = FALSE, size = 1.75, linetype = 'dashed', color= 'navyblue', fullrange = TRUE) + 
  scale_x_continuous(name = '# of Crosses', limits = c(0, 45), breaks = seq(0, 45, 3)) +
  scale_y_continuous(name = 'Goals For', limits = c(0, 4), breaks = seq(0, 4, 1)) + 
  ggtitle('2018-20 RB Lin. Reg. of Crosses on Scoring')

CrsOnGF_GRAPH + 
  theme(plot.title = element_text(face = 'bold')) +
  annotate('text', fontface = 4, size = 4.5, x = 36, y = 2.2, label = 'r^2 = 0.056, p-value = 0.019', color = 'navyblue') + 
  annotate('text', fontface = 4, size = 4.5, x = 33.4, y = 1.85, label = 'y = -0.047x + 2.179', color = 'navyblue')

# Att. Long.
AttLongOnGFLM = lm(GF ~ Att.Long, data = MLS_REDBULLS)
summary(AttLongOnGFLM)

AttLongOnGF_GRAPH = ggplot(data = MLS_REDBULLS, aes(x = Att.Long, y = GF, color = 'darkgoldenrod2')) + 
  geom_point(shape = 21, size = 3, fill = 'darkgoldenrod2', colour = 'black', stroke = 0.4, alpha = 0.9, show.legend = FALSE) + # SHAPE IMPORTANT!
  geom_smooth(method = lm, se = FALSE, size = 1.75, linetype = 'dotdash', color= 'navyblue', fullrange = TRUE) + 
  scale_x_continuous(name = '# of Attempted Long Passes', limits = c(55, 135), breaks = seq(55, 135, 5)) +
  scale_y_continuous(name = 'Goals For', limits = c(0, 4), breaks = seq(0, 4, 1)) + 
  ggtitle('2018-20 RB Lin. Reg. of Attempted Long Passes on Scoring')
# AttLongOnGF_GRAPH 

GF_ACTUALS = MLS_REDBULLS$GF
ATTLONGS = MLS_REDBULLS$Att.Long
for (INDEX in seq(1, 97, 1)) { # seq_len much, much quicker (well, apparently). 
  
  # WHAT WE KNOW. 
  GF_ACTUAL = GF_ACTUALS[INDEX]
  # print(GF_ACTUAL)
  ATTLONG = ATTLONGS[INDEX]
  # print(ATTLONG)
  
  # WHAT WE GUESS. 
  GF_PREDICTED = (-0.02894*ATTLONG) + 4.00987
  # print(GF_PREDICTED)
  
  # SEGMENTING. 
  AttLongOnGF_GRAPH = AttLongOnGF_GRAPH + 
    geom_segment(mapping = aes_string(x = ATTLONG, xend = ATTLONG, y = GF_ACTUAL, yend = GF_PREDICTED), linetype = 3, size = 0.33, color = 'navyblue', arrow = arrow(length = unit(0.33, 'cm')))
}

AttLongOnGF_GRAPH + 
  theme(plot.title = element_text(face = 'bold')) +
  annotate('text', fontface = 4, size = 4, x = 120, y = 2.2, label = 'r^2 = 0.086, p-value = 0.003', color = 'navyblue') + 
  annotate('text', fontface = 4, size = 4, x = 120, y = 1.85, label = 'y = -0.029x + 4.009', color = 'navyblue')

### LOG1. RED BULLS SPECIFIC!
# Crs.
CrsOnWinOrNotLOG = glm(WinOrNot ~ Crs, data = MLS_REDBULLS, family = 'binomial')
CrsOnWinOrNot_NULL = CrsOnWinOrNotLOG$null.deviance/-2
CrsOnWinOrNot_PROPOSED = CrsOnWinOrNotLOG$deviance/-2
summary(CrsOnWinOrNotLOG)
paste("McFadden's Psuedo R-squared:", (CrsOnWinOrNot_NULL- CrsOnWinOrNot_PROPOSED)/CrsOnWinOrNot_NULL, sep = ' ')

CrsOnWinOrNot_GRAPH = ggplot(data = MLS_REDBULLS, aes(x = Crs, y = WinOrNot, color = 'red3')) + 
  geom_point(shape = 21, size = 3, fill = 'red3', color = 'black', stroke = 0.4, alpha = 0.9, show.legend = FALSE) + # SHAPE IMPORTANT!
  geom_smooth(method = glm, se = FALSE, size = 1.75, linetype = 'dashed', color= 'navyblue', method.args = list(family = 'binomial'), fullrange = TRUE) + # method.args NEEDS a LIST of PARAMETERS ATTACHED to the method previously specified. 
  scale_x_continuous(name = '# of Crosses', limits = c(0, 45), breaks = seq(0, 45, 3)) +  # limits CUT OFF HOW FAR LINE GOES. 
  scale_y_continuous(name = 'Probability of Winning', limits = c(0, 1), breaks = seq(0, 1, 0.125)) + 
  ggtitle('2018-20 RB Log. Reg. of Crosses on Winning')
CrsOnWinOrNot_GRAPH

CrsOnWinOrNot_GRAPH + 
  theme(plot.title = element_text(face = 'bold')) +
  annotate('text', fontface = 4, size = 4.5, x = 30, y = 0.575, label = 'r^2 = 0.070, p-value = 0.005', color = 'navyblue') + 
  annotate('text', fontface = 4, size = 4.5, x = 30.5, y = 0.475, label = 'y = 1 / (1 + e^(0.121x - 1.468))', color = 'navyblue')

# Att.Long. 
AttLongOnWinOrNotLOG = glm(WinOrNot ~ Att.Long, data = MLS_REDBULLS, family = 'binomial')
AttLongOnWinOrNot_NULL = AttLongOnWinOrNotLOG$null.deviance/-2
AttLongOnWinOrNot_PROPOSED = AttLongOnWinOrNotLOG$deviance/-2
summary(AttLongOnWinOrNotLOG)
paste("McFadden's Psuedo R-squared:", (AttLongOnWinOrNot_NULL- AttLongOnWinOrNot_PROPOSED)/AttLongOnWinOrNot_NULL, sep = ' ')

AttLongOnWinOrNot_GRAPH = ggplot(data = MLS_REDBULLS, aes(x = Att.Long, y = WinOrNot, color = 'red3')) + 
  geom_point(shape = 21, size = 3, fill = 'red3', color = 'black', stroke = 0.4, alpha = 0.9, show.legend = FALSE) + # SHAPE IMPORTANT!
  geom_smooth(method = glm, se = FALSE, size = 1.75, linetype = 'dotdash', color= 'navyblue', method.args = list(family = 'binomial'), fullrange = TRUE) + # method.args NEEDS a LIST of PARAMETERS ATTACHED to the method previously specified. 
  scale_x_continuous(name = '# of Attempted Long Passes', limits = c(55, 135), breaks = seq(55, 135, 5)) +
  scale_y_continuous(name = 'Probability of Winning', limits = c(0, 1), breaks = seq(0, 1, 0.125)) + 
  ggtitle('2018-20 RB Log. Reg. of Attempted Long Passes on Winning')
# AttLongOnWinOrNot_GRAPH 

WIN_ACTUALS = MLS_REDBULLS$WinOrNot
ATTLONGS = MLS_REDBULLS$Att.Long
for (INDEX in seq(1, 97, 1)) { # seq_len much, much quicker (well, apparently). 
  
  # WHAT WE KNOW. 
  WIN_ACTUAL = WIN_ACTUALS[INDEX]
  # print(GF_ACTUAL)
  ATTLONG = ATTLONGS[INDEX]
  # print(ATTLONG)
  
  # WHAT WE GUESS. 
  WIN_PREDICTED = 1/(1 + exp(0.05170*ATTLONG - 4.27082))
  # print(GF_PREDICTED)
  
  # SEGMENTING. 
  AttLongOnWinOrNot_GRAPH = AttLongOnWinOrNot_GRAPH + 
    geom_segment(mapping = aes_string(x = ATTLONG, xend = ATTLONG, y = WIN_ACTUAL, yend = WIN_PREDICTED), linetype = 3, size = 0.33, color = 'navyblue', arrow = arrow(length = unit(0.33, 'cm')))
}

AttLongOnWinOrNot_GRAPH + 
  theme(plot.title = element_text(face = 'bold')) +
  annotate('text', fontface = 4, size = 4, x = 124.5, y = 0.575, label = 'r^2 = 0.057, p-value = 0.010', color = 'navyblue') + 
  annotate('text', fontface = 4, size = 4, x = 124.5, y = 0.475, label = 'y = 1 / (1 + e^(0.052x - 4.271))', color = 'navyblue')

###################################### ---------------------- ######################################. 

##### MULTI GRAPHING. 

### FOUNDATIONAL FLINGS. 
MLS_REDBULLS$WinOrNotFACTOR[which(MLS_REDBULLS$WinOrNot == 0)] = 'No Win...'
MLS_REDBULLS$WinOrNotFACTOR[which(MLS_REDBULLS$WinOrNot == 1)] = 'Win!'
MLS_REDBULLS$WinOrNotFACTOR = as.factor(MLS_REDBULLS$WinOrNotFACTOR)

MLS_REDBULLS$Result = as.factor(MLS_REDBULLS$Result)  # KEY, KEY, KEY!

# Crs and Att.Pen...LM. DROP ON from OBJECT NAME. 
CrsandAttPenGFLM = lm(GF ~ Crs + Att.Pen, data = MLS_REDBULLS)
summary(CrsandAttPenGFLM)
ols_vif_tol(CrsandAttPenGFLM)

# SCATTERPLOTING. USING plot_ly...
CrsandAttPenGF_SCAT = plot_ly(MLS_REDBULLS, x = ~Crs, y = ~Att.Pen, z = ~GF, color = I('darkgoldenrod2'), stroke = I('black'), alpha = 0.9)
CrsandAttPenGF_SCAT = CrsandAttPenGF_SCAT %>% add_markers()
CrsandAttPenGF_SCAT = CrsandAttPenGF_SCAT %>% layout(scene = list(xaxis = list(title = 'Crosses'),
                                   yaxis = list(title = 'Att. Pen. Touches', autorange = 'reversed'),
                                   zaxis = list(title = 'Goals For')), showlegend = FALSE)
# CrsandAttPenLMFORMULA = -0.09121 * MLS_REDBULLS$Crs + 0.05657 * MLS_REDBULLS$Att.Pen + 1.32565
# CrsandAttPenGF_SCAT %>% add_mesh(z = ~CrsandAttPenLMFORMULA)

# MAPPING. USING scatterplot3d...
CrsandAttPenTRIX = data.matrix(MLS_REDBULLS[, c(45, 67, 9)])
CrsandAttPenDF = MLS_REDBULLS[, c(45, 67, 9)]
# CrsandAttPenDF = setNames(CrsandAttPenDF, c('Crs', 'Att.Pen', 'GF'))
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d')  # TO ADD FUNCTION!


CrsandAttPenGF_GRAPH = scatterplot3d(CrsandAttPenTRIX, 
                                     main = '2018-20 RB Lin. Reg. of Crosses, Touches in Att. Pen. Area', 
                                     xlab = 'Crosses', 
                                     ylab = '',
                                     zlab = 'Goals For',
                                     zlim = c(0, 4),
                                     pch = '',
                                     tick.marks = TRUE,
                                     label.tick.marks = TRUE, 
                                     grid = FALSE, 
                                     box = FALSE, 
                                     angle = 40)
addgrids3d(CrsandAttPenTRIX, grid = c('xy', 'xz', 'yz'), lty.grid = 'dashed')
CrsandAttPenGF_GRAPH$points3d(CrsandAttPenTRIX, pch = 20, type = 'p', col = 'darkgoldenrod2')

# REGRESSION PLACEMENT. 
CrsandAttPenORIG = CrsandAttPenGF_GRAPH$xyz.convert(CrsandAttPenDF$Crs, CrsandAttPenDF$Att.Pen, CrsandAttPenDF$GF)
CrsandAttPenPLANE = CrsandAttPenGF_GRAPH$xyz.convert(CrsandAttPenDF$Crs, CrsandAttPenDF$Att.Pen, fitted(CrsandAttPenGFLM))
CrsandAttPenINEGPOS = 1 + (resid(CrsandAttPenGFLM) > 0)  # 2 = POS (ABOVE), 1 = NEG (BELOW). 
segments(CrsandAttPenORIG$x, CrsandAttPenORIG$y, CrsandAttPenPLANE$x, CrsandAttPenPLANE$y, col = 'navyblue', lty = c(2, 1)[CrsandAttPenINEGPOS], lwd = 1.5)  # SWITCHUP? POSITIVE GO WITH A 1 (SOLID LINE), NEGATIVE GO WITH A 2 (DASHED LINE). 
CrsandAttPenGF_GRAPH$plane3d(CrsandAttPenGFLM, draw_lines = TRUE, draw_polygon = TRUE, polygon_args = list(border = 'transparent', col = rgb(1, 0.8, 0.2, 0.5)))

# REDRAWING POSITIVE POINTS. 
CrsandAttPenPOSWHICH = resid(CrsandAttPenGFLM) > 0
segments(CrsandAttPenORIG$x[CrsandAttPenPOSWHICH], CrsandAttPenORIG$y[CrsandAttPenPOSWHICH], CrsandAttPenPLANE$x[CrsandAttPenPOSWHICH], CrsandAttPenPLANE$y[CrsandAttPenPOSWHICH], col = 'navyblue', lty = 1, lwd = 1.5)
CrsandAttPenGF_GRAPH$points3d(CrsandAttPenDF$Crs[CrsandAttPenPOSWHICH], CrsandAttPenDF$Att.Pen[CrsandAttPenPOSWHICH], CrsandAttPenDF$GF[CrsandAttPenPOSWHICH], pch = 20, type = 'p', col = 'darkgoldenrod2')

# ANGLING Y AXIS LABEL. 
CrsandAttPenMinsandMaxs = par('usr')
CrsandAttPenX = CrsandAttPenMinsandMaxs [1] + 0.87 * diff(CrsandAttPenMinsandMaxs[1:2])
CrsandAttPenY = CrsandAttPenMinsandMaxs [3] + 0.129 * diff(CrsandAttPenMinsandMaxs[3:4])
text(CrsandAttPenX, CrsandAttPenY,'Att. Pen. Touches', srt=27)

# FINALLY. 
CrsandAttPenGF_GRAPH

### --- ###. 

# Crs and TB...LOG. DROP ON from OBJECT NAME. 
CrsandTBWinOrNotLOG = glm(WinOrNot ~ Crs + TB..Through.Balls., data = MLS_REDBULLS, family = 'binomial')
CrsandTBWinOrNot_NULL = CrsandTBWinOrNotLOG$null.deviance/-2
CrsandTBWinOrNot_PROPOSED = CrsandTBWinOrNotLOG$deviance/-2
summary(CrsandTBWinOrNotLOG)
paste("McFadden's Psuedo R-squared:", (CrsandTBWinOrNot_NULL- CrsandTBWinOrNot_PROPOSED)/CrsandTBWinOrNot_NULL, sep = ' ')

# SCATTERPLOTING. USING plot_ly...
CrsandTBWinOrNot_SCAT = plot_ly(MLS_REDBULLS, x = ~Crs, y = ~TB..Through.Balls., z = ~WinOrNot, color = ~WinOrNotFACTOR, colors =  c(col2hex('red3'), col2hex('deepskyblue3')))
CrsandTBWinOrNot_SCAT = CrsandTBWinOrNot_SCAT %>% add_markers()
CrsandTBWinOrNot_SCAT = CrsandTBWinOrNot_SCAT %>% layout(scene = list(xaxis = list(title = 'Crosses'),
                                                                    yaxis = list(title = 'Through Balls Made', autorange = 'reversed'),
                                                                    zaxis = list(title = 'Win or Not')))
# CrsandTBX = seq(0, 40, 1)
# CrsandTBY = seq(0, 10, 1)
# CrsandTBWINFORMULA = 1 / (1 + (exp(1)^(0.1274 * MLS_REDBULLS$Crs - 0.3031 * MLS_REDBULLS$TB..Through.Balls. - 1.1779)))
# CrsandTBWinOrNot_SCAT %>% add_mesh(z = ~CrsandTBWINFORMULA, cmin = 0, cmax = 1)

# MAPPING. USING scatterplot3d...
CrsandTBTRIX = data.matrix(MLS_REDBULLS[, c(45, 42, 89)])
CrsandTBTRIX2 = data.matrix(MLS_REDBULLS[, c(42, 45, 89)])
CrsandTBDF = MLS_REDBULLS[, c(45, 42, 89)]

CrsandTBWIN_GRAPH = scatterplot3d(CrsandTBTRIX, 
                                     main = '2018-20 RB Log. Reg. of Crosses, Through Balls', 
                                     xlab = 'Crosses', 
                                     ylab = '',
                                     zlab = 'Winning Probability',
                                     zlim = c(0, 1), 
                                     pch = '',
                                     tick.marks = TRUE,
                                     label.tick.marks = TRUE, 
                                     grid = FALSE, 
                                     box = TRUE, 
                                     angle = 40)
addgrids3d(CrsandTBTRIX, grid = c('xy', 'xz', 'yz'), lty.grid = 'dashed')
CrsandTBWIN_GRAPH$points3d(CrsandTBTRIX, pch = 20, type = 'p', col = 'red3')

# REGRESSION PLACEMENT. 
CrsandTBORIG = CrsandTBWIN_GRAPH$xyz.convert(CrsandTBDF$Crs, CrsandTBDF$TB..Through.Balls., CrsandTBDF$WinOrNot)
CrsandTBPLANE = CrsandTBWIN_GRAPH$xyz.convert(CrsandTBDF$Crs, CrsandTBDF$TB..Through.Balls., fitted(CrsandTBWinOrNotLOG))
CrsandTBINEGPOS = 1 + (resid(CrsandTBWinOrNotLOG) > 0)  # 2 = POS (ABOVE), 1 = NEG (BELOW). 
segments(CrsandTBORIG$x, CrsandTBORIG$y, CrsandTBPLANE$x, CrsandTBPLANE$y, col = 'navyblue', lty = c(2, 1)[CrsandTBINEGPOS], lwd = 1.5)  # SWITCHUP? POSITIVE GO WITH A 1 (SOLID LINE), NEGATIVE GO WITH A 2 (DASHED LINE). 
CrsandTBWIN_GRAPH$plane3d(CrsandTBWinOrNotLOG, draw_lines = TRUE, draw_polygon = TRUE, polygon_args = list(border = 'transparent', col = rgb(0.9, 0, 0, 0.5)))

# REDRAWING POSITIVE POINTS. 
CrsandTBPOSWHICH = resid(CrsandTBWinOrNotLOG) > 0
segments(CrsandTBORIG$x[CrsandTBPOSWHICH], CrsandTBORIG$y[CrsandTBPOSWHICH], CrsandTBPLANE$x[CrsandTBPOSWHICH], CrsandTBPLANE$y[CrsandTBPOSWHICH], col = 'navyblue', lty = 1, lwd = 1.5)
CrsandTBWIN_GRAPH$points3d(CrsandTBDF$Crs[CrsandTBPOSWHICH], CrsandTBDF$TB..Through.Balls.[CrsandTBPOSWHICH], CrsandTBDF$WinOrNot[CrsandTBPOSWHICH], pch = 20, type = 'p', col = 'red3')

# ANGLING Y AXIS LABEL. 
CrsandTBMinsandMaxs = par('usr')
CrsandTBX = CrsandTBMinsandMaxs [1] + 0.97 * diff(CrsandTBMinsandMaxs [1:2])
CrsandTBY = CrsandTBMinsandMaxs [3] + 0.1 * diff(CrsandTBMinsandMaxs [3:4])
text(CrsandTBX, CrsandTBY,'TBs', srt=35)

# FINALLY. 
CrsandTBWIN_GRAPH
par(xpd = TRUE)

###################################### ---------------------- ######################################. 

### CLARIFYING LINEAR PLOT (NOT SO MUCH A PRIORITY SINCE scatterplot3d SUFFICES.)
CrsandAttPenLMX = seq(min(MLS_REDBULLS$Crs), max(MLS_REDBULLS$Crs), length = 100)
CrsandAttPenLMY = seq(min(MLS_REDBULLS$Att.Pen), max(MLS_REDBULLS$Att.Pen), length = 100)
CrsandAttPenLMZ_FUNCTION = function(x, y) {
  -0.09121 * x + 0.05657 * y + 1.32565
}

CrsandAttPenLMZ = outer(CrsandAttPenLMX, CrsandAttPenLMY, CrsandAttPenLMZ_FUNCTION)

persp(CrsandAttPenLMX, CrsandAttPenLMY, CrsandAttPenLMZ, d = 3, theta = 30, phi = 10, ltheta = -135, lphi = 90, shade = NA, col = 'darkgoldenrod2', 
      xlab = 'Crosses', ylab = 'Att. Pen. Touches', zlab = 'Goals For', 
      main = 'Lin. Reg. for Crosses, Touches in Att. Pen. Area',
      ticktype = 'simple',  # ARROWS! 
      cex.lab = 0.84)

### CLARIFYING LOGISTIC PLOT (#1 PRIORITY!).
CrsandTBLOGX = seq(min(MLS_REDBULLS$Crs), max(MLS_REDBULLS$Crs), length = 100)
CrsandTBLOGY = seq(min(MLS_REDBULLS$TB..Through.Balls.), max(MLS_REDBULLS$TB..Through.Balls.), length = 100)
CrsandTBLOGZ_FUNCTION = function(x, y) {
  1 / (1 + (exp(1)^(0.1274 * x - 0.3031 * y - 1.1779)))
}

CrsandTBLOGZ = outer(CrsandTBLOGX, CrsandTBLOGY, CrsandTBLOGZ_FUNCTION)

persp(CrsandTBLOGX, CrsandTBLOGY, CrsandTBLOGZ, d = 3, theta = 45, phi = 20, ltheta = -135, lphi = 90, shade = NA, col = 'red3', 
      xlab = 'Crosses', ylab = 'TBs', zlab = 'Winning Probability', 
      main = 'Log. Reg. for Crosses, Through Balls',
      ticktype = 'simple',  # ARROWS! 
      cex.lab = 0.84) # theta ROTATES COUNTERECLOCKWISE HORIZONTALLY, phi FRONT FACING ROLL VERTICALLY. 

# START WITH 2 2D GRAPHS, SAVED IN DOCUMENTS
# THEN INCLUDE BOTH LM with ONE COLOR and LOG with TWO COLORS. (2 GRAPHS)
# FINALLY INCLUDE WIN, DRAW, LOSE CLASSIFICATION TOO FOR *THREE* COLORS... (2 GRAPHS)
# USELESS WAY TO MAP WITH plotly! TOO MANY POINTS, 886529281 ELEMENTS FOR THE MATRIX. SCALES UP FAR TOO QUICKLY!
