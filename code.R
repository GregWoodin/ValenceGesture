# Greg Woodin
# 13/03/18
# Analysing gesture data

# Preprocessing, loading in data and packages
setwd("~/Documents/University/Masters/Analysis/overall")
library(tidyverse)    # Data processing and visualisation
library(tidyr)    # Data processing and visualisation
library(plyr)   # Renaming columns
library(dplyr)    # Writing for loop 
library(lsr)  # Calculating Cramer's V / phi
library(lme4)   # Fitting logistic regression mixed models
library(effects)    # Plotting logistic regression mixed models
library(boot)   # Bootstrapping data
library(FactoMineR)   # For MCA
library(factoextra)   # For MCA plots
library(naniar)   # Replacing specified values with NA
library(Publish)    # Creating 2x2 contingency tables with inputted values

# Give dataset shorter name:
df <- read_csv("overall_results.csv")  

# Create column for verb versus adjective phrases:
df <- mutate(df, VerbVsAdj = ifelse(Phrase %in% c("high_standards", "low_standards"), "Adj", "Verb"))

# Create column for negative versus positive phrases:
df <- mutate(df, NegVsPos = ifelse(Phrase %in% c("low_standards", "lower_the_standards"), "Neg", "Pos"))

# Create function to calculate raw data, proportions and binomial tests of single variables:
calc <- function(dataframe, column){
  print(xtab <- table(dataframe[, column]))   # Tabulate raw counts
  print((prop.table <- prop.table(xtab) * 100))   # Tabulate proportions
  print(binom.test(xtab))}   # Binomial test (violates independence)

# Create function to calculate raw data, proportions, Chi-square values and Cramer's V / phi across two variables:
calc2 <- function(dataframe, column, column2){
  print(xtab <- table(dataframe[, column], dataframe[, column2]))   # Tabulate raw counts
  print((prop.table <- prop.table(xtab, 1) * 100))   # Tabulate proportions
  print(chisq.test(xtab))   # Chi-square test (violates independence)
  print(cramersV(xtab))}    # Cramer's V / phi: Small = 0.1, Medium = 0.3, Large = 0.8

# ============
# Reduce corpus to analysable videos
# ============

# Original dataset: 1600 videos

# HasPhrase == "yes": 1280 videos
df <- filter(df, HasPhrase != "no")

# SpeakerVisible == "yes": 950 videos
df <- filter(df, SpeakerVisible != "no")

# HandsVisibleGenerous == "one" or "both": 499 videos
df <- filter(df, HandsVisibleGenerous != "neither")

# HandsFreeGenerous == "one" or "both": 425 videos
df <- filter(df, HandsFreeGenerous != "neither")

# Negated == "no": 410 videos
df <- filter(df, is.na(Negated) | Negated != "yes")

# Duplicate == "no": 399 videos
df <- filter(df, Duplicate != "yes")

# Create dataframe showing reduction of corpus size:
reduc <- data.frame(
  c("Overall", "Has Phrase", "Speaker Visible", "Hands Visible", "Hands Free", "Not Negated", "Not Duplicated"),
  c(1600, 1280, 950, 499, 425, 410, 399))

# Change column names:
colnames(reduc) = c("level", "videos")

# Preliminary work to make graph:
reduc$level <- factor(reduc$level, levels = c("Not Duplicated", "Not Negated", "Hands Free", "Hands Visible", "Speaker Visible", "Has Phrase", "Overall"))
bold.24.text <- element_text(size = 24, face = "bold")    # Create font style for axis titles
bold.21.text <- element_text(size = 19, face = "bold")    # Create font style for legend title
black.18.text <- element_text(size = 18, color = "black")    # Create font style for axis labels

# Make graph:
reduc %>%
  ggplot(aes(x = reduc$level, y = reduc$videos, fill = reduc$videos)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  coord_flip() + 
  theme_classic() +
  xlab('Exclusion Level') + 
  ylab('Number of Videos') +
  theme(axis.title = bold.24.text, axis.text.y = element_text(size = 17)) +
  theme(axis.text = black.18.text) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  scale_fill_gradient(low = "snow2", high = "midnightblue")

# ============ 
# Gestural attraction 
# ============ 

# Create dataframe without ContextMove == no videos:
df_mvonly <- filter(df, is.na(ContextMove) | ContextMove != "no") %>%
  as.data.frame(df_mvonly)   # Turn df_mvonly into data frame

# Compare 'yes' vs. 'no' for HandsMoving:
calc(df_mvonly, "HandsMoving")

# Preliminary work to make graph showing gestural attraction across the four phrases:
xtab <- table(df_mvonly$Phrase, df_mvonly$HandsMoving)    # Tabulate raw data
prop.table <- prop.table(xtab, 1)    # Tabulate proportions
xtab <- as.data.frame(prop.table)   # Turn table into dataframe
as.numeric(xtab$Freq)   # Turn Frequency into numeric variable
xtab$Var1 <- factor(xtab$Var1, levels = c("low_standards", "high_standards", "lower_the_standards", "raise_the_standards"))   # Re-order x-axis variables
xtab$Var2 <- factor(xtab$Var2, levels = c("no", "yes"))    # Re-order legend variables

# Make graph:
xtab %>%
  ggplot(aes(x = xtab$Var1, y = Freq, fill = factor(Var2))) + 
  geom_bar(stat = "identity") + 
  theme_classic() +
  xlab('Phrase') + 
  ylab('Proportion') +
  theme(axis.title = bold.24.text, legend.text = element_text(size = 17), legend.title = bold.21.text) +
  theme(axis.text = black.18.text, axis.text.x = element_text(face = "italic")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  scale_fill_manual(values = c('grey75', 'forestgreen'), breaks = c("yes","no"), name = "Contains Gesture?", labels = c("Yes", "No")) + 
  scale_x_discrete(labels = c('low', 'high', 'lower', 'raise'))

# Compare verb vs. adjective:
calc2(df_mvonly, "VerbVsAdj", "HandsMoving")

# Create for loop to simulate 1000 samples with only unique speakers:
set.seed(13)    # Make example reproducible
nsim <- 1000    # Create object containing information about number of simulations
loop_p <- numeric(length = nsim)    # Create numeric object to store p-values from simulations
loop_chi <- numeric(length = nsim)    # Create numeric object to store chi-square values from simulations

for (i in 1:nsim) {
  df_loop <- sample_n(df_mvonly, size = nrow(df_mvonly))
  df_reduc <- filter(df_loop, !duplicated(SpeakerName))
  xtab <- table(df_reduc[, "HandsMoving"], df_reduc[, "VerbVsAdj"])
  loop_p[i] <- chisq.test(xtab)$p.value
  loop_chi[i] <- chisq.test(xtab)$statistic}

sum(loop_p < 0.05)    # Find number of p-values less than 0.05
mean(loop_chi)    # Find mean of Chi-square values
sum(loop_chi < 3.84)    # Find number of Chi-square values lower than critical value for 1DF

# Compare -valence vs. +valence:
calc2(df_mvonly, "NegVsPos", "HandsMoving")

# Compare progressive vs. non-progressive:
df2 <- filter(df_mvonly, !(Aspect %in% 'none')) # Remove 'none' videos
calc2(df2, "Aspect", "HandsMoving")

# Compare present participle vs. non-present participle:
df2 <- mutate(df_mvonly, ing_forms = ifelse(Aspect %in% c("none", "progressive"), "ing", "none"))
calc2(df2, "ing_forms", "HandsMoving")

# Compare speaker-agent vs. other-agent :
calc2(df_mvonly, "Agent", "HandsMoving")

# ============ 
# Gestural effort
# ============ 

# Include only HandsFree == 'both' videos:
df_bothhnd <- filter(df, HandsFree %in% 'both') %>%
  as.data.frame(df_bothhnd)   # Turn df_bothhnd into data frame

# Create column for one-handed gestures vs. both-handed gestures:
df_bothhnd <- mutate(df_bothhnd, OneVsBoth = ifelse(WhichHand %in% c("L", "R"), "one", "both"))

# Compare 'one' vs. 'both' for OneVsBoth:
calc(df_bothhnd, "OneVsBoth")

# Preliminary work to make graph showing gestural effort across the four phrases:
xtab <- table(df_bothhnd$Phrase, df_bothhnd$OneVsBoth)    # Tabulate raw data
prop.table <- prop.table(xtab, 1)    # Tabulate proportions
xtab <- as.data.frame(prop.table)   # Turn table into dataframe
as.numeric(xtab$Freq)   # Turn Frequency into numeric variable
xtab$Var1 <- factor(xtab$Var1, levels = c("low_standards", "high_standards", "lower_the_standards", "raise_the_standards"))   # Re-order x-axis variables
xtab$Var2 <- factor(xtab$Var2, levels = c("neither", "one", "both"))    # Re-order legend variables

# Make graph showing gestural effort across the four phrases:
xtab %>%
  ggplot(aes(x = xtab$Var1, y = Freq, fill = xtab$Var2)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic() +
  xlab('Phrase') +
  ylab('Proportion') +
  theme(axis.title = bold.24.text, legend.text = element_text(size = 17), legend.title = bold.21.text) +
  theme(axis.text = black.18.text, axis.text.x = element_text(face = "italic")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  scale_fill_manual(values = c('firebrick', 'dodgerblue4'), name = "Gesturing Hand/s", labels = c("One", "Both")) +
  scale_x_discrete(labels = c('low','high','lower', 'raise'))

# Compare verb vs. adjective:
calc2(df_bothhnd, "VerbVsAdj", "OneVsBoth")

# Compare -valence vs. +valence:
calc2(df_bothhnd, "NegVsPos", "OneVsBoth")

# Compare progressive vs. non-progressive:
df2 <- filter(df_bothhnd, !(Aspect %in% 'none')) # Remove Aspect == 'none' videos
calc2(df2, "Aspect", "OneVsBoth")

# Compare present participle vs. non-present participle:
df2 <- mutate(df_bothhnd, ing_forms = ifelse(Aspect %in% c("none", "progressive"), "ing", "none"))
calc2(df2, "ing_forms", "OneVsBoth")

# Compare speaker-agent vs. other-agent:
calc2(df_bothhnd, "Agent", "OneVsBoth")

# ============ 
# Gestural fit: vertical movement
# ============

# Create dataframe without HandsMoving == no videos:
df_mvonly <- filter(df, HandsMoving != "no") 

# Create column for matching vs. mismatching values:
df_mvonly$VerticalMatch <- "mismatch"   # Assign 'mismatch' to every row by default

# Assign 'match' to matching cases:
df_mvonly[df_mvonly$Phrase %in% c("low_standards", "lower_the_standards") & 
            df_mvonly$MovementVertical %in% "down", ]$VerticalMatch <- "match"  
df_mvonly[df_mvonly$Phrase %in% c("high_standards", "raise_the_standards") & 
            df_mvonly$MovementVertical %in% "up", ]$VerticalMatch <- "match"

### 1. Compatible vs. incompatible vertical movement:

# Create new dataframe
df_mvonly2 <- df_mvonly %>% drop_na(MovementVertical)   # Remove rows with missing values
df_mvonly2 <- filter(df_mvonly2, MovementVertical %in% c('up', 'down'))   # Only keep 'up' and 'down' values

# Preliminary work to make graph showing gestural fit across the four phrases:
(xtab <- table(df_mvonly2$Phrase, df_mvonly2$VerticalMatch))    # Tabulate raw data
(prop.table <- prop.table(xtab, 1))   # Tabulate proportions
xtab <- as.data.frame(prop.table)   # Turn table into dataframe
as.numeric(xtab$Freq)   # Turn Frequency into numeric variable
xtab$Var1 <- factor(xtab$Var1, levels = c("low_standards", "high_standards", "lower_the_standards", "raise_the_standards"))   # Re-order x-axis variables
xtab$Var2 <- factor(xtab$Var2, levels = c("mismatch", "match"))   # Re-order legend variables

# Make graph:
ggplot(xtab, aes(x = xtab$Var1, y = Freq, fill = xtab$Var2)) + 
  geom_bar(stat = "identity") + theme_classic() +
  xlab('Phrase') +
  ylab('Proportion') +
  theme(axis.title = bold.24.text, legend.text = element_text(size = 17), legend.title = bold.21.text) +
  theme(axis.text = black.18.text, axis.text.x = element_text(face = "italic")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  scale_fill_manual(values = c('grey75', 'forestgreen'), name = "Matching Gesture?", labels = c("No", "Yes")) +
  scale_x_discrete(labels = c('low', 'high', 'lower', 'raise'))
# Want to switch legend labels round without altering graph itself

### 2. Compatible vertical movement vs. no vertical movement

# Create new dataframe:
df_mvonly2 <- filter(df_mvonly, Phrase %in% c("high_standards", "raise_the_standards") & MovementVertical %in% "up" |
                       Phrase %in% c("low_standards", "lower_the_standards") & MovementVertical %in% "down" |
                       is.na(MovementVertical))

# Preliminary work to make graph showing gestural fit across four phrases: 
xtab <- table(df_mvonly2$Phrase, df_mvonly2$VerticalMatch)   # Tabulate raw data
prop.table <- prop.table(xtab, 1)    # Tabulate proportions
xtab <- as.data.frame(prop.table)   # Turn table into dataframe
as.numeric(xtab$Freq)   # Turn Frequency into numeric variable
xtab$Var1 <- factor(xtab$Var1, levels = c("low_standards", "high_standards", "lower_the_standards", "raise_the_standards"))   # Re-order x-axis variables
xtab$Var2 <- factor(xtab$Var2, levels = c("mismatch", "match"))   # Re-order legend variables

# Make graph:
ggplot(xtab, aes(x = xtab$Var1, y = Freq, fill = xtab$Var2)) + 
  geom_bar(stat = "identity") + theme_classic() +
  xlab('Phrase') +
  ylab('Proportion') +
  theme(axis.title = bold.24.text, legend.text = element_text(size = 17), legend.title = bold.21.text) +
  theme(axis.text = black.18.text, axis.text.x = element_text(face = "italic")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  scale_fill_manual(values = c('grey75', 'forestgreen'), name = "Matching Gesture?", labels = c("No", "Yes")) +
  scale_x_discrete(labels = c('low', 'high', 'lower', 'raise'))
# Want to switch legend labels round without altering graph itself

### 3. Tests with combined measure

# Turn df_mvonly into data frame:
df_mvonly <- as.data.frame(df_mvonly)

# Compare 'match' vs. 'mismatch' for VerticalMatch:
calc(df_mvonly, "VerticalMatch")

# Compare verb vs. adjective:
calc2(df_mvonly, "VerbVsAdj", "VerticalMatch")

# Create for loop to simulate 1000 samples with only unique speakers:
set.seed(13)    # Make example reproducible
nsim <- 1000    # Create object containing information about number of simulations
loop_p <- numeric(length = nsim)    # Create numeric object to store p-values from simulations
loop_chi <- numeric(length = nsim)    # Create numeric object to store chi-square values from simulations

for (i in 1:nsim) {
  df_loop <- sample_n(df_mvonly, size = nrow(df_mvonly))
  df_reduc <- filter(df_loop, !duplicated(SpeakerName))
  xtab <- table(df_reduc[, "VerbVsAdj"], df_reduc[, "VerticalMatch"])
  loop_p[i] <- chisq.test(xtab)$p.value
  loop_chi[i] <- chisq.test(xtab)$statistic}

sum(loop_p < 0.05)    # Find number of p-values less than 0.05
mean(loop_chi)    # Find mean of Chi-square values
sum(loop_chi < 3.84)    # Find number of Chi-square values lower than critical value for 1DF

# Compare -valence vs. +valence:
calc2(df_mvonly, "NegVsPos", "VerticalMatch")

# Create for loop to simulate 1000 samples with only unique speakers:
set.seed(13)    # Make example reproducible
nsim <- 1000    # Create object containing information about number of simulations
loop_p <- numeric(length = nsim)    # Create numeric object to store p-values from simulations
loop_chi <- numeric(length = nsim)    # Create numeric object to store chi-square values from simulations

for (i in 1:nsim) {
  df_loop <- sample_n(df_mvonly, size = nrow(df_mvonly))
  df_reduc <- filter(df_loop, !duplicated(SpeakerName))
  xtab <- table(df_reduc[, "NegVsPos"], df_reduc[, "VerticalMatch"])
  loop_p[i] <- chisq.test(xtab)$p.value
  loop_chi[i] <- chisq.test(xtab)$statistic}

sum(loop_p < 0.05)    # Find number of p-values less than 0.05
mean(loop_chi)    # Find mean of Chi-square values
sum(loop_chi < 3.84)    # Find number of Chi-square values lower than critical value for 1DF

# Compare progressive vs. non-progressive:
df2 <- filter(df_mvonly, !(Aspect %in% 'none')) # Remove Aspect == 'none' videos
calc2(df2, "Aspect", "VerticalMatch")

# Compare present participle vs. non-present participle:
df2 <- mutate(df_mvonly, ing_forms = ifelse(Aspect %in% c("none", "progressive"), "ing", "none"))
calc2(df2, "ing_forms", "VerticalMatch")

# Create for loop to simulate 1000 samples with only unique speakers:
set.seed(13)    # Make example reproducible
nsim <- 1000    # Create object containing information about number of simulations
loop_p <- numeric(length = nsim)    # Create numeric object to store p-values from simulations
loop_chi <- numeric(length = nsim)    # Create numeric object to store chi-square values from simulations

for (i in 1:nsim) {
  df_loop <- sample_n(df2, size = nrow(df_mvonly))
  df_reduc <- filter(df_loop, !duplicated(SpeakerName))
  xtab <- table(df_reduc[, "ing_forms"], df_reduc[, "VerticalMatch"])
  loop_p[i] <- chisq.test(xtab)$p.value
  loop_chi[i] <- chisq.test(xtab)$statistic}

sum(loop_p < 0.05)    # Find number of p-values less than 0.05
mean(loop_chi)    # Find mean of Chi-square values
sum(loop_chi < 3.84)    # Find number of Chi-square values lower than critical value for 1DF

# Compare agent vs. non-agent:
calc2(df_mvonly, "Agent", "VerticalMatch")

# ============ 
# Body-specificity hypothesis 
# ============ 

### 1. Gesturing hand

# Only include videos where HandsFreeGenerous == both:
df2 <- filter(df, HandsFreeGenerous == "both") %>%
  as.data.frame(df2)    # Turn df2 into a data frame

# Remove WhichHand values other than 'left' and 'right':
df2 <- filter(df2, !(WhichHand %in% 'both'))  

# Compare 'left' vs. 'right' for WhichHand:
calc(df2, "WhichHand")

# Preliminary work to make graph showing WhichHand across Phrase:
xtab <- table(df2$Phrase, df2$WhichHand)    # Tabulate raw data
prop.table <- prop.table(xtab, 1)    # Tabulate proportions
xtab <- as.data.frame(prop.table)   # Turn table into dataframe
as.numeric(xtab$Freq)   # Turn Frequency into numeric variable
xtab$Var1 <- factor(xtab$Var1, levels = c("low_standards", "high_standards", "lower_the_standards", "raise_the_standards"))   # Re-order x-axis variables

# Make graph:
xtab %>%
  ggplot(aes(x = xtab$Var1, y = Freq, fill = xtab$Var2)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_classic() +
  xlab('Phrase') +
  ylab('Proportion') +
  theme(axis.title = bold.24.text, legend.text = element_text(size = 17), legend.title = bold.21.text) +
  theme(axis.text = black.18.text, axis.text.x = element_text(face = "italic")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  scale_fill_manual(values = c('firebrick', 'dodgerblue4'), name = "Which Hand?", labels = c("Left", "Right")) +
  scale_x_discrete(labels = c('low','high','lower', 'raise'))

# Compare -valence vs. +valence:
calc2(df2, "NegVsPos", "WhichHand")

### 2. Horizontal movement

# Exclude values other than 'left' and 'right':
df2 <- filter(df, !(MovementHorizontal %in% c('outward', 'left / right'))) %>%
  as.data.frame(df2)    # Turn df2 into data frame

# Compare 'left' vs. 'right' for MovementHorizontal:
calc(df2, "MovementHorizontal")

# Preliminary work to make graph showing MovementHorizontal across Phrase:
(xtab <- table(df2$Phrase, df2$MovementHorizontal))   # Tabulate raw data
prop.table <- prop.table(xtab, 1)    # Tabulate proportions
xtab <- as.data.frame(prop.table)   # Turn table into dataframe
as.numeric(xtab$Freq)   # Turn Frequency into numeric variable
xtab$Var1 <- factor(xtab$Var1, levels = c("low_standards", "high_standards", "lower_the_standards", "raise_the_standards"))   # Re-order x-axis variables

# Make graph:
xtab %>%
  ggplot(aes(x = xtab$Var1, y = Freq, fill = xtab$Var2)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_classic() +
  xlab('Phrase') +
  ylab('Proportion') +
  theme(axis.title = bold.24.text, legend.text = element_text(size = 17), legend.title = bold.21.text) +
  theme(axis.text = black.18.text, axis.text.x = element_text(face = "italic")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  scale_fill_manual(values = c('firebrick', 'dodgerblue4'), name = "Which Direction?", labels = c("Left", "Right")) +
  scale_x_discrete(labels = c('low', 'high', 'lower', 'raise'))

# Compare -valence vs. +valence:
calc2(df2, "NegVsPos", "MovementHorizontal")

# ============ 
# Articulatory plurality
# ============

### 1. Hands 

# Only include videos where HandsFreeGenerous == both:
df2 <- filter(df, HandsFreeGenerous == "both") %>%
  as.data.frame(df2)

# Create column for one-handed gestures versus both-handed gestures:
df2 <- mutate(df2, OneVsBoth = ifelse(WhichHand %in% c("L", "R"), "one", "both"))

# Compare OneVsBoth across NounPlural:
(xtab <- table(df2$OneVsBoth, df2$NounPlural))   # Tabulate raw counts
(prop.table <- prop.table(xtab, 2) * 100)   # Tabulate proportions
chisq.test(xtab)   # Chi-square test (violates independence)
cramersV(xtab)    # Cramer's V / phi: Small = 0.1, Medium = 0.3, Large = 0.8

# Preliminary work to make graph showing OneVsBoth across NounPlural:
xtab <- as.data.frame(prop.table)   # Turn table into dataframe
as.numeric(xtab$Freq)   # Turn Frequency into numeric variable
xtab$Var1 <- factor(xtab$Var1, levels = c("one", "both"))    # Re-order legend variables

# Make graph:
ggplot(xtab, aes(x = xtab$Var2, y = Freq, fill = xtab$Var1)) + 
  geom_bar(stat = "identity", position = "dodge") + theme_classic() +
  xlab('Phrase') +
  ylab('Proportion') +
  theme(axis.title = bold.24.text, legend.text = element_text(size = 17), legend.title = bold.21.text) +
  theme(axis.text = black.18.text, axis.text.x = element_text(face = "italic")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  scale_fill_manual(values = c('firebrick', 'dodgerblue4'), name = "Gesturing Hand/s", labels = c("One", "Two")) +
  scale_x_discrete(labels = c('singular', 'plural'))

# ============ 
# Articulatory economy
# ============ 

# Compare OneVsBoth across Phrase:
(xtab <- table(df2$OneVsBoth, df2$Phrase))   # Tabulate raw counts
(prop.table <- prop.table(xtab, 2) * 100)   # Tabulate proportions
chisq.test(xtab)   # Chi-square test (violates independence)
cramersV(xtab)    # Cramer's V / phi: Small = 0.1, Medium = 0.3, Large = 0.8

# ============ 
# Post-hoc halfway vs. open 
# ============

# Exclude HandConfig variables apart from 'open' and 'halfway': 
df2 <- filter(df, HandConfig %in% c('open', 'halfway')) %>%
  as.data.frame(df2)

# Compare verb vs. adjective:
calc2(df2, "VerbVsAdj", "HandConfig")

# ============
# Re-analysis of Casasanto and Jasmin (2010):
# ============

# Create data frame comparing gesture vs. no gesture for negative and positive clauses
(cas_jas <- data.frame(NegativeValence = c(449,237), PositiveValence = c(314, 292)))    # Input column names and values    
rownames(cas_jas) <- c("Gesture","NoGesture")   # Input row names
cas_jas <- as.matrix(cas_jas)   # Turn data frame into data matrix
prop.table(cas_jas, 2) * 100    # Taculate proportions
chisq.test(cas_jas)   # Chi-squared test == significant
cramersV(cas_jas)   # Cramer's V / phi: Small = 0.1, Medium = 0.3, Large = 0.8

# Check how many unique speakers there are in my dataset:
length(unique(df$SpeakerName))    

# ============
# Other
# ============

# Calculate NAs for SpeakerName:
(xtab <- table(is.na(df$SpeakerName)))    # Tabulate raw data
prop.table(xtab)    # Tabulate proportions
