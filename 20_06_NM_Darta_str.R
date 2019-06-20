rm(list = ls())
nursery<- read.csv(file.choose(), header = T, )
View(nursery)
str(nursery)
library(tidyverse)
str(nursery)
summary(nursery)
list(is.na(nursery$seedling_height))
nursery[!is.na(nursery)]
nursery1<-na.omit(nursery)

# lookup table for treatment
treat <- c("Control", 
           "Poultry",
           "Vermi",
           "FYM",
           "Myco")

treat1 <- c(treat, treat)

# convert dataset to tibble format (better printing features than regular data.frame)
nursery1 <- tbl_df(nursery) %>%
  # mutate creates a new column based on existing columns and other data
  mutate(month1 = as.numeric(substr(month,1,1)),
         dead   = ifelse(month1 > 2 & is.na(leaves), "dead", "alive"), # substr selects a subset of a character string (in this case first to first letter)
         # create a numerical variable for treatment
         treatnum = as.numeric(gsub("T", "", treatment)), # gsub is search an replace
         NPK      = ifelse(treatnum > 5, "NPK", "Control"),
         treat    = treat1[treatnum]) 
nursery1

# list the number of NA values for all columns in the dataset
sapply(nursery1, function(x) sum(is.na(x)))  

# find rows with missing observations in "leaves"
filter(nursery, is.na(leaves))

# visual check for seedling_height
nursery1 %>%
  filter(seedling_height < 30) %>%
  ggplot(aes(x = month1, y = seedling_height)) +
  geom_line(aes(group = ID), alpha = 0.6) +
  geom_point(aes(col = dead), alpha = 0.6) +  
  facet_wrap(~treatment) +
  theme_minimal()

# check single trees separately to find offending trees
nursery1 %>%
  filter(treatment == "T1") %>%
  ggplot(aes(x = month1, y = seedling_height)) +
  geom_line() +
  facet_wrap(~ID) +
  theme_minimal()

# visual check for leaves
nursery1 %>%
  ggplot(aes(x = month1, y = leaves, col = treatment)) +
  geom_line(aes(group = ID), alpha = 0.6) +
  facet_wrap(~treatment) +
  theme_minimal()

# check single trees separately to find offending trees
nursery1 %>%
  filter(treatment == "T1") %>%
  ggplot(aes(x = month1, y = leaves)) +
  geom_line() +
  facet_wrap(~ID) +
  theme_minimal()





# reeeeaaaaaallllllllyyyy simple way of calculating absolute growth for all trees
# define function that calculates growth based on month and height
growthfun <- function(x, month) {
  # get values that are not NA
  x1 <- x[!is.na(x)]
  m  <- month[!is.na(x)]
  # get number of observations
  n <- length(x1)
  # return difference between first and last value divided by time difference in years
  return((x1[n] - x1[1])/ ((m[n] - m[1]) / 12))
}

# summarize dataset with the growth function
growth <- nursery1 %>%
  filter(dead == "alive") %>%
  group_by(ID, treatment,NPK, treat, replication) %>%
  summarize(hgr = growthfun(seedling_height, month1),
            dgr = growthfun(col_diameter, month1),
            lgr = growthfun(leaves, month1),
            # really stupid estimate of biomass (don't try at home)
            bgr = growthfun(pi * col_diameter^2 / 4 * seedling_height, month1))
growth  



# first visual inspection for height growth
growth %>%
  filter(abs(hgr) < 200) %>%
  ggplot(aes(x = treat, y = hgr, col = NPK)) +
  geom_boxplot()


# reaaaaallllllllyyyyy simple analysis with ANOVA
anova1 <- lm(hgr ~ NPK * treat, filter(growth, hgr < 200))

# model inspection function
modplot <- function(mod){
  op <- par(mfrow = c(2,2))
  plot(mod)
  par(op)
}
# check model inspection plots
modplot(anova1)

# check if there are any effects at all
anova(anova1)

summary(anova1) # there is a significant effect of treatment 10



# ... but --- we did not correct for replication effects
growth %>%
  filter(abs(hgr) < 200) %>%
  ggplot(aes(x = treatment, y = hgr, fill = replication)) +
  geom_boxplot(alpha = 0.4)





