# general data prep script for Political Distance project
# project looks at explaining why some countries are politically distant from others/neighbors

library(tidyverse)
library(ggplot2)
library(peacesciencer)
library(vdemdata)

## To-do
# directed dyad-level df: spatial weights from min distance, ccode2 vars (polyarchy)
# country-year df: weighted sum of vars

df_dyad <- create_dyadyears(system = "cow", subset_years = c(1946:2019)) %>% # stopping in 2019 because of data availability on mindist
  add_minimum_distance()

vdem1 <- vdem %>% 
  dplyr::rename(., ccode1 = COWcode, v2x_polyarchy1 = v2x_polyarchy) %>%
  filter(., year>1945) %>%
  select(., ccode1, year, v2x_polyarchy1) %>%
  arrange(., ccode1, year)

vdem2 <- vdem %>% 
  dplyr::rename(., ccode2 = COWcode, v2x_polyarchy2 = v2x_polyarchy) %>%
  filter(., year>1945) %>%
  select(., ccode2, year, v2x_polyarchy2) %>%
  arrange(., ccode2, year)

df_dyad <- df_dyad %>%
  left_join(., vdem2, by =c('ccode2', 'year'))

# spatial weight: inverse distance between i and j, normalized by the sum of the inverse distance between i and all j
df_dyad <- df_dyad %>%
  mutate(in_dist = 1/(mindist+1)) %>% # creating the inverse distance +1 because cont is coded as 0
  mutate(sum_in_dist = sum(in_dist, na.rm = TRUE), .by = c(ccode1, year)) %>% # sum of distance to all other states in country year
  mutate(w = in_dist / sum_in_dist) %>% # creating the spatial weight
  select(-c(in_dist, sum_in_dist)) # removing superfluous variables

# looking at the missing data
sum(is.na(df_dyad$w))
filter(df_dyad, is.na(w))
filter(df_dyad, is.na(w) & year>1974)
filter(df_dyad, is.na(w) & year>1990)
filter(df_dyad, is.na(w) & year>1995)
# insane amount of missing data for countries with values on polyarchy
filter(df_dyad, w!="NA")
filter(df_dyad, w!="NA" & year==2020)

# making sure the w sums to 1 per ccode1 year
test <- df_dyad %>% mutate(sum = sum(w, na.rm = TRUE), .by=c(ccode1, year))
# it does
sum(is.na(test$sum))

# add in polyarchy to create weighted mean
df <- df_dyad %>%
  mutate(w_poly = v2x_polyarchy2*w) %>%
  group_by(ccode1, year) %>%
  summarise(n_poly = sum(w_poly, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(., vdem1, by = c('ccode1', 'year'))

# time to validate
filter(df, year==1946)
filter(df, year==2001)

test <- df_dyad %>%
  filter(., ccode1==2 & year==2001) %>%
  mutate(w_poly = v2x_polyarchy2*w)
sum(test$w_poly, na.rm = TRUE)
filter(df, ccode1==2 & year==2001)
# same

sum(is.na(df$n_poly))



