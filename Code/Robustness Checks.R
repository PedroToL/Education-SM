# Libraries ----
library(tidyverse)
library(rddtools)

# Data ----
df <- read_csv("./Data/final.csv")
mobility <- read_csv("./Data/predictions_mob.csv")

df$Upward <- mobility$`0`
df$Downward <- mobility$`1`
df$Equal <- mobility$`2`

# Upward mobility ----
rdd_df <- rdd_data(df$Upward, df$year, cutpoint = 1981)
bw = floor(rdd_bw_ik(rdd_df))

for (i in -3:3) {
  print(floor(bw) + i)
  rdd_df <- df %>% filter(year >= (1981 - (floor(bw) + i)) & year <= (1981 + (floor(bw) + i)))
  rdd_df <- rdd_data(rdd_df$Upward, rdd_df$year, cutpoint = 1981)
  
  p = 4
  
  a <- rdd_reg_lm(rdd_df, order = p, bw = (floor(bw) + i))
  b <- clusterInf(a, clusterVar = "ID")

  stargazer::stargazer(b, type = "text")
}

for (p in 1:4) {
  print(p)
  rdd_df <- df %>% filter(year >= (1981 - (floor(bw))) & year <= (1981 + (floor(bw))))
  rdd_df <- rdd_data(rdd_df$Upward, rdd_df$year, cutpoint = 1981)
  
  a <- rdd_reg_lm(rdd_df, order = p, bw = (floor(bw)))
  b <- clusterInf(a, clusterVar = "ID")
  
  stargazer::stargazer(b, type = "text")
}

# Downward Mobility ----
rdd_df <- rdd_data(df$Downward, df$year, cutpoint = 1981)
bw = floor(rdd_bw_ik(rdd_df))

for (i in -2:2) {
  print(floor(bw) + i)
  rdd_df <- df %>% filter(year >= (1981 - (floor(bw) + i)) & year <= (1981 + (floor(bw) + i)))
  rdd_df <- rdd_data(rdd_df$Downward, rdd_df$year, cutpoint = 1981)
  
  p = 3
  
  a <- rdd_reg_lm(rdd_df, order = p, bw = (floor(bw) + i))
  b <- clusterInf(a, clusterVar = "ID")
  
  stargazer::stargazer(b, type = "text")
}

for (p in 1:4) {
  rdd_df <- df %>% filter(year >= (1981 - (floor(bw))) & year <= (1981 + (floor(bw))))
  rdd_df <- rdd_data(rdd_df$Downward, rdd_df$year, cutpoint = 1981)
  
  a <- rdd_reg_lm(rdd_df, order = p, bw = (floor(bw)))
  b <- clusterInf(a, clusterVar = "ID")
  
  stargazer::stargazer(b, type = "text")
}

# No mobility ----
rdd_df <- rdd_data(df$Equal, df$year, cutpoint = 1981)
bw = floor(rdd_bw_ik(rdd_df))

for (i in -2:2) {
  print(floor(bw) + i)
  rdd_df <- df %>% filter(year >= (1981 - (floor(bw) + i)) & year <= (1981 + (floor(bw) + i)))
  rdd_df <- rdd_data(rdd_df$Equal, rdd_df$year, cutpoint = 1981)
  rdd_bw_cct_plot(rdd_df)
  
  p = 2
  
  a <- rdd_reg_lm(rdd_df, order = p, bw = (floor(bw) + i))
  b <- clusterInf(a, clusterVar = "ID")
  
  stargazer::stargazer(b, type = "text")
}

for (p in 1:4) {
  rdd_df <- df %>% filter(year >= (1981 - (floor(bw))) & year <= (1981 + (floor(bw))))
  rdd_df <- rdd_data(rdd_df$Equal, rdd_df$year, cutpoint = 1981)

  a <- rdd_reg_lm(rdd_df, order = p, bw = (floor(bw)))
  b <- clusterInf(a, clusterVar = "ID")
  
  stargazer::stargazer(b, type = "text")
}

# Persistence ---- 
rdd_df <- rdd_data(evol$relative, evol$year, cutpoint = 1981)
bw = floor(rdd_bw_ik(rdd_df))

for (i in -2:2) {
  print(floor(bw) + i)
  
  p = 4
  
  a <- rdd_reg_lm(rdd_df, order = p, bw = (floor(bw) + i))
  b <- clusterInf(a, clusterVar = "ID")
  
  stargazer::stargazer(b, type = "text")
}

for (p in 1:4) {
  a <- rdd_reg_lm(rdd_df, order = p, bw = (floor(bw)))
  b <- clusterInf(a, clusterVar = "ID")
  
  stargazer::stargazer(b, type = "text")
}

# Absolute ---- 
rdd_df <- rdd_data(evol$absolut, evol$year, cutpoint = 1981)
bw = floor(rdd_bw_ik(rdd_df))

for (i in -2:2) {
  print(floor(bw) + i)
  
  p = 3
  
  a <- rdd_reg_lm(rdd_df, order = p, bw = (floor(bw) + i))
  b <- clusterInf(a, clusterVar = "ID")
  
  stargazer::stargazer(b, type = "text")
}

for (p in 1:4) {
  a <- rdd_reg_lm(rdd_df, order = p, bw = (floor(bw)))
  b <- clusterInf(a, clusterVar = "ID")
  
  stargazer::stargazer(b, type = "text")
}


# Cutoff change 
## RDD ---
for(i in c(-1, 1)) {
  print(i)
  # Upward
  rdd_df <- rdd_data(df$Upward, df$year, cutpoint = (1981 + i))
  bw = rdd_bw_ik(rdd_df)
  
  p = 4
  
  a <- rdd_reg_lm(rdd_df, order = p, bw = floor(bw))
  b <- clusterInf(a, clusterVar = "ID")
  stargazer::stargazer(b, type = "text")
  
  # Downward
  rdd_df <- rdd_data(df$Downward, df$year, cutpoint = (1981 + i))
  bw = rdd_bw_ik(rdd_df)
  
  p = 3
  
  a <- rdd_reg_lm(rdd_df, order = p, bw = floor(bw))
  b <- clusterInf(a, clusterVar = "ID")
  stargazer::stargazer(b, type = "text")
  
  # Equal 
  rdd_df <- rdd_data(df$Equal, df$year, cutpoint = (1981 + i))
  bw = rdd_bw_ik(rdd_df)
  
  p = 2
  
  a <- rdd_reg_lm(rdd_df, order = p, bw = floor(bw))
  b <- clusterInf(a, clusterVar = "ID")
  stargazer::stargazer(b, type = "text")
  
  # Persistence
  rdd_df <- rdd_data(evol$relative, evol$year, cutpoint = (1981 + i))
  bw = rdd_bw_ik(rdd_df)
  
  rdd_bw_cct_plot(rdd_df)
  
  p = 4
  
  a <- rdd_reg_lm(rdd_df, order = p, bw = floor(bw))
  b <- clusterInf(a, clusterVar = "ID")
  b
  stargazer::stargazer(b, type = "text")
  
  # Absolute 
  rdd_df <- rdd_data(evol$absolut, evol$year, cutpoint = (1981 + i))
  bw = rdd_bw_ik(rdd_df)
  
  rdd_bw_cct_plot(rdd_df)
  
  p = 3
  
  a <- rdd_reg_lm(rdd_df, order = p, bw = floor(bw))
  b <- clusterInf(a, clusterVar = "ID")
  b
  stargazer::stargazer(b, type = "text")
}
