# Libraries ---- 
library(tidyverse)
library(rdrobust)
library(rddtools)

# Data ---- 
df <- read_csv("./Data/final.csv")
df$parental_educ <- (df$father_educ + df$mother_educ)/2

# Education ----
reg <- lm(educ~parental_educ, data = df)
summary(reg)

## Evolution 
evol <- NULL
j = 1
for (y in min(df$year):max(df$year)) {
  reg <- df %>% filter(year == y) %>%
    lm(data = ., educ~parental_educ)
  
  evol$year[j]     <- y
  evol$absolut[j]  <- reg$coefficients[1]
  evol$relative[j] <- reg$coefficients[2]
  
  j = j + 1
}

evol <- data.frame(evol)

ggplot(evol) + aes(x = year, 
                   y = relative) +
  geom_point(color = "#112445", alpha = 0.75) +
  geom_smooth(color = alpha("#B22222", 0.75), alpha = 0) +
  ggthemes::theme_clean()  +
  theme(
    legend.position = "none",
    plot.background = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  ) +
  xlab("Birth Year") +
  labs(y = expression("Persistence " * beta))

ggsave("./Plots/Evol-Educ.png")

## Persistence
rdd_df <- rdd_data(evol$relative, evol$year, cutpoint = 1981)
bw = rdd_bw_ik(rdd_df)

rdd_bw_cct_plot(rdd_df)

p = 2

rdd_reg_lm(rdd_df, order = p, bw = floor(bw))
rdd <- rdd_bw_cct_plot(rdd_df, p = p, h = floor(bw))

rdd$rdplot + xlab("Birth Year") +
  ylab("Persistence") +
  labs(subtitle = paste("BW = ", floor(bw), "\n", "Order = ", p, sep = "")) +
  xlim(c(1963, 1993)) +
  ylim(c(0.4, 0.76)) +
  ggtitle("") +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    text = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    axis.line.y = element_blank(), 
    legend.key.size = unit(0.2, units = "cm"),
    legend.background = element_blank(),
    legend.position = "none"
  )

ggsave("./Plots/RDD-Educ_1.png")

## Years of Schooling 
rdd_df <- rdd_data(df$educ, df$year, cutpoint = 1981)
bw = rdd_bw_ik(rdd_df)

rdd_df <- df %>% filter(year >= (1981 - floor(bw)) & year <= (1981 + floor(bw)))
rdd_df <- rdd_data(rdd_df$educ, rdd_df$year, cutpoint = 1981)
rdd_bw_cct_plot(rdd_df)

p = 2

rdd_reg_lm(rdd_df, order =  p, bw = floor(bw))
rdd <- rdd_bw_cct_plot(rdd_df, p = p, h = floor(bw))

rdd$rdplot + xlab("Birth Year") +
  ylab("Years") +
  ggtitle("") +
  labs(subtitle = paste("BW = ", floor(bw), "\n", "Order = ", p, sep = "")) +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    text = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    axis.line.y = element_blank(), 
    legend.key.size = unit(0.2, units = "cm"),
    legend.background = element_blank(),
    legend.position = "none"
  )

ggsave("./Plots/RDD-Educ_2.png")

# Occupation ----
rdd_df <- rdd_data(df$ocup_01, df$year, cutpoint = 1981)
bw = rdd_bw_ik(rdd_df)

rdd_df <- df %>% filter(year >= (1981 - floor(bw)) & year <= (1981 + floor(bw)))
rdd_df <- rdd_data(rdd_df$ocup_01, rdd_df$year, cutpoint = 1981)
rdd_bw_cct_plot(rdd_df)

p = 3

rdd_reg_lm(rdd_df, order = p , bw = floor(bw))
rdd <- rdd_bw_cct_plot(rdd_df, p = p, h = floor(bw))

rdd$rdplot + xlab("Birth Year") +
  ylab("Proportion") +
  ggtitle("") +
  labs(subtitle = paste("BW = ", floor(bw), "\n", "Order = ", p, sep = "")) +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    text = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    axis.line.y = element_blank(), 
    legend.key.size = unit(0.2, units = "cm"),
    legend.background = element_blank(),
    legend.position = "none"
  )

ggsave("./Plots/RDD-Ocup_1.png")

rdd_df <- rdd_data(df$ocup_09, df$year, cutpoint = 1981)
bw = rdd_bw_ik(rdd_df)

rdd_df <- df %>% filter(year >= (1981 - floor(bw)) & year <= (1981 + floor(bw)))
rdd_df <- rdd_data(rdd_df$ocup_09, rdd_df$year, cutpoint = 1981)
rdd_bw_cct_plot(rdd_df)

p = 3

rdd_reg_lm(rdd_df, order = p , bw = floor(bw))
rdd <- rdd_bw_cct_plot(rdd_df, p = p, h = floor(bw))

rdd$rdplot + xlab("Birth Year") +
  ylab("Proportion") +
  labs(subtitle = paste("BW = ", floor(bw), "\n", "Order = ", p, sep = "")) +
  ggtitle("") +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    text = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    axis.line.y = element_blank(), 
    legend.key.size = unit(0.2, units = "cm"),
    legend.background = element_blank(),
    legend.position = "none"
  )

ggsave("./Plots/RDD-Ocup_2.png")

rdd_df <- rdd_data(df$ocup_04, df$year, cutpoint = 1981)
bw = rdd_bw_ik(rdd_df)

rdd_df <- df %>% filter(year >= (1981 - floor(bw)) & year <= (1981 + floor(bw)))
rdd_df <- rdd_data(rdd_df$ocup_04, rdd_df$year, cutpoint = 1981)
rdd_bw_cct_plot(rdd_df)

p = 3

rdd_reg_lm(rdd_df, order = p, bw = floor(bw))
rdd <- rdd_bw_cct_plot(rdd_df, p = p, h = floor(bw))

rdd$rdplot + xlab("Birth Year") +
  ylab("Proportion") +
  labs(subtitle = paste("BW = ", floor(bw), "\n", "Order = ", p, sep = "")) +
  ggtitle("") +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    text = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    axis.line.y = element_blank(), 
    legend.key.size = unit(0.2, units = "cm"),
    legend.background = element_blank(),
    legend.position = "none"
  )

ggsave("./Plots/RDD-Ocup_3.png")

# Wealth
# Education ----
reg <- lm(wealth_individual~wealth_parental, data = df)
summary(reg)

## Evolution 
evol <- NULL
j = 1
for (y in min(df$year):max(df$year)) {
  reg <- df %>% filter(year == y) %>%
    lm(data = ., wealth_individual~wealth_parental)
  
  evol$year[j]     <- y
  evol$absolut[j]  <- reg$coefficients[1]
  evol$relative[j] <- reg$coefficients[2]
  
  j = j + 1
}

evol <- data.frame(evol)

ggplot(evol) + aes(x = year, 
                   y = relative) +
  geom_point(color = "#112445", alpha = 0.75) +
  geom_smooth(color = alpha("#B22222", 0.75), alpha = 0) +
  ggthemes::theme_clean()  +
  theme(
    legend.position = "none",
    plot.background = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  ) +
  xlab("Birth Year") +
  labs(y = expression("Persistence " * beta))

ggsave("./Plots/evol-Wealth.png")

rdd_df <- rdd_data(evol$relative, evol$year, cutpoint = 1981)
bw = rdd_bw_ik(rdd_df)

rdd_bw_cct_plot(rdd_df)

p = 2

rdd_reg_lm(rdd_df, order = p, bw = floor(bw))
rdd <- rdd_bw_cct_plot(rdd_df, p = p, h = floor(bw))

rdd$rdplot + xlab("Birth Year") +
  ylab("Persistence") +
  labs(subtitle = paste("BW = ", floor(bw), "\n", "Order = ", p, sep = "")) +
  xlim(c(1972, 1991)) +
  ylim(c(0.4, 0.76)) +
  ggtitle("") +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    text = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    axis.line.y = element_blank(), 
    legend.key.size = unit(0.2, units = "cm"),
    legend.background = element_blank(),
    legend.position = "none"
  )

ggsave("./Plots/RDD-Wealth.png")

rdd_df <- rdd_data(df$wealth_individual, df$year, cutpoint = 1981)
bw = rdd_bw_ik(rdd_df)

rdd_bw_cct_plot(rdd_df)

p = 3

rdd_reg_lm(rdd_df, order = p, bw = floor(bw))
rdd <- rdd_bw_cct_plot(rdd_df, p = p, h = floor(bw))

rdd$rdplot + xlab("Birth Year") +
  ylab("Rank") +
  labs(subtitle = paste("BW = ", floor(bw), "\n", "Order = ", p, sep = "")) +
  xlim(c(1972, 1991)) +
  ylim(c(48.5, 52))+ 
  ggtitle("") +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    text = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    axis.line.y = element_blank(), 
    legend.key.size = unit(0.2, units = "cm"),
    legend.background = element_blank(),
    legend.position = "none"
  )

ggsave("./Plots/RDD-Wealth_2.png")
