# Libraries ----
library(tidyverse)
library(ggsankey)
library(rdrobust)
library(rddtools)
library(ggpubr)
library(merDeriv)

# Data ----
df <- read_csv("./Data/final.csv")

# Analysis ----
## Rank-Rank 
reg <- lm(SEI_individual~SEI_parental, data = df)
summary(reg)

ggplot(df) + aes(x = SEI_parental, 
                 y = SEI_individual) +
  ggtitle(" ") +
  geom_line(
    aes(y = SEI_parental),
    size = 0.75,
    alpha = 0.5, 
    linetype = 2) +
  geom_smooth(method = "lm", color = alpha("#B22222", 0.75)) +
  xlab("Parental Rank") + 
  ylab("Children's Rank") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 101),
                     breaks = c(seq(0, 100, 25))) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 101),
                     breaks = c(seq(0, 100, 25))) +
  ggthemes::theme_clean() +
  theme(
    plot.background   = element_blank(),
    axis.text         = element_text(size = 10),
    axis.title        = element_text(size = 12),
    axis.line.y       = element_blank(), 
    legend.key.size   = unit(0.2, units = "cm"),
    legend.background = element_blank(),
    legend.position   = "right"
  )

ggsave("./Plots/Rank-Rank.png")

## Evolution 
evol <- NULL
j = 1
for (y in min(df$year):max(df$year)) {
  reg <- df %>% filter(year == y) %>%
    lm(data = ., SEI_individual~SEI_parental)
  
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
  scale_y_continuous(breaks = c(seq(0.55, 0.85, 0.05))) +
  theme(
    legend.position = "none",
    plot.background = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15)
  ) +
  xlab("Birth Year") +
  labs(y = expression("Persistence " * beta))

ggsave("./Plots/Evo_Rel.png")

ggplot(evol) + aes(x = year, 
                   y = absolut) +
  geom_point(color = "#112445", alpha = 0.75) +
  geom_smooth(color = alpha("#B22222", 0.75), alpha = 0) +
  ggthemes::theme_clean()  +
  theme(
    legend.position = "none",
    plot.background = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15)
  ) +
  xlab("Birth Year") +
  labs(y = expression("Absolute " * alpha))

ggsave("./Plots/Evo_Abs.png")

df %>% group_by(Q_parental) %>%
  mutate(
    n = n()
  ) %>% ungroup() %>%
  group_by(Q_parental, Q_individual) %>%
  summarise(
    n = mean(n),
    n_ = n(),
    perc = n_/n*100
  ) %>%
ggplot() +
  aes(x = Q_individual, 
      weights = perc, fill = factor(Q_individual)) +
  geom_bar() +
  facet_wrap(vars(Q_parental), nrow = 1L) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60),
                     breaks = c(seq(0, 60, 10))) +
  scale_fill_manual(values = c(alpha("#112445", 0.75), alpha("#B22222", 0.75),
                               alpha("#228B22", 0.75), alpha("#FF8C00", 0.75),
                               alpha("#46337E", 0.75))) +
  ggthemes::theme_clean()  +
  labs(subtitle = "Paretnal Quintile") +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(hjust = 0.5),
    plot.background = element_blank(),
    axis.line.y     = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  ) +
  xlab("Children's Quintile") +
  ylab("%")

ggsave("./Plots/Tansition Matrix 1.png")

total <- nrow(df)
perc <- df %>%
  make_long(Q_parental, Q_individual) %>%
  group_by(node)%>%
  tally() %>%
  group_by(node)%>%
  mutate(pct = n/total)

df  %>%
  make_long(Q_parental, Q_individual) %>%
  full_join(perc, by = "node") %>%
  mutate(
    node = as.numeric(node),
    next_node = as.numeric(next_node)) %>%
  ggplot() + aes(x = x,
                 next_x = next_x,
                 node = node, 
                 next_node = next_node, 
                 fill = factor(node),
                 label = paste0("Q-", as.double(node))) +
  geom_alluvial(flow.alpha = .5) +
  geom_alluvial_text(size = 2.5,
                     color = "white",
                     vjust = 0.45) +
  scale_x_discrete(labels = c("Parental", "Children's")) +
  scale_fill_manual(values = c(alpha("#112445", 0.75), alpha("#B22222", 0.75),
                               alpha("#228B22", 0.75), alpha("#FF8C00", 0.75),
                               alpha("#46337E", 0.75))) +
  theme_void() +
  theme(
    legend.position     = "none",
    plot.title = element_text(size = 15,
                              hjust = 0.28),
    plot.subtitle = element_text(size = 10,
                                 hjust = 0.33),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0.8),
    plot.margin = margin(0.1, -4, 0.1, -4, "cm"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  ) +
  labs(
    x = "SEI Quintile"
  ) 

ggsave("./Plots/Tansition Matrix 2.png")

df$Upward   <- ifelse(df$Q_individual > df$Q_parental, 1, 0)
df$Downward <- ifelse(df$Q_individual < df$Q_parental, 1, 0)
df$Equal    <- ifelse(df$Q_individual == df$Q_parental, 1, 0)

df %>% group_by(year) %>% 
  summarise(
    n = n(), 
    `Upward Mobility` = mean(Upward),
    `Downward Mobility` = mean(Downward),
    `No Mobility` = mean(Equal)
  ) %>%
  pivot_longer(cols = c(`Upward Mobility`, `Downward Mobility`, `No Mobility`),
               names_to = "Mobility",
               values_to = "Value") %>%
  select(year, Mobility, Value) %>%
  mutate(
    Mobility = factor(Mobility, levels = unique(Mobility))
  ) %>%
  ggplot() + aes(x = year, y = Value, fill = Mobility) +
  geom_area(alpha = 0.75) +
  xlab("") + ylab("Percentage") +
  geom_vline(aes(xintercept = 1981),
             linetype = "longdash",
             color = "black",
             alpha = 0.75) +
  scale_fill_manual(values = c("#228B22", "#B22222", "#112445")) +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.line.y = element_blank(), 
    legend.key.size = unit(0.2, units = "cm"),
    legend.background = element_blank(),
    legend.position = "bottom",
  )

ggsave("./Plots/Tansition Matrix 3.png")

df %>% group_by(year) %>% 
  summarise(
    n = n(), 
    `Upward Mobility` = mean(Upward),
    `Downward Mobility` = mean(Downward),
    `No Mobility` = mean(Equal)
  ) %>%
  pivot_longer(cols = c(`Upward Mobility`, `Downward Mobility`, `No Mobility`),
               names_to = "Mobility",
               values_to = "Value") %>%
  select(year, Mobility, Value) %>%
  mutate(
    Mobility = factor(Mobility, levels = unique(Mobility))
  ) %>%
  ggplot() + aes(x = year, y = Value, color = Mobility) +
  geom_point(alpha = 0.75) +
  geom_smooth(alpha = 0) +
  xlab("") + ylab("Percentage") +
  scale_color_manual(values = c("#228B22", "#B22222", "#112445")) +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    text = element_text(size = 17),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 17),
    axis.line.y = element_blank(), 
    legend.key.size = unit(0.2, units = "cm"),
    legend.background = element_blank(),
    legend.position = "none",
    rect = element_blank()
  ) +
  facet_wrap(~Mobility, nrow = 3)

ggsave("./Plots/Tansition Matrix 4.png")

df$mobility <- ifelse(df$Upward == 1, 0, 
                      ifelse(df$Downward == 1, 1, 2))

write.csv(select(df, mobility, year, sex, rururb, ent, ind_lang), "./Data/model.csv")

source("./Code/Version 1/XGBoost.py")

mobility <- read_csv("./Data/predictions_mob.csv")

df$Upward <- mobility$`0`
df$Downward <- mobility$`1`
df$Equal <- mobility$`2`

df %>% group_by(year) %>%
  summarise(
    n = n(), 
    `Upward Mobility` = mean(Upward),
    `Downward Mobility` = mean(Downward),
    `No Mobility` = mean(Equal)
  ) %>%
  pivot_longer(cols = c(`Upward Mobility`, `Downward Mobility`, `No Mobility`),
               names_to = "Mobility",
               values_to = "Value") %>%
  select(year, Mobility, Value) %>%
  mutate(
    Mobility = factor(Mobility, levels = unique(Mobility))
  ) %>%
  ggplot() + aes(x = year, y = Value, color = Mobility) +
  geom_point(alpha = 0.75) +
  geom_smooth(alpha = 0) +
  xlab("") + ylab("Probability") +
  scale_color_manual(values = c("#228B22", "#B22222", "#112445")) +
  facet_wrap(~Mobility, nrow = 3) +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    text = element_text(size = 17),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 17),
    axis.line.y = element_blank(), 
    legend.key.size = unit(0.2, units = "cm"),
    legend.background = element_blank(),
    legend.position = "none",
    rect = element_blank()
  )


ggsave("./Plots/Tansition Matrix 5.png")

## RDD ---
df$D <- ifelse(df$year >= 1981, 1, 0)
df$year_centered <- df$year - 1981

rdd_df <- rdd_data(df$Upward, df$year, cutpoint = 1981)
bw = rdd_bw_ik(rdd_df)
summary(rdd_bw_cct_plot(rdd_df))

rdd_df <- df %>% filter(year >= (1981 - floor(bw)) & year <= (1981 + floor(bw)))
rdd_df <- rdd_data(rdd_df$Upward, rdd_df$year, cutpoint = 1981)
summary(rdd_bw_cct_plot(rdd_df))

p = 4

a <- rdd_reg_lm(rdd_df, order = p, bw = floor(bw))
b <- clusterInf(a, clusterVar = "ID")
b
stargazer::stargazer(b, type = "text")

rdd <- rdd_bw_cct_plot(rdd_df, p = p, h = floor(bw))

rdd$rdplot + xlab("Birth Year") +
  ylab("Probability") +
  labs(subtitle = paste("BW = ", floor(bw), "\n", "Order = ", 4, sep = "")) +
  ggtitle("") +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    text = element_text(size = 17),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 17),
    axis.line.y = element_blank(), 
    legend.key.size = unit(0.2, units = "cm"),
    legend.background = element_blank(),
    legend.position = "none"
  )

ggsave("./Plots/RDD-Upward.png")

## Downward 
rdd_df <- rdd_data(df$Downward, df$year, cutpoint = 1981)
bw = rdd_bw_ik(rdd_df)

rdd_df <- df %>% filter(year >= (1981 - floor(bw)) & year <= (1981 + floor(bw)))
rdd_df <- rdd_data(rdd_df$Downward, rdd_df$year, cutpoint = 1981)
rdd_bw_cct_plot(rdd_df)

p = 3

a <- rdd_reg_lm(rdd_df, order = p, bw = floor(bw))
b <- clusterInf(a, clusterVar = "ID")
b
stargazer::stargazer(b, type = "text")

rdd <- rdd_bw_cct_plot(rdd_df, p = p, h = floor(bw))

rdd$rdplot + xlab("Birth Year") +
  ylab("Probability") +
  ggtitle("") +
  labs(subtitle = paste("BW = ", floor(bw), "\n", "Order = ", p, sep = "")) +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    text = element_text(size = 17),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 17),
    axis.line.y = element_blank(), 
    legend.key.size = unit(0.2, units = "cm"),
    legend.background = element_blank(),
    legend.position = "none"
  )

ggsave("./Plots/RDD-Downward.png")

# No mobility
rdd_df <- rdd_data(df$Equal, df$year, cutpoint = 1981)
bw = rdd_bw_ik(rdd_df)

rdd_df <- df %>% filter(year >= (1981 - floor(bw)) & year <= (1981 + floor(bw)))
rdd_df <- rdd_data(rdd_df$Equal, rdd_df$year, cutpoint = 1981)
rdd_bw_cct_plot(rdd_df)

p = 1

a <- rdd_reg_lm(rdd_df, order = p, bw = floor(bw))
b <- clusterInf(a, clusterVar = "ID")
b
stargazer::stargazer(b, type = "text")

rdd <- rdd_bw_cct_plot(rdd_df, p = p, h = floor(bw))

rdd$rdplot + xlab("Birth Year") +
  ylab("Probability") +
  labs(subtitle = paste("BW = ", floor(bw), "\n", "Order = ", p, sep = "")) +
  ggtitle("") +
  scale_x_continuous(breaks = c(1976, 1978, 1980, 1982, 1984, 1986)) +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    text = element_text(size = 17),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 17),
    axis.line.y = element_blank(), 
    legend.key.size = unit(0.2, units = "cm"),
    legend.background = element_blank(),
    legend.position = "none"
  )

ggsave("./Plots/RDD-Equal.png")

# Relative 
rdd_df <- rdd_data(evol$relative, evol$year, cutpoint = 1981)
bw = rdd_bw_ik(rdd_df)

rdd_bw_cct_plot(rdd_df)

p = 4

a <- rdd_reg_lm(rdd_df, order = p, bw = floor(bw))
b <- clusterInf(a, clusterVar = "ID")
b
stargazer::stargazer(b, type = "text")

rdd <- rdd_bw_cct_plot(rdd_df, p = p, h = floor(bw))

rdd$rdplot + xlab("Birth Year") +
  ylab("Persistence") +
  ggtitle("") +
  labs(subtitle = paste("BW = ", floor(bw), "\n", "Order = ", p, sep = "")) +
  xlim(c(1960, 1992)) +
  ylim(c(0.5, 0.8)) +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    text = element_text(size = 17),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 17),
    axis.line.y = element_blank(), 
    legend.key.size = unit(0.2, units = "cm"),
    legend.background = element_blank(),
    legend.position = "none"
  )

ggsave("./Plots/RDD-relative.png")

# Absolute
rdd_df <- rdd_data(evol$absolut, evol$year, cutpoint = 1981)
bw = rdd_bw_ik(rdd_df)

rdd_bw_cct_plot(rdd_df)

p = 3

a <- rdd_reg_lm(rdd_df, order = p, bw = floor(bw))
b <- clusterInf(a, clusterVar = "ID")
b
stargazer::stargazer(b, type = "text")

rdd <- rdd_bw_cct_plot(rdd_df, p = p, h = floor(bw))

rdd$rdplot + xlab("Birth Year") +
  ylab("Absolute") +
  labs(subtitle = paste("BW = ", floor(bw), "\n", "Order = ", p, sep = "")) +
  ggtitle("") +
  xlim(c(1960, 1993)) +
  ylim(c(10, 25)) +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    text = element_text(size = 17),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 17),
    axis.line.y = element_blank(), 
    legend.key.size = unit(0.2, units = "cm"),
    legend.background = element_blank(),
    legend.position = "none"
  )

ggsave("./Plots/RDD-abso.png")


