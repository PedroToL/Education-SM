# libraries ----
library(tidyverse)

# Data ----
df <- read_csv("./Data/final.csv")

# Density Plots ----
df %>% filter(substr(ID, 1, 1) == "m") %>% ggplot() + aes(x = year) +
  geom_density(fill = "#112446", alpha = .75, adjust = 2L) +
  geom_vline(aes(xintercept = 1981),
             linetype = "longdash",
             color = "black",
             alpha = 0.75) +
  xlab("Birth Year") + ylab("Density") +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.line.y = element_blank() 
  )

ggsave("./Plots/Density_MMSI.png")

df %>% filter(substr(ID, 1, 1) == "e") %>% ggplot() + aes(x = year) +
  geom_density(fill = "#112446", alpha = .75, adjust = 2L) +
  geom_vline(aes(xintercept = 1981),
             linetype = "longdash",
             color = "black",
             alpha = 0.75) +
  xlab("Birth Year") + ylab("Density") +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.line.y = element_blank() 
  )

ggsave("./Plots/Density_EMOVI.png")

ggplot(df) + aes(x = ntile(SEI_parental, 50), 
                 y = ntile(SEI_individual, 50)) +
  ggtitle(" ") +
  geom_line(
    aes(y = ntile(SEI_parental, 50)),
    size = 0.75,
    alpha = 0.5, 
    linetype = 2) +
  geom_smooth(method = "lm", color = alpha("#B22222", 0.75)) +
  xlab("Parental Rank") + 
  ylab("Children's Rank") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 51),
                     breaks = c(seq(0, 50, 15))) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 51),
                     breaks = c(seq(0, 50, 15))) +
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

ggsave("./Plots/Rank-Rank-50.png")

reg <- lm(ntile(SEI_individual, 50)~ntile(SEI_parental,50), data = df)
summary(reg)

ggplot(df) + aes(x = ntile(SEI_parental, 25), 
                 y = ntile(SEI_individual, 25)) +
  ggtitle(" ") +
  geom_line(
    aes(y = ntile(SEI_parental, 25)),
    size = 0.75,
    alpha = 0.5, 
    linetype = 2) +
  geom_smooth(method = "lm", color = alpha("#B22222", 0.75)) +
  xlab("Parental Rank") + 
  ylab("Children's Rank") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 26),
                     breaks = c(seq(0, 25, 5))) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 26),
                     breaks = c(seq(0, 25, 5))) +
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

ggsave("./Plots/Rank-Rank-25.png")

reg <- lm(ntile(SEI_individual, 25)~ntile(SEI_parental, 25), data = df)
summary(reg)

