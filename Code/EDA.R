# Libraries ----
library(tidyverse)
source("Code/Final/DataFunctions.R")

# Data ----
df <- read_csv("./Data/final.csv")

# EDA ----
## Tables
### Parental 
df %>% select(
  age, ends_with(".x"), starts_with("father"), starts_with("mother"), Q_parental
  ) %>% group_by(Q_parental) %>%
  summarise_all(
    list(mean = mean)
  ) %>% t() %>% write.csv("./Results/SEI/parental.csv")

df %>% select(
  age, ends_with(".y"), computer, internet, cellphone, starts_with("ocup"), educ, Q_individual
) %>% group_by(Q_individual) %>%
  summarise_all(
    list(mean = mean)
  ) %>% t() %>% write.csv("./Results/SEI/individual.csv")  

## Plots ----
### Sorting
df %>% ggplot() + aes(year) +
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

ggsave("./Plots/Year_Density.png")

### Education
df %>% 
  group_by(year) %>% summarise(
    Years = mean(educ)
  ) %>%
  ggplot() + aes(x = year,
                 y = Years) +
    geom_point(color = "#112446", 
               alpha = .75) + 
    geom_line(color = "#112446",
              alpha = .75) +
    geom_vline(
      aes(xintercept = 1982),
      linetype = "longdash",
      color = "black",
      alpha = 0.75) +
    xlab("Birth Year") +
    ylab("Average Years") +
    ggthemes::theme_clean() +
    theme(
      plot.background = element_blank(),
      axis.text       = element_text(size = 10),
      axis.title      = element_text(size = 12),
      axis.line.y     = element_blank() 
    )

ggsave("./Plots/Years_Schooling.png")

df %>% group_by(year) %>%
  summarise(
    n = n(),
    `Less than Primary` = sum(less_primary, na.rm = T)/n*100,
    Primary = sum(primary, na.rm = T)/n*100 ,
    Secondary = sum(secondary, na.rm = T) /n*100,
    `High School` = sum(high_school, na.rm = T)/n*100 ,
    University = sum(grad, na.rm = T)/n*100 ,
  ) %>% 
  pivot_longer(cols = -c(n, year), names_to = "Grade", values_to = "Percentage") %>%
  mutate(
    Grade = ordered(Grade, levels = c("University", "High School", "Secondary", "Primary", "Less than Primary"))
  ) %>%
  ggplot() + aes(x    = year, 
                 y    = Percentage,
                 fill = Grade) +
  geom_area(position = "stack",
            alpha    = 0.75) +
  geom_vline(
    aes(xintercept = 1982),
    linetype = "longdash",
    color = "black",
    alpha = 0.75) +
  xlab("Birth Year") +
  ylab("Percentage") +
  scale_fill_manual(values = c("#112445", "#B22222","#228B22",
                               "#FF8C00", "#46337E")) +
  ggthemes::theme_clean() +
  theme(
    plot.background   = element_blank(),
    axis.text         = element_text(size = 10),
    axis.title        = element_text(size = 12),
    axis.line.y       = element_blank(), 
    legend.key.size   = unit(0.2, units = "cm"),
    legend.background = element_blank(),
    legend.position   = "top"
  )

ggsave("./Plots/Grade_Schooling.png")

## SEI and Ranks
df %>% ggplot() + aes(x = min_max(y_individual)) +
  geom_density(
    aes(fill = "Children"),
    alpha = 0.75) +
  geom_density(
    aes(x    = min_max(y_parental),
        fill = "Parental"), 
    alpha = 0.75) +
  xlab("SES Index") + 
  ylab("Density") +
  scale_fill_manual(values = c(alpha("#112445", 0.75),
                               alpha("#B22222", 0.75))) +
  ggthemes::theme_clean() +
  theme(
    plot.background   = element_blank(),
    axis.text         = element_text(size = 10),
    axis.title        = element_text(size = 12),
    axis.line.y       = element_blank(), 
    legend.key.size   = unit(0.2, units = "cm"),
    legend.background = element_blank(),
    legend.position   = "none"
  )

ggsave("./Plots/Rank_1.png")

df %>% ggplot() + aes(x = SEI_individual) +
  geom_density(
    aes(fill = "Children"), 
    alpha = 0.75) +
  geom_density(
    aes(x    = SEI_parental,
        fill = "Parental"), 
    alpha = 0.5) +
  xlab("Rank") + 
  ylab("") +
  labs(fill = "") +
  scale_fill_manual(values = c("#112445", "#B22222")) +
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

ggsave("./Plots/Rank_2.png")
