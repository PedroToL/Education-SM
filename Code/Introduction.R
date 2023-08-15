# Libraries ----
library(tidyverse)

# Data ----
educ <- read_csv("./Data/Expected years of schooling/Expected years of schooling.csv")
gdp  <- read_csv("./Data/GDP_PC.csv")

# Eduaction ----
ggplot(na.omit(educ)) + aes(x = Year, y = `Mexico, Total`) +
  geom_point(color = "#112446", 
             alpha = .75) + 
  geom_line(color = "#112446",
            alpha = .75) +
  xlab("Year") +
  ylab("Expected Years") +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    axis.text       = element_text(size = 10),
    axis.title      = element_text(size = 12),
    axis.line.y     = element_blank() 
  )

ggsave("./Plots/education.png")

# GDP ----
ggplot(na.omit(gdp)) + aes(x = Year, y = GDP_PC) +
  geom_point(color = "#112446", 
             alpha = .75) + 
  geom_line(color = "#112446",
            alpha = .75) +
  xlab("Year") +
  ylab("Constant 2015 US$") +
  scale_y_continuous(labels = c("4,000", "6,000", "8,000", "10,000")) +
  ggthemes::theme_clean() +
  theme(
    plot.background = element_blank(),
    axis.text       = element_text(size = 10),
    axis.title      = element_text(size = 12),
    axis.line.y     = element_blank() 
  )

ggsave("./Plots/gdp.png")
