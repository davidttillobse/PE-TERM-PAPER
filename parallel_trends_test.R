rm(list=ls()) 

library(dplyr)
library(ggplot2)
library(readxl)
library(grid)
library(gridExtra)

source("data_clean_aggregate_votes.R")

turnout_plot <- ggplot(elections_master, aes(x = as.Date(paste0(year, "-01-01")), y = turnout_rate, group = territory, color = territory, linetype = territory)) +
  geom_line() +
  labs(x = "Year",
       y = "Turnout Rate",
       color = "Territory",
       linetype = "Territory") +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  theme_bw() +
  ylim(0, 0.4)

blank_plot <- ggplot(elections_master, aes(x = as.Date(paste0(year, "-01-01")), y = blank_rate, group = territory, color = territory, linetype = territory)) +
  geom_line() +
  labs(x = "Year",
       y = "Blank Vote Rate",
       color = "Territory",
       linetype = "Territory") +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  theme_bw() +
  ylim(0, 0.02)

null_plot <- ggplot(elections_master, aes(x = as.Date(paste0(year, "-01-01")), y = null_rate, group = territory, color = territory, linetype = territory)) +
  geom_line() +
  labs(x = "Year",
       y = "Invalid Vote Rate",
       color = "Territory",
       linetype = "Territory") +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  theme_bw() +
  ylim(0, 0.4)

grid.arrange(turnout_plot, blank_plot, null_plot, ncol = 1)

#Save graph
trends <- grid.arrange(turnout_plot, blank_plot, null_plot, ncol = 1)
ggsave("parallel_trend.png", trends, dpi = 300)

