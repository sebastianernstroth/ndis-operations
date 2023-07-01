library(rprojroot)
library(qs)
library(tidyverse)

# Establish project directory
root <- rprojroot::is_rstudio_project

# Set data directory
dataDir <- root$find_file("data")

# Grab data files
qsFiles <- list.files(dataDir, pattern = "*.qs", recursive = TRUE)

# Read data
data <- qread(paste(dataDir, qsFiles[1], sep = "/"))

# Australia
df <- data %>%
  subset(State=="ALL"&
           ServiceDistrict=="ALL"&
           DisabilityGroup=="ALL"&
           AgeBand=="ALL"&
           SupportClass=="ALL")

figure1data <- df %>%
  mutate(ReportingDate = as.Date(ReportingDate),
         ActiveIn = ifelse(ActiveIn=="Ever", "Total",
                           ifelse(ActiveIn=="Quarter", "Active", NA))) %>%
  rename(X1 = ProviderCount,
         X2 = ReportingDate,
         X3 = ActiveIn) %>%
  select(X1, X2, X3)

scaleFUN = function(x) sprintf("%.0f", x)

ggplot(figure1data, aes(x = X2, y = X1, colour = X3)) +
  geom_line() +
  labs(x="Quarter", y="Number of providers in Australia") +
  theme(plot.title.position = "plot",
        text = element_text(colour = "black", size = 18, family = "serif"),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour =" black"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        legend.box.background = element_rect(),
        legend.box.margin = margin(1, 1, 1, 1),
        legend.key = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.spacing.y = unit(0.0, "cm"),
        legend.spacing.x = unit(0.1, 'cm')) +
  scale_y_continuous(breaks = seq(min(0), max(20000), by = 2000), labels = scaleFUN) +
  scale_x_date(breaks = seq(as.Date("2019-09-30"), as.Date("2023-03-31"), by = "3 months"), date_labels = "%b\n%Y") +
  coord_cartesian(ylim = c(0, 20000))

# Set output directory
outputDir <- root$find_file("output")

ggsave(filename = "Figure1.pdf",
       plot = figure1,
       device = "pdf",
       path = outputDir,
       width = 30,
       height = 13,
       units = "cm",
       dpi = 300)


# Western Australia
df <- data %>%
  subset(State=="WA"&
           ServiceDistrict=="ALL"&
           DisabilityGroup=="ALL"&
           AgeBand=="ALL"&
           SupportClass=="ALL")

figure1data <- df %>%
  mutate(ReportingDate = as.Date(ReportingDate),
         ActiveIn = ifelse(ActiveIn=="Ever", "Total",
                           ifelse(ActiveIn=="Quarter", "Active", NA))) %>%
  rename(X1 = ProviderCount,
         X2 = ReportingDate,
         X3 = ActiveIn) %>%
  select(X1, X2, X3)

scaleFUN = function(x) sprintf("%.0f", x)

ggplot(figure1data, aes(x = X2, y = X1, colour = X3)) +
  geom_line() +
  labs(x="Quarter", y="Number of providers in Western Australia") +
  theme(plot.title.position = "plot",
        text = element_text(colour = "black", size = 18, family = "serif"),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour =" black"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        legend.box.background = element_rect(),
        legend.box.margin = margin(1, 1, 1, 1),
        legend.key = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.spacing.y = unit(0.0, "cm"),
        legend.spacing.x = unit(0.1, 'cm')) +
  scale_y_continuous(breaks = seq(min(0), max(3000), by = 500), labels = scaleFUN) +
  scale_x_date(breaks = seq(as.Date("2019-09-30"), as.Date("2023-03-31"), by = "3 months"), date_labels = "%b\n%Y") +
  coord_cartesian(ylim = c(0, 3000))

# Set output directory
outputDir <- root$find_file("output")

ggsave(filename = "Figure1.pdf",
       plot = figure1,
       device = "pdf",
       path = outputDir,
       width = 30,
       height = 13,
       units = "cm",
       dpi = 300)



levels(as.factor(data$State))

