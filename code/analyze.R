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

# Figure 1
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

scaleFUN <- function(x) sprintf("%.0f", x)

figure1a <- ggplot(figure1data, aes(x = X2, y = X1, group = X3)) +
  geom_bar(stat="identity",
           aes(fill = X3),
           position = position_dodge(width = 60),
           width = 50)+
  labs(x = "Quarter", y = "Number of providers in Australia") +
  theme(plot.title.position = "plot",
        text = element_text(colour = "black", size = 14, family = "serif"),
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
  scale_fill_manual(values=c("darkorchid4","azure4"))+
  scale_y_continuous(breaks = seq(min(0), max(20000), by = 2000), labels = scaleFUN) +
  scale_x_date(breaks = seq(as.Date("2019-09-30"), as.Date("2023-03-31"), by = "3 months"), date_labels = "%b\n%Y") +
  coord_cartesian(ylim = c(0, 20000))

# Set output directory
outputDir <- root$find_file("output")

ggsave(filename = "Figure1A.pdf",
       plot = figure1a,
       device = "pdf",
       path = outputDir,
       width = 30,
       height = 13,
       units = "cm",
       dpi = 300)

figure1stats <- figure1data %>% 
  group_by(X2) %>%
  summarise(X1 = (X1[X3=="Active"]/X1[X3=="Total"])*100) %>%
  as.data.frame()

figure1b <- ggplot(figure1stats, aes(x = X2, y = X1)) +
  geom_line()+
  geom_point()+
  labs(x = "Quarter", y = "Active provider rate in Australia (%)") +
  theme(plot.title.position = "plot",
        text = element_text(colour = "black", size = 14, family = "serif"),
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
  scale_y_continuous(breaks = seq(min(0), max(100), by = 20), labels = scaleFUN) +
  scale_x_date(breaks = seq(as.Date("2019-09-30"), as.Date("2023-03-31"), by = "3 months"), date_labels = "%b\n%Y") +
  coord_cartesian(ylim = c(0, 100))

ggsave(filename = "Figure1B.pdf",
       plot = figure1b,
       device = "pdf",
       path = outputDir,
       width = 30,
       height = 13,
       units = "cm",
       dpi = 300)

# Figure 2
df <- data %>%
  subset(State=="WA"&
           ServiceDistrict=="ALL"&
           DisabilityGroup=="ALL"&
           AgeBand=="ALL"&
           SupportClass=="ALL")

figure2data <- df %>%
  mutate(ReportingDate = as.Date(ReportingDate),
         ActiveIn = ifelse(ActiveIn=="Ever", "Total",
                           ifelse(ActiveIn=="Quarter", "Active", NA))) %>%
  rename(X1 = ProviderCount,
         X2 = ReportingDate,
         X3 = ActiveIn) %>%
  select(X1, X2, X3)

scaleFUN <- function(x) sprintf("%.0f", x)

figure2a <- ggplot(figure2data, aes(x = X2, y = X1, group = X3)) +
  geom_bar(stat="identity",
           aes(fill = X3),
           position = position_dodge(width = 60),
           width = 50)+
  labs(x = "Quarter", y = "Number of providers in Western Australia") +
  theme(plot.title.position = "plot",
        text = element_text(colour = "black", size = 14, family = "serif"),
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
  scale_fill_manual(values=c("darkorchid4","azure4"))+
  scale_y_continuous(breaks = seq(min(0), max(3000), by = 500), labels = scaleFUN) +
  scale_x_date(breaks = seq(as.Date("2019-09-30"), as.Date("2023-03-31"), by = "3 months"), date_labels = "%b\n%Y") +
  coord_cartesian(ylim = c(0, 3000))

ggsave(filename = "Figure2A.pdf",
       plot = figure2a,
       device = "pdf",
       path = outputDir,
       width = 30,
       height = 13,
       units = "cm",
       dpi = 300)

figure2stats <- figure2data %>% 
  group_by(X2) %>%
  summarise(X1 = (X1[X3=="Active"]/X1[X3=="Total"])*100) %>%
  as.data.frame()

figure2b <- ggplot(figure2stats, aes(x = X2, y = X1)) +
  geom_line()+
  geom_point()+
  labs(x = "Quarter", y = "Active provider rate in Western Australia (%)") +
  theme(plot.title.position = "plot",
        text = element_text(colour = "black", size = 14, family = "serif"),
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
  scale_y_continuous(breaks = seq(min(0), max(100), by = 20), labels = scaleFUN) +
  scale_x_date(breaks = seq(as.Date("2019-09-30"), as.Date("2023-03-31"), by = "3 months"), date_labels = "%b\n%Y") +
  coord_cartesian(ylim = c(0, 100))

ggsave(filename = "Figure2B.pdf",
       plot = figure2b,
       device = "pdf",
       path = outputDir,
       width = 30,
       height = 13,
       units = "cm",
       dpi = 300)

# Table 1
df <- data %>%
  subset(State=="WA"&
           DisabilityGroup=="ALL"&
           AgeBand=="ALL"&
           SupportClass=="ALL")
         
table1data <- df %>%
  arrange(ReportingDate) %>%
  group_by(ServiceDistrict, ActiveIn) %>%
  mutate(lag_ProviderCount = dplyr::lag(ProviderCount, n = 1L, default = NULL),
         Change = ((ProviderCount/lag_ProviderCount)-1)*100,
         Status = ifelse(ActiveIn=="Ever", "Total",
                    ifelse(ActiveIn=="Quarter", "Active", NA))) %>%
  rename(N = ProviderCount) %>%
  as.data.frame()

table1 <- table1data %>%
  group_by(ServiceDistrict, ReportingDate) %>%
  summarise(N = (N[Status=="Active"]/N[Status=="Total"])*100) %>%
  mutate(Status = "Rate") %>%
  as.data.frame() %>%
  bind_rows(table1data, .) %>%
  select(ServiceDistrict, Status, ReportingDate, N, Change) %>%
  pivot_longer(c(-ServiceDistrict, -Status, -ReportingDate),
               names_to = "Variable",
               values_to = "Value") %>%
  pivot_wider(names_from = c(ReportingDate, Variable),
              values_from = Value,
              names_vary = "slowest") %>%
  arrange(ServiceDistrict, Status)

write.csv(table1, paste(outputDir, "Table1.csv", sep = "/"), na = "", row.names = FALSE)



levels(as.factor(data$State))

