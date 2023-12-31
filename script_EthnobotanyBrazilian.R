# Article: bibliographic and biogeographical synthesis of medicinal plants in Brazil --------

# Miranda et al.

# Required packages:

library(dplyr)
library(openxlsx)
library(ggplot2)
library(ggsci)
library(janitor)

# selecting directory

setwd("D:/OneDrive/Artigos/EthnobotanyBrazilian")

# loading database

d1 = read.xlsx("database1.xlsx", sheet = "Extract") 

# Temporal analysis -------------------------------------------------------

# Creating the basis for graphic production

year <- 
  d1 %>% 
  clean_names() %>%
  group_by(year) %>% 
  summarise(contagem = n())

year %>% 
  ggplot() + 
  aes(year, contagem) +
  geom_line(color= "#737373", 
            size = 1
            ) +
  geom_hline(yintercept = mean(year$contagem), 
             linetype="dashed", 
             color="#6E8B3D"
             ) +
  geom_point(size = 5, 
             color="#800000"
             ) + 
  geom_smooth(method = "lm",
              se = FALSE, 
              color="#8B8878"
              ) +
  labs(x ="Years of publication",
       y = "Number of publication"
       ) +
  theme_bw(base_family = "serif",
           base_size = 15
           ) +
  theme(axis.text = element_text(family = "serif",
                                 size = 12,
                                 color = "black"
                                 ),
        panel.grid = element_blank()) +
  scale_x_continuous(breaks = seq(1992,2023,5)) +
  scale_y_continuous(breaks = seq(0,16,2)) +
  annotate("text", 
           x = 1992, 
           y = 15, 
           label = "R² = 0.40\n  p = 0.0001",
           family = "serif",
           size = 5
           ) + 
  annotate("text",
           x = 1992,
           y = 7.1,
           label = "Mean = 6.85",
           family = "serif",
           size = 5
           )

ggsave("temporal.png", plot = last_plot(), width = 14, height = 8, dpi = 300)


# Richness ----------------------------------------------------------------

# Variable select

rich <- d1 %>% 
  clean_names() %>% 
  select(richness, sample, amb,region)

# Testing the difference between locations

kruskal.test(richness ~ amb, data=rich)

# Checking the richness in different localities

mean_rich <- rich %>%
  group_by(amb) %>%
  summarise(media = mean(richness, na.rm = TRUE))

# Data Viz

rich %>% 
  ggplot() + 
  aes(amb, richness, color = amb)+
  geom_jitter(width = .2, 
              size = 2, 
              alpha = .8
              ) +
  geom_boxplot(alpha = 0,
               outlier.shape = NA,
               width = .3
               ) +
  geom_hline(yintercept = mean(rich$richness), 
             linetype="dashed", 
             color="#6E8B3D"
             ) +
  labs(x = "Type of location", 
       y = "Number of species"
       ) +
  theme_bw(base_size = 12,
           base_family = "serif"
           ) +
  scale_colour_uchicago() +
  theme(axis.text = element_text(family = "serif",
                                 color = "black",
                                 size = 12
                                 ),
        legend.position = "none",
        panel.grid = element_blank()) +
  scale_y_continuous(breaks = seq(0, 300, 50)) +
  annotate("text",
           x = .6,
           y = 93.6,
           label = "Mean = 89.41",
           size = 5,
           family = "serif"
           ) +
  annotate("text",
           x = .8,
           y = 290,
           label = "Kruskal-Wallis test:\n  p = 0.33",
           family = "serif",
           size = 6
           ) +
  annotate("text",
           x = 1.3,
           y = 170,
           label = "Mean (IPLC) = 77.54",
           family = "serif",
           size = 5
  ) +
  annotate("text",
           x = 2.5,
           y = 170,
           label = "Mean (Rural) = 95.45",
           family = "serif",
           size = 5
  ) +
  annotate("text",
           x = 3.3,
           y = 170,
           label = "Mean (Urban) = 86.64",
           family = "serif",
           size = 5
  )

ggsave("rich_loc.png", plot = last_plot(), width = 14, height = 8, dpi = 300)


# Richenss p/ Region

# Checking the richness in different regions

mean_region <- rich %>%
  group_by(region) %>%
  summarise(media = mean(richness, na.rm = TRUE))

rich %>% 
  ggplot() + 
  aes(reorder(region, -richness), richness, color = region) + 
  geom_jitter(width = .3, 
              size = 2.5, 
              alpha = .8) +
  geom_boxplot(alpha = 0, 
               width = .33,
               outlier.shape = NA) +
  labs(x = "Region", 
       y = "Number of species") +
  theme_bw(base_size = 15,
           base_family = "serif") + 
  scale_color_uchicago() + 
  theme(
    axis.text = element_text(
      family = "serif",
      size = 12,
      color = "black"
    ),
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  scale_y_continuous(
    breaks = seq(0, 300, 50)
  ) +
  annotate("text",
           x = .8,
           y = 290,
           label = "Kruskal-Wallis test \n  p = 0.15",
           family = "serif",
           size = 5) +
  annotate(
    "text",
    x = 1.2,
    y = 250,
    label = "Mean (North) = 104.15",
    family = "serif",
    size = 4
  ) +
  annotate(
    "text",
    x = 2.2,
    y = 250,
    label = "Mean (Midwest) = 99.61",
    family = "serif",
    size = 4
  ) +
  annotate(
    "text",
    x = 3.2,
    y = 250,
    label = "Mean (Southeast) = 96.05",
    family = "serif",
    size = 4
  ) + 
  annotate(
    "text",
    x = 4.2,
    y = 250,
    label = "Mean (South) = 92.40",
    family = "serif",
    size = 4
  ) +
  annotate(
    "text",
    x = 5.2,
    y = 250,
    label = "Mean (Northeast) = 79.73",
    family = "serif",
    size = 4
  )

ggsave("region.png", plot = last_plot(), width = 14, height = 8, dpi = 300)

# Richness x Sample

rich %>% 
  ggplot() +
  aes(sample, richness, color = amb) +
  geom_point(size = 2, alpha = .9) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              color="#8B8878") + 
  labs(x = "Sampling", y = "Number of species", color = "Type of location") + 
  scale_colour_uchicago() + 
  theme_bw(base_size = 15, 
           base_family = "serif") + 
  theme(axis.text = element_text(size = 12,
                                 color = "black",
                                 family = "serif"),
        panel.grid = element_blank(),
        legend.text = element_text(size = 12,
                                   color = "black",
                                   family = "serif")) +
  scale_y_continuous(breaks = seq(0,300,50)) + 
  scale_x_continuous(breaks = seq(0,400,50))

# Journals

df_j <- 
  d1 %>% 
  clean_names() %>% 
  group_by(journal) %>% 
  summarise(contagem = n()) %>% 
  top_n(10, contagem)

df_j %>% 
  ggplot() + 
  aes(contagem, reorder(journal, contagem), label = contagem) +
  geom_bar(stat = "identity", fill = "#800000") +
  geom_label(family = "serif",
             size = 5) +
  labs(x = "Number of publication", y = "Journal") + 
  theme_bw(base_size = 15,
           base_family = "serif") +
  theme(axis.text = element_text(family = "serif",
                                  color = "black",
                                  size = 12),
        panel.grid = element_blank()) +
  scale_x_continuous(breaks = seq(0,35,5))
