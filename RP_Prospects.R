library(tidyverse)
library(glmnet)
library(ggimage)
library(ggrepel)
library(readxl)

# Load data
RP <- read_excel("/Users/adamwilliams/Desktop/Football Data/RP_prospects.xlsx")

#Adding logos for each team from GitHub repository
RP <- RP %>%
  mutate(logo_url = case_when(
    team == "ARI" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-arizona-cardinals-team-logo-2-768x768.png",
    team == "ATL" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-atlanta-falcons-team-logo-2-768x768.png",
    team == "BAL" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-baltimore-ravens-team-logo-2-768x768.png",
    team == "BUF" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-buffalo-bills-team-logo-2-768x768.png",
    team == "CAR" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-carolina-panthers-team-logo-2-768x768.png",
    team == "CHI" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-chicago-bears-team-logo-2-768x768.png",
    team == "CIN" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-cincinnati-bengals-team-logo-768x768.png",
    team == "CLE" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-cleveland-browns-team-logo-2-768x768.png",
    team == "DAL" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-dallas-cowboys-team-logo-2-768x768.png",
    team == "DEN" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-denver-broncos-team-logo-2-768x768.png",
    team == "DET" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-detroit-lions-team-logo-2-768x768.png",
    team == "GB"  ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-green-bay-packers-team-logo-2-768x768.png",
    team == "HOU" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-houston-texans-team-logo-2-768x768.png",
    team == "IND" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-indianapolis-colts-team-logo-2-768x768.png",
    team == "JAC" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-jacksonville-jaguars-team-logo-2-768x768.png",
    team == "KC"  ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-kansas-city-chiefs-team-logo-2-768x768.png",
    team == "LAC" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-los-angeles-chargers-team-logo-2-768x768.png",
    team == "LAR"  ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/los-angeles-rams-2020-logo-300x300.png",
    team == "LV"  ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-oakland-raiders-team-logo-768x768.png",
    team == "MIA" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/Miami-Dolphins-Logo-768x768.png",
    team == "MIN" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-minnesota-vikings-team-logo-2-768x768.png",
    team == "NE"  ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-new-england-patriots-team-logo-2-768x768.png",
    team == "NO"  ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-new-orleans-saints-team-logo-2-768x768.png",
    team == "NYG" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-new-york-giants-team-logo-2-768x768.png",
    team == "NYJ" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/New-York-Jets-logo-2024-768x768.png",
    team == "PHI" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-philadelphia-eagles-team-logo-2-768x768.png",
    team == "PIT" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-pittsburgh-steelers-team-logo-2-768x768.png",
    team == "SEA" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-seattle-seahawks-team-logo-2-768x768.png",
    team == "SF"  ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-san-francisco-49ers-team-logo-2-768x768.png",
    team == "TB"  ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/tampa-bay-buccaneers-2020-logo-768x768.png",
    team == "TEN" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/nfl-tennessee-titans-team-logo-2-768x768.png",
    team == "WAS" ~ "https://raw.githubusercontent.com/adcw156/Fantasy-RB-Exploration/main/washington-commanders-logo-768x768.png",
    TRUE ~ NA_character_
  ))

# Plotting 
p <- ggplot(RP, aes(y = Comeback_succ, x = Comeback_succpros)) +
  geom_image(aes(image = logo_url), size = 0.022, asp = 1.5, by = "width") +
  geom_text_repel(
    aes(label = Player),
    size = 4.2,
    max.overlaps = Inf,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.color = "gray30",
    segment.size = 0.3,
    min.segment.length = 0,
    force = 2
  ) +
  labs(
    title = "Comeback Route Success Rate as a Propect and in NFL",
    y = "Comeback Success (NFL)",
    x = "Comeback Success (NCAA)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # centered, bold, larger
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) 

# Export to Desktop
ggsave("/Users/adamwilliams/Desktop/Comeback.png", plot = p, width = 24, height = 14, dpi = 300)










