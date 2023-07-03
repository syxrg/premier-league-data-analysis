library(tidyverse)
library(lubridate)

url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv"
soccer <- read_csv(url)

soccer <- soccer %>%
  mutate(Year = year(dmy(Date))) %>%
  select(Year, HomeTeam, AwayTeam, HTHG, HTAG, FTHG, FTAG, -Date, -FTR, -HTR, -Referee, -HS, -AS, -HST, -AST, -HF, -AF, -HC, -AC, -HY, -AY, -HR, -AR) %>%
  rename(
    "Half Time Home Goal" = HTHG,
    "Half Time Away Goal" = HTAG,
    "Full Time Home Goal" = FTHG,
    "Full Time Away Goal" = FTAG
  )

write.csv(x = soccer, file = "data/soccer.csv")

