library(tidyverse)
library(lubridate)

allPoints <- list.files(pattern="*points.csv") %>%
  map_df(~read_csv(., col_types = cols(.default = "c")))

colnames(allPoints)

allMatches <- list.files(pattern="*matches.csv") %>%
  map_df(~read_csv(., col_types = cols(.default = "c")))

colnames(allMatches)

w <- read_csv("wta_SR_ratings_jan21.csv", col_names = T)
m <- read_csv("atp_SR_ratings_jan21.csv", col_names = T)

w50 <- w %>% 
  mutate(Rank = min_rank(desc(Rating))) %>% 
  select(Rank, everything()) %>% 
  filter(Rank <= 50)

w50

m50 <- m %>% 
  mutate(Rank = min_rank(desc(Rating))) %>% 
  select(Rank, everything()) %>% 
  filter(Rank <= 50)

m50

  
# Write data to csv file

write_csv(allPoints, "slam_points_all.csv")
write_csv(allMatches, "slam_matches_all.csv")

###########

# pts <- read_csv("slams_points_hard.csv", col_names = T)
# mts <- read_csv("slam_matches_hard.csv", col_names = T)

sp <- allPoints %>% 
  left_join(allMatches, by = "match_id")

head(sp)

wta <- sp %>% 
  filter(player1 %in% w50$Player | player2 %in% w50$Player)

atp <- sp %>% 
  filter(player1 %in% m50$Player | player2 %in% m50$Player)

write_csv(wta, "wta_slam_points_all.csv")
write_csv(atp, "atp_slam_points_all.csv")

view(head(wta))

colnames(wta)

