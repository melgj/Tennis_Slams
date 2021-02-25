library(tidyverse)
library(lubridate)

pts <- read_csv("slam_points_all.csv", col_names = T)
ms <- read_csv("slam_matches_all.csv", col_names = T)

newPts <- read_csv("2021-ausopen-points.csv", col_names = T)
newMs <- read_csv("2021-ausopen-matches.csv", col_names = T)

allP <- rbind(pts, newPts)
allM <- rbind(ms, newMs)

write_csv(allP, "slam_points_all.csv")
write_csv(allM, "slam_matches_all.csv")

sPts <- allP %>% 
  left_join(allM, by = "match_id")

write_csv(sPts, "slam_complete_pbp.csv")
