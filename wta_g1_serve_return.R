library(tidyverse)
library(lubridate)
library(stringr)

wtaPts <- read_csv("wta_slam_points_all.csv", col_names = T)

str(wtaPts)

t50 <- read_csv("wta_SR_ratings_jan21.csv", col_names = T) %>% 
  mutate(Rank = min_rank(desc(Rating))) %>% 
  filter(Rank <= 50) %>% 
  select(Rank, everything())

tail(t50)

g1 <- wtaPts %>% 
  filter(SetNo == 1, GameNo == 1, GameWinner != 0,
         (player1 %in% t50$Player | player2 %in% t50$Player)) %>% 
  select(match_id:PointServer, P1Score, P2Score, 
         player1, player2, player1id, player2id)

summary(g1)

#####################################

# SERVE

g1s <- g1 %>% 
  mutate(Server_Name = if_else(PointServer == 1, player1, player2),
         Winner_Name = if_else(GameWinner == 1, player1, player2),
         Held_Serve = if_else(GameWinner == PointServer, T, F))

g1s_summary <- g1s %>% 
  group_by(Server_Name) %>% 
  summarise(Service_Games_G1 = n(),
            Held_G1 = sum(Held_Serve == T),
            Broken_G1 = sum(Held_Serve == F)) %>% 
  mutate(Held_Pct_G1 = signif(Held_G1/(Held_G1+Broken_G1), 2)) %>% 
  filter(Service_Games_G1 >= 10) %>% 
  arrange(desc(Held_Pct_G1))

g1s_summary
            
###########################################

allGms <- wtaPts %>% 
  filter(GameWinner != 0, (player1 %in% t50$Player | player2 %in% t50$Player)) %>% 
  select(match_id:PointServer, P1Score, P2Score, 
         player1, player2, player1id, player2id) 

head(allGms)

allGmsS <- allGms %>% 
  mutate(Server_Name = if_else(PointServer == 1, player1, player2),
         Winner_Name = if_else(GameWinner == 1, player1, player2),
         Held_Serve = if_else(GameWinner == PointServer, T, F))

allGmsS_summary <- allGmsS %>% 
  group_by(Server_Name) %>% 
  summarise(Service_Games_All = n(),
            Held_All = sum(Held_Serve == T),
            Broken_All = sum(Held_Serve == F)) %>% 
  mutate(Held_Pct_All = signif(Held_All/(Held_All + Broken_All),2)) %>% 
  filter(Service_Games_All >= 50) %>% 
  arrange(desc(Held_Pct_All))

allGmsS_summary

comb_server <- allGmsS_summary %>% 
  left_join(g1s_summary, by = "Server_Name") %>% 
  mutate(Abs_Diff_Serve_G1vAll = abs(Held_Pct_G1 - Held_Pct_All),
         Net_Diff_Serve_G1vAll = Held_Pct_G1 - Held_Pct_All) %>% 
  select(Server_Name, Service_Games_All, Held_Pct_All, Service_Games_G1, 
         Held_Pct_G1, Abs_Diff_Serve_G1vAll, Net_Diff_Serve_G1vAll) %>% 
  arrange(desc(Net_Diff_Serve_G1vAll)) %>% 
  drop_na()

comb_server  

tail(comb_server)

##############################################

# RETURN

g1r <- g1 %>% 
  mutate(Receiver_Name = if_else(PointServer == 1, player2, player1),
         Winner_Name = if_else(GameWinner == 1, player1, player2),
         Broke_Serve = if_else(GameWinner != PointServer, T, F))

g1r_summary <- g1r %>% 
  group_by(Receiver_Name) %>% 
  summarise(Receiver_Games_G1 = n(),
            Broke_G1 = sum(Broke_Serve == T),
            Lost_G1 = sum(Broke_Serve == F)) %>% 
  mutate(Broke_Pct_G1 = signif(Broke_G1/(Broke_G1 + Lost_G1), 2)) %>% 
  filter(Receiver_Games_G1 >= 10) %>% 
  arrange(desc(Broke_Pct_G1))

g1r_summary

###########################################

allGmsR <- allGms %>% 
  mutate(Receiver_Name = if_else(PointServer == 1, player2, player1),
         Winner_Name = if_else(GameWinner == 1, player1, player2),
         Broke_Serve = if_else(GameWinner != PointServer, T, F))

allGmsR_summary <- allGmsR %>% 
  group_by(Receiver_Name) %>% 
  summarise(Receiver_Games_All = n(),
            Broke_All = sum(Broke_Serve == T),
            Lost_All = sum(Broke_Serve == F)) %>% 
  mutate(Broke_Pct_All = signif(Broke_All/(Broke_All + Lost_All), 2)) %>% 
  filter(Receiver_Games_All >= 50) %>% 
  arrange(desc(Broke_Pct_All))

allGmsR_summary

comb_receiver <- allGmsR_summary %>% 
  left_join(g1r_summary, by = "Receiver_Name") %>% 
  mutate(Abs_Diff_Rec_G1vAll = abs(Broke_Pct_G1 - Broke_Pct_All),
         Net_Diff_Rec_G1vAll = Broke_Pct_G1 - Broke_Pct_All) %>% 
  select(Receiver_Name, Receiver_Games_All, Broke_Pct_All, Receiver_Games_G1, 
         Broke_Pct_G1, Abs_Diff_Rec_G1vAll, Net_Diff_Rec_G1vAll) %>% 
  arrange(desc(Net_Diff_Rec_G1vAll)) %>% 
  drop_na()

comb_receiver  

tail(comb_receiver)


#######################################

comb_all <- comb_server %>% 
  full_join(comb_receiver, by = c("Server_Name" = "Receiver_Name")) %>% 
  rename(Player = Server_Name)

colSums(is.na(comb_all))
x <- which(is.na(comb_all$Receiver_Games_G1))

comb_all[x,]

comb_all_summary <- comb_all %>% 
  mutate(Game1_Win_Pct_Diff = Held_Pct_G1 - Broke_Pct_G1,
         Matches = Service_Games_G1 + Receiver_Games_G1,
         Served_1st_Pct = signif(Service_Games_G1 / Matches, 2)) %>% 
  filter(Matches >= 25) %>% 
  select(Player, Matches, Service_Games_G1, Receiver_Games_G1, Served_1st_Pct,
         Game1_Win_Pct_Diff, Held_Pct_G1, Broke_Pct_G1) %>% 
  arrange(desc(Game1_Win_Pct_Diff)) %>% 
  drop_na()

comb_all_summary

tail(comb_all_summary)

mean(comb_all_summary$Held_Pct_G1)
mean(comb_all_summary$Broke_Pct_G1)
