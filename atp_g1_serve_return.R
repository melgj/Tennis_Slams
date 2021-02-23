library(tidyverse)
library(lubridate)
library(stringr)

atpPts <- read_csv("atp_slam_points_all.csv", col_names = T)

glimpse(atpPts)

atpPts$Surface <- case_when(atpPts$slam == "ausopen" | atpPts$slam == "usopen" ~ "Hard",
                            atpPts$slam == "frenchopen" ~ "Clay",
                            atpPts$slam == "wimbledon" ~ "Grass")

t50 <- read_csv("atp_SR_ratings_jan21.csv", col_names = T) %>% 
  mutate(Rank = min_rank(desc(Rating))) %>% 
  filter(Rank <= 50) %>% 
  select(Rank, everything())

tail(t50)

g1 <- atpPts %>% 
  filter(SetNo == 1, GameNo == 1, GameWinner != 0,
         (player1 %in% t50$Player | player2 %in% t50$Player)) %>% 
  select(match_id:PointServer, P1Score, P2Score, 
         player1, player2, player1id, player2id, Surface)

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
  #filter(Service_Games_G1 >= 10) %>% 
  arrange(desc(Held_Pct_G1))

g1s_summary %>% 
  filter(Service_Games_G1 >= 10) %>% 
  arrange(desc(Held_Pct_G1))
  

###########################################

allGms <- atpPts %>% 
  filter(GameWinner != 0, (player1 %in% t50$Player | player2 %in% t50$Player)) %>% 
  select(match_id:PointServer, P1Score, P2Score, 
         player1, player2, player1id, player2id, Surface) 

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
  #filter(Service_Games_All >= 50) %>% 
  arrange(desc(Held_Pct_All))

allGmsS_summary %>% 
  filter(Service_Games_All >= 100) %>% 
  arrange(desc(Held_Pct_All))

comb_server <- allGmsS_summary %>% 
  left_join(g1s_summary, by = "Server_Name") %>% 
  mutate(Abs_Diff_Serve_G1vAll = abs(Held_Pct_G1 - Held_Pct_All),
         Net_Diff_Serve_G1vAll = Held_Pct_G1 - Held_Pct_All) %>% 
  select(Server_Name, Service_Games_All, Held_Pct_All, Service_Games_G1, 
         Held_Pct_G1, Abs_Diff_Serve_G1vAll, Net_Diff_Serve_G1vAll) %>% 
  arrange(desc(Net_Diff_Serve_G1vAll))

comb_server %>% 
  filter(Service_Games_All >= 100) %>%
  drop_na() %>% 
  arrange(desc(Net_Diff_Serve_G1vAll))

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
  #filter(Receiver_Games_G1 >= 10) %>% 
  arrange(desc(Broke_Pct_G1))

g1r_summary %>% 
  filter(Receiver_Games_G1 >= 10) %>% 
  arrange(desc(Broke_Pct_G1))

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
  #filter(Receiver_Games_All >= 50) %>% 
  arrange(desc(Broke_Pct_All))

allGmsR_summary %>% 
  filter(Receiver_Games_All >= 100) %>% 
  arrange(desc(Broke_Pct_All))

allGmsR_summary

comb_receiver <- allGmsR_summary %>% 
  left_join(g1r_summary, by = "Receiver_Name") %>% 
  mutate(Abs_Diff_Rec_G1vAll = abs(Broke_Pct_G1 - Broke_Pct_All),
         Net_Diff_Rec_G1vAll = Broke_Pct_G1 - Broke_Pct_All) %>% 
  select(Receiver_Name, Receiver_Games_All, Broke_Pct_All, Receiver_Games_G1, 
         Broke_Pct_G1, Abs_Diff_Rec_G1vAll, Net_Diff_Rec_G1vAll) %>% 
  arrange(desc(Net_Diff_Rec_G1vAll)) 

comb_receiver %>% 
  filter(Receiver_Games_All >= 50) %>%
  drop_na() %>% 
  arrange(desc(Net_Diff_Rec_G1vAll))

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
  arrange(desc(Game1_Win_Pct_Diff)) 

comb_all_summary %>% 
  drop_na() %>% 
  arrange(desc(Game1_Win_Pct_Diff)) 

#view(comb_all_summary)

tail(comb_all_summary)

mean(comb_all_summary$Held_Pct_G1)
mean(comb_all_summary$Broke_Pct_G1)

# Summary Stats and Plots

# By Surface
allGmsS %>% 
  group_by(Server_Name, Surface) %>% 
  summarise(Service_Games_All = n(),
            Held_All = sum(Held_Serve == T),
            Broken_All = sum(Held_Serve == F)) %>% 
  mutate(Held_Pct_All = signif(Held_All/(Held_All + Broken_All),2)) %>% 
  filter(Service_Games_All >= 50, Held_Pct_All >= 0.85) %>% 
  arrange(Surface, desc(Held_Pct_All)) %>% 
  ggplot(aes(reorder(Server_Name, Held_Pct_All), Held_Pct_All, fill = Service_Games_All)) +
  geom_col() +
  geom_text(aes(label = Held_Pct_All), nudge_y = -0.2, colour = "orange") +
  labs(title = "Best Service Hold Percentage by Surface in Grand Slams (Men)", subtitle = "minimum 50 service games") +
  ylab("Hold Percentage") +
  xlab("Player") +
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip() +
  facet_wrap(~ Surface)

allGmsR %>% 
  group_by(Receiver_Name, Surface) %>% 
  summarise(Receiver_Games_All = n(),
            Broke_All = sum(Broke_Serve == T),
            Lost_All = sum(Broke_Serve == F)) %>% 
  mutate(Broke_Pct_All = signif(Broke_All/(Broke_All + Lost_All), 2)) %>% 
  filter(Receiver_Games_All >= 50, Broke_Pct_All >= 0.25) %>% 
  arrange(Surface, desc(Broke_Pct_All)) %>% 
  ggplot(aes(reorder(Receiver_Name, Broke_Pct_All), Broke_Pct_All, fill = Receiver_Games_All)) +
  geom_col() +
  geom_text(aes(label = Broke_Pct_All), nudge_y = -0.1, colour = "orange") +
  labs(title = "Best Service Break Percentage by Surface in Grand Slams (Men)", subtitle = "minimum 50 return games") +
  ylab("Break Percentage") +
  xlab("Player") +
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip() +
  facet_wrap(~ Surface)

# All Surfaces

allGmsS %>% 
  group_by(Server_Name) %>% 
  summarise(Service_Games_All = n(),
            Held_All = sum(Held_Serve == T),
            Broken_All = sum(Held_Serve == F)) %>% 
  mutate(Held_Pct_All = signif(Held_All/(Held_All + Broken_All),2)) %>% 
  filter(Service_Games_All >= 50, Held_Pct_All >= 0.85) %>% 
  arrange(desc(Held_Pct_All)) %>% 
  ggplot(aes(reorder(Server_Name, Held_Pct_All), Held_Pct_All, fill = Service_Games_All)) +
  geom_col() +
  geom_text(aes(label = Held_Pct_All), nudge_y = -0.2, colour = "orange") +
  labs(title = "Best Service Hold Percentage in Grand Slams (Men)", subtitle = "minimum 50 service games") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Hold Percentage") +
  xlab("Player") +
  coord_flip()
   

allGmsR %>% 
  group_by(Receiver_Name) %>% 
  summarise(Receiver_Games_All = n(),
            Broke_All = sum(Broke_Serve == T),
            Lost_All = sum(Broke_Serve == F)) %>% 
  mutate(Broke_Pct_All = signif(Broke_All/(Broke_All + Lost_All), 2)) %>% 
  filter(Receiver_Games_All >= 50, Broke_Pct_All >= 0.25) %>% 
  arrange(desc(Broke_Pct_All))%>% 
  ggplot(aes(reorder(Receiver_Name, Broke_Pct_All), Broke_Pct_All, fill = Receiver_Games_All)) +
  geom_col() +
  geom_text(aes(label = Broke_Pct_All), nudge_y = -0.1, colour = "orange") +
  labs(title = "Best Service Break Percentage in Grand Slams (Men)", subtitle = "minimum 50 return games") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Break Percentage") +
  xlab("Player") +
  coord_flip()

