## general idea ##

## create "good" brackets using vegas win chance & something else

## create "bad" brackets using espn pick chance

## slider to blend how "bad" the brackets are

## run sim, collect points, create rank ##

library(tidyverse)
library(data.table)
library(furrr)
options(future.fork.enable = T)

### Number of sims to run
n_creation_sims <- 1
# set.seed(13579)

### Function to sim games 
sim_game <- function(team, opponent, win_prob) {
  ifelse(runif(1) <= win_prob, team, opponent)
}

### Function to Build a Bracket For a Round
build_bracket <- function(seed_list) {
  n <- nrow(seed_list)
  ix <- seq(1, n, 2)
  tibble('team' = seed_list$team[ix],
         'opponent' = seed_list$team[-ix]) %>% 
    inner_join(wp_matrix, by = c('team', 'opponent'))
  
}

### Function to Simulate Games in Round
sim_round <- function(bracket) {
  pmap_chr(list('team' = bracket$team,
                'opponent' = bracket$opponent,
                'win_prob' = bracket$win_prob),
           sim_game)
}



### Read in Seed list and team ratings
seed_list <- read_csv("~/Downloads/bracketmaniav2 - seed_list.csv")

power_rankings <- read_csv("~/Downloads/bracketmaniav2 - KP.csv")
perc_chosen <- read_csv("~/Downloads/bracketmaniav2 - pick_perc_std.csv")

# power_rankings <- read_csv("~/Downloads/power_rankings.csv")
power_rankings = power_rankings %>% rename("team" = "TeamName")


seed_list <- 
  seed_list %>% 
  inner_join(select(perc_chosen, team, r1 = prob_r1, r2 = prob_r2, r3 = prob_r3, r4 = prob_r4, r5 = prob_r5, r6 = prob_r6), by = 'team')


### Compute a win-probability matrix for each possible combination of teams
wp_matrix <- 
  crossing('team' = seed_list$team, 
           'opponent' = seed_list$team) %>% 
  inner_join(select(seed_list, team, r1, r2, r3, r4, r5, r6), 
             by = 'team') %>% 
  inner_join(select(seed_list, team, r1, r2, r3, r4, r5, r6), 
             by = c('opponent' = 'team'), 
             suffix = c('_team', '_opponent')) %>%
  inner_join(select(seed_list, region, seed, team),
             by = 'team') %>%
  inner_join(select(seed_list, region, seed, team),
             by = c('opponent'='team'),
             suffix = c('_team', '_opponent'))%>%
  mutate(when_can_play = case_when(
    region_team == "East" & region_opponent == "West" ~ "r5",
    region_team == "West" & region_opponent == "East" ~ "r5",
    region_team == "South" & region_opponent == "Midwest" ~ "r5",
    region_team == "Midwest" & region_opponent == "South" ~ "r5",
    (region_team == "East" | region_team == "West" )& (region_opponent == "South" | region_opponent == "Midwest") ~ "r6",
    (region_team == "South" | region_team == "Midwest") & (region_opponent == "East" | region_opponent == "West") ~ "r6",
    
    (region_team == region_opponent) & (seed_team == 1 & seed_opponent == 16) ~ "r1",
    (region_team == region_opponent) & (seed_team == 2 & seed_opponent == 15) ~ "r1",
    (region_team == region_opponent) & (seed_team == 3 & seed_opponent == 14) ~ "r1",
    (region_team == region_opponent) & (seed_team == 4 & seed_opponent == 13) ~ "r1",
    (region_team == region_opponent) & (seed_team == 5 & seed_opponent == 12) ~ "r1",
    (region_team == region_opponent) & (seed_team == 6 & seed_opponent == 11) ~ "r1",
    (region_team == region_opponent) & (seed_team == 7 & seed_opponent == 10) ~ "r1",
    (region_team == region_opponent) & (seed_team == 8 & seed_opponent == 9) ~ "r1",
    
    (region_team == region_opponent) & (seed_opponent == 1 & seed_team == 16) ~ "r1",
    (region_team == region_opponent) & (seed_opponent == 2 & seed_team == 15) ~ "r1",
    (region_team == region_opponent) & (seed_opponent == 3 & seed_team == 14) ~ "r1",
    (region_team == region_opponent) & (seed_opponent == 4 & seed_team == 13) ~ "r1",
    (region_team == region_opponent) & (seed_opponent == 5 & seed_team == 12) ~ "r1",
    (region_team == region_opponent) & (seed_opponent == 6 & seed_team == 11) ~ "r1",
    (region_team == region_opponent) & (seed_opponent == 7 & seed_team == 10) ~ "r1",
    (region_team == region_opponent) & (seed_opponent == 8 & seed_team == 9) ~ "r1",
    
    
    (region_team == region_opponent) & ((seed_team == 1 | seed_team == 16) & (seed_opponent == 9 | seed_opponent == 8)) ~ "r2",
    (region_team == region_opponent) & ((seed_team == 5 | seed_team == 12) & (seed_opponent == 4 | seed_opponent == 13)) ~ "r2",
    (region_team == region_opponent) & ((seed_team == 6 | seed_team == 11) & (seed_opponent == 3 | seed_opponent == 14)) ~ "r2",
    (region_team == region_opponent) & ((seed_team == 7 | seed_team == 10) & (seed_opponent == 2 | seed_opponent == 15)) ~ "r2",
    
    (region_team == region_opponent) & ((seed_opponent == 1 | seed_opponent == 16) & (seed_team == 9 | seed_team == 8)) ~ "r2",
    (region_team == region_opponent) & ((seed_opponent == 5 | seed_opponent == 12) & (seed_team == 4 | seed_team == 13)) ~ "r2",
    (region_team == region_opponent) & ((seed_opponent == 6 | seed_opponent == 11) & (seed_team == 3 | seed_team == 14)) ~ "r2",
    (region_team == region_opponent) & ((seed_opponent == 7 | seed_opponent == 10) & (seed_team == 2 | seed_team == 15)) ~ "r2",
    
    
    (region_team == region_opponent) & ((seed_team == 1 | seed_team == 16 | seed_team == 8 | seed_team == 9) & (seed_opponent == 5 | seed_opponent == 12 | seed_opponent == 4 | seed_opponent == 13)) ~"r3",
    (region_team == region_opponent) & ((seed_team == 6 | seed_team == 11 | seed_team == 3 | seed_team == 14) & (seed_opponent == 7 | seed_opponent == 10 | seed_opponent == 2 | seed_opponent == 15)) ~"r3",
   
    (region_team == region_opponent) & ((seed_opponent == 1 | seed_opponent == 16 | seed_opponent == 8 | seed_opponent == 9) & (seed_team == 5 | seed_team == 12 | seed_team == 4 | seed_team == 13)) ~"r3",
    (region_team == region_opponent) & ((seed_opponent == 6 | seed_opponent == 11 | seed_opponent == 3 | seed_opponent == 14) & (seed_team == 7 | seed_team == 10 | seed_team == 2 | seed_team == 15)) ~"r3",
    
    
    (region_team == region_opponent) & ((seed_team == 1 | seed_team == 16 | seed_team == 8 | seed_team == 9 | seed_team == 5 | seed_team == 12 | seed_team == 4 | seed_team ==13) & (seed_opponent == 6 | seed_opponent == 11 | seed_opponent == 3 | seed_opponent == 14 | seed_opponent == 7 | seed_opponent == 10 | seed_opponent == 2 | seed_opponent == 15)) ~"r4",
    
    (region_team == region_opponent) & ((seed_opponent == 1 | seed_opponent == 16 | seed_opponent == 8 | seed_opponent == 9 | seed_opponent == 5 | seed_opponent == 12 | seed_opponent == 4 | seed_opponent ==13) & (seed_team == 6 | seed_team == 11 | seed_team == 3 | seed_team == 14 | seed_team == 7 | seed_team == 10 | seed_team == 2 | seed_team == 15)) ~"r4"
     ),   win_prob = ifelse(when_can_play == "r1",  r1_team, 
                            ifelse(when_can_play == "r2",(1-(r2_team + r2_opponent))/2+r2_team,
                                   ifelse(when_can_play == "r3",(1-(r3_team + r3_opponent))/2+r3_team,
                                          ifelse(when_can_play == "r4",(1-(r4_team + r4_opponent))/2+r4_team,
                                                 ifelse(when_can_play=="r5",(1-(r5_team + r5_opponent))/2+r5_team,
                                                        ifelse(when_can_play == "r6", (1-(r6_team + r6_opponent))/2+r6_team,0.5))))))) %>% select(team, opponent, win_prob)

  wp_matrix$win_prob = ifelse(is.na(wp_matrix$win_prob), 0.5, wp_matrix$win_prob)
 # win_prob = ifelse(when_can_play == "r1",  r1_team, 
 #                   ifelse(when_can_play == "r2",(1-(r2_team + r2_opponent))/2+r2_team,
 #                          ifelse(when_can_play == "r3",(1-(r3_team + r3_opponent))/2+r3_team,
 #                                 ifelse(when_can_play == "r4",(1-(r4_team + r4_opponent))/2+r4_team,
 #                                        ifelse(when_can_play=="r5",(1-(r5_team + r5_opponent))/2+r5_team,
 #                                               ifelse(when_can_play == "r6", (1-(r6_team + r6_opponent))/2+r6_team,0.5))))))) %>% select(team, opponent, win_prob)

# win_prob = ifelse(when_can_play == "r1",  r1_team, 
#                   ifelse(when_can_play == "r2",r2_team / (r2_team + r2_opponent),
#                          ifelse(when_can_play == "r3",r3_team / (r3_team + r3_opponent),
#                                 ifelse(when_can_play == "r4",r4_team / (r4_team + r4_opponent),
#                                        ifelse(when_can_play=="r5",r5_team / (r5_team + r5_opponent),
#                                               ifelse(when_can_play == "r6", r6_team / (r6_team + r6_opponent),0.5))))))) %>% select(team, opponent, win_prob)
#
  
  ### Score Diff of Game
  #mutate('pred_score_diff' = rating_team - rating_opponent)%>%  
  ### Win Prob for Team over Opponent
  #mutate('win_prob' = round(pnorm(pred_score_diff, 0,11),5))


### First Four
first_four <- 
  seed_list %>% 
  filter(first_four) %>% 
  build_bracket()
first_four <- map(1:n_creation_sims, ~{first_four})

first_four_winners <- future_map(first_four, sim_round)

### Brackets for Tournament Proper
ncaa_brackets <- 
  map(first_four_winners, ~{
    filter(seed_list, !first_four | team %in% .x) 
  })

### Keep Track of Winners
bracket_created <- list()

### Simulate Tournament
for(i in 1:6) {
  cat('Simulating Round', i, 'of', 6, '\n')
  ### in bracket Form
  brackets <- future_map(ncaa_brackets, build_bracket)
  winners_creation <- future_map(brackets, sim_round)
  bracket_created[[i]] <- unlist(winners_creation)
  ncaa_brackets <- map2(ncaa_brackets, winners_creation, ~{filter(.x, team %in% .y)})
}

### Aggregate Results
sim_results <- 
  seed_list %>% 
  select(-elim_round) %>% 
  group_by(team) %>% 
  mutate('first_round' = ifelse(!first_four, 1, sum(team == unlist(first_four_winners))/n_creation_sims),
         'second_round' = sum(team == bracket_created[[1]])/n_creation_sims,
         'sweet_sixteen' = sum(team == bracket_created[[2]])/n_creation_sims,
         'elite_eight' = sum(team == bracket_created[[3]])/n_creation_sims,
         'final_four' = sum(team == bracket_created[[4]])/n_creation_sims,
         'championship_game' = sum(team == bracket_created[[5]])/n_creation_sims,
         'champ' = sum(team == bracket_created[[6]])/n_creation_sims) %>% 
  ungroup() %>% 
  select(-first_four) 

#write_csv(sim_results, 'ncaa_sims.csv')

## general idea ##

## create "good" brackets using vegas win chance & something else

## create "bad" brackets using espn pick chance

## slider to blend how "bad" the brackets are

## run sim, collect points, create rank ##

library(tidyverse)
library(data.table)
library(furrr)
options(future.fork.enable = T)

### Number of sims to run
n_result_sims <- 1
# set.seed(13579)

### Function to sim games 
sim_game <- function(team, opponent, win_prob) {
  ifelse(runif(1) <= win_prob, team, opponent)
}

### Function to Build a Bracket For a Round
build_bracket <- function(seed_list) {
  n <- nrow(seed_list)
  ix <- seq(1, n, 2)
  tibble('team' = seed_list$team[ix],
         'opponent' = seed_list$team[-ix]) %>% 
    inner_join(wp_matrix, by = c('team', 'opponent'))
  
}

### Function to Simulate Games in Round
sim_round <- function(bracket) {
  pmap_chr(list('team' = bracket$team,
                'opponent' = bracket$opponent,
                'win_prob' = bracket$win_prob),
           sim_game)
}



### Read in Seed list and team ratings
seed_list <- read_csv("~/Downloads/bracketmaniav2 - seed_list.csv")

power_rankings <- read_csv("~/Downloads/bracketmaniav2 - KP.csv")

# power_rankings <- read_csv("~/Downloads/power_rankings.csv")
power_rankings = power_rankings %>% rename("team" = "TeamName")
vegas <- read_csv("~/Downloads/bracketmaniav2 - vegas_so_far.csv")


seed_list <- 
  seed_list %>% 
  inner_join(select(power_rankings, team, 'rating' = AdjEM), by = 'team')


### Compute a win-probability matrix for each possible combination of teams
wp_matrix_test <- 
  crossing('team' = seed_list$team, 
           'opponent' = seed_list$team) %>% 
  inner_join(select(seed_list, team, rating), 
             by = 'team') %>% 
  inner_join(select(seed_list, team, rating), 
             by = c('opponent' = 'team'), 
             suffix = c('_team', '_opponent')) %>% 
  ### Score Diff of Game
  mutate('pred_score_diff' = rating_team - rating_opponent)%>%  
  ### Win Prob for Team over Opponent
  mutate('win_prob' = round(pnorm(pred_score_diff, 0,11),5))

wp_matrix_test = left_join(wp_matrix_test, vegas[c("pwin","team","opponent")], by = c("team"="team","opponent"="opponent"))
wp_matrix_test$win_prob = ifelse(!is.na(wp_matrix_test$pwin), wp_matrix_test$pwin, wp_matrix_test$win_prob)
wp_matrix_test$pwin = NULL


### First Four
first_four <- 
  seed_list %>% 
  filter(first_four) %>% 
  build_bracket()
first_four <- map(1:n_result_sims, ~{first_four})

first_four_winners <- future_map(first_four, sim_round)

### Brackets for Tournament Proper
ncaa_brackets <- 
  map(first_four_winners, ~{
    filter(seed_list, !first_four | team %in% .x) 
  })

### Keep Track of Winners
round_winners <- list()

### Simulate Tournament
for(i in 1:6) {
  cat('Simulating Round', i, 'of', 6, '\n')
  ### in bracket Form
  brackets <- future_map(ncaa_brackets, build_bracket)
  winners <- future_map(brackets, sim_round)
  round_winners[[i]] <- unlist(winners)
  ncaa_brackets <- map2(ncaa_brackets, winners, ~{filter(.x, team %in% .y)})
}

### Aggregate Results
sim_results <- 
  seed_list %>% 
  select(-elim_round) %>% 
  group_by(team) %>% 
  mutate('first_round' = ifelse(!first_four, 1, sum(team == unlist(first_four_winners))/n_result_sims),
         'second_round' = sum(team == round_winners[[1]])/n_result_sims,
         'sweet_sixteen' = sum(team == round_winners[[2]])/n_result_sims,
         'elite_eight' = sum(team == round_winners[[3]])/n_result_sims,
         'final_four' = sum(team == round_winners[[4]])/n_result_sims,
         'championship_game' = sum(team == round_winners[[5]])/n_result_sims,
         'champ' = sum(team == round_winners[[6]])/n_result_sims) %>% 
  ungroup() %>% 
  select(-first_four) 

#write_csv(sim_results, 'ncaa_sims.csv')





