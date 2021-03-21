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
n_result_sims <- 30000
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
    inner_join(wp_matrix_test, by = c('team', 'opponent'))
  
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



