# hold_num = rep(NA, len)
n_in_pool = 137
len = n_result_sims * n_in_pool
hold_num = rep(NA, len)

#hold_num <- character(len)
# hold_which_bracket <- character(len)
# sim_num <- character(len)

hold_which_bracket = rep(NA, len)
sim_num = rep(NA, len)

calc_bracket_score = function(bracket_created, round_winners){ 
  chunk_length = 32
  
  creation_rd1 = split(bracket_created[[1]],             # Applying split() function
        ceiling(seq_along(bracket_created[[1]]) / 32))
  
  results_rd1 = split(round_winners[[1]],
                      ceiling(seq_along(round_winners[[1]]) / 32))
  
  creation_rd2 = split(bracket_created[[2]],             # Applying split() function
                       ceiling(seq_along(bracket_created[[2]]) / 16))
  
  results_rd2 = split(round_winners[[2]],
                      ceiling(seq_along(round_winners[[2]]) / 16))
  
  creation_rd3 = split(bracket_created[[3]],             # Applying split() function
                       ceiling(seq_along(bracket_created[[3]]) / 8))
  
  results_rd3 = split(round_winners[[3]],
                      ceiling(seq_along(round_winners[[3]]) / 8))
  
  creation_rd4 = split(bracket_created[[4]],             # Applying split() function
                       ceiling(seq_along(bracket_created[[4]]) / 4))
  
  results_rd4 = split(round_winners[[4]],
                      ceiling(seq_along(round_winners[[4]]) / 4))
  
  creation_rd5 = split(bracket_created[[5]],             # Applying split() function
                       ceiling(seq_along(bracket_created[[5]]) / 2))
  
  results_rd5 = split(round_winners[[5]],
                      ceiling(seq_along(round_winners[[5]]) / 2))
  
  creation_rd6 = split(bracket_created[[6]],             # Applying split() function
                       ceiling(seq_along(bracket_created[[6]]) / 1))
  
  results_rd6 = split(round_winners[[6]],
                      ceiling(seq_along(round_winners[[6]]) / 1))
  x = 0
  for (j in 1:n_result_sims) {
    for (i in 1:n_in_pool) {
  x = x+1
round_1_score <- creation_rd1[[i]] == results_rd1[[j]]
round_1_score = (length(which(round_1_score)))

round_2_score <- creation_rd2[[i]] == results_rd2[[j]]
round_2_score = (length(which(round_2_score)))*2

round_3_score <- creation_rd3[[i]] == results_rd3[[j]]
round_3_score = (length(which(round_3_score)))*4

round_4_score <- creation_rd4[[i]] == results_rd4[[j]]
round_4_score = (length(which(round_4_score)))*8

round_5_score <- creation_rd5[[i]] == results_rd5[[j]]
round_5_score = (length(which(round_5_score)))*16

round_6_score <- creation_rd6[[i]] == results_rd6[[j]]
round_6_score = (length(which(round_6_score)))*32

score = sum(round_1_score, round_2_score, round_3_score, round_4_score, round_5_score, round_6_score)
# print(i)
# print(j)
hold_num[x] = score
hold_which_bracket[x] = i
sim_num[x] = j


    }

    print(j/n_result_sims)
# round_2_score <- bracket_created[[2]] == round_winners[[2]]
# round_2_score = (length(which(round_2_score)))*2
# 
# round_3_score <- bracket_created[[3]] == round_winners[[3]]
# round_3_score = (length(which(round_3_score)))*4
# 
# round_4_score <- bracket_created[[4]] == round_winners[[4]]
# round_4_score = (length(which(round_4_score)))*8
# 
# round_5_score <- bracket_created[[5]] == round_winners[[5]]
# round_5_score = (length(which(round_5_score)))*16
# 
# round_6_score <- bracket_created[[6]] == round_winners[[6]]
# round_6_score = (length(which(round_6_score)))*32
# 
# score = sum(round_1_score, round_2_score, round_3_score, round_4_score, round_5_score, round_6_score)
# return(score)
  }
newlist = list(hold_num = hold_num, which_bracket = hold_which_bracket, sim_num = sim_num)
return(newlist)
  }
 

g = calc_bracket_score(bracket_created, round_winners)

scores = g[[1]]
bracket_num = g[[2]]
sim_num = g[[3]]

results = data.frame(scores, bracket_num, sim_num)

results = left_join(results, summary_which, by = c("bracket_num"="bracket_number"))

max_pts = results %>% group_by(sim_num)%>%
           mutate(rank = rank(-scores, ties.method = c("random")))


summary = max_pts %>% group_by(df)%>%
  summarise(win_prob = sum(ifelse(rank == 1, 1,0))/n_result_sims)
summary = summary %>% rename("Bracket" = "df")
summary$win_prob = round(summary$win_prob, 3)
summary = summary %>% arrange(-win_prob)
summary$Time = Sys.time()
write.csv(summary, file = "win_prob_as_of_now.csv", row.names = FALSE)


#wins_to_check = subset(max_pts, df == "Eric Thiel 3" & rank == 1)

get_cheering = function(bracket_name){
wins_to_check = subset(max_pts, df == bracket_name & rank == 1)
  
#winner_bracket = 29
#get_best_bracket(round_winners, 29)

hold_rd1_bb <- data.frame(matrix(ncol = 1, nrow = 0))
x <- c("hmmmmmm[[1]]")
colnames(hold_rd1_bb) <- x

hold_rd2_bb <- data.frame(matrix(ncol = 1, nrow = 0))
x <- c("hmmmmmm[[2]]")
colnames(hold_rd2_bb) <- x

hold_rd3_bb <- data.frame(matrix(ncol = 1, nrow = 0))
x <- c("hmmmmmm[[3]]")
colnames(hold_rd3_bb) <- x

hold_rd4_bb <- data.frame(matrix(ncol = 1, nrow = 0))
x <- c("hmmmmmm[[4]]")
colnames(hold_rd4_bb) <- x

hold_rd5_bb <- data.frame(matrix(ncol = 1, nrow = 0))
x <- c("hmmmmmm[[5]]")
colnames(hold_rd5_bb) <- x

hold_rd6_bb <- data.frame(matrix(ncol = 1, nrow = 0))
x <- c("hmmmmmm[[6]]")
colnames(hold_rd6_bb) <- x

for (t in wins_to_check$sim_num) {
  hmmmmmm = get_best_bracket(round_winners, t)
  rd1_bb = as.data.frame(hmmmmmm[[1]])
  rd2_bb = as.data.frame(hmmmmmm[[2]])
  rd3_bb = as.data.frame(hmmmmmm[[3]])
  rd4_bb = as.data.frame(hmmmmmm[[4]])
  rd5_bb = as.data.frame(hmmmmmm[[5]])
  rd6_bb = as.data.frame(hmmmmmm[[6]])
  
  
  hold_rd1_bb = rbind(hold_rd1_bb, rd1_bb)
  hold_rd2_bb = rbind(hold_rd2_bb, rd2_bb)
  hold_rd3_bb = rbind(hold_rd3_bb, rd3_bb)
  hold_rd4_bb = rbind(hold_rd4_bb, rd4_bb)
  hold_rd5_bb = rbind(hold_rd5_bb, rd5_bb)
  hold_rd6_bb = rbind(hold_rd6_bb, rd6_bb)
  
  }

wins_who_to_cheer_for <- 
  seed_list %>% 
  select(-elim_round) %>% 
  group_by(team) %>% 
  mutate(
         'second_round' = sum(team == hold_rd1_bb$`hmmmmmm[[1]]`)/nrow(wins_to_check),
         'sweet_sixteen' = sum(team == hold_rd2_bb$`hmmmmmm[[2]]`)/nrow(wins_to_check),
         'elite_eight' = sum(team == hold_rd3_bb$`hmmmmmm[[3]]`)/nrow(wins_to_check),
         'final_four' = sum(team == hold_rd4_bb$`hmmmmmm[[4]]`)/nrow(wins_to_check),
         'championship_game' = sum(team == hold_rd5_bb$`hmmmmmm[[5]]`)/nrow(wins_to_check),
         'champ' = sum(team == hold_rd6_bb$`hmmmmmm[[6]]`)/nrow(wins_to_check)) %>% 
  ungroup()

return(wins_who_to_cheer_for)

}
# get_sim = subset(max_pts, sim_num == 29)

wins_who_to_cheer_for = get_cheering("Ryder Varga")



