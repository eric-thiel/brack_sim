# hold_num = rep(NA, len)
len = n_result_sims * n_creation_sims
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
    for (i in 1:n_creation_sims) {
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
newlist = list(hold_num, hold_which_bracket, sim_num)
return(newlist)
  }
 

g = calc_bracket_score(bracket_created, round_winners)

scores = g[[1]]
bracket_num = g[[2]]
sim_num = g[[3]]

results = data.frame(scores, bracket_num, sim_num)

pool_size = 200
pool_sims = 1000
options(dplyr.summarise.inform = FALSE)

holding_money = c(1:n_creation_sims)
holding_money = as.data.frame(holding_money)
holding_money = holding_money %>% rename("bracket_number"="holding_money")%>%
  mutate(cumulative_money_won = 0)



for (i in 1:pool_sims) {
  
  results_m = results %>% filter(!is.na(scores))%>%
    mutate(rand = runif(nrow(results),0,1))%>%arrange(sim_num, rand)%>%mutate(
      id =  rep(seq(1, 1 + nrow(results) %/% pool_size), each = pool_size, length.out = nrow(results))
    )%>%group_by(id)%>%
    mutate(rank = rank(-scores, ties.method = "first"), won = ifelse(rank == 1, 400, ifelse(rank == 2, 200, ifelse(rank == 3, 100,-10))))%>%
    group_by(bracket_num)%>%
    summarise(money_won = sum(won))
  
  holding_money = left_join(holding_money, results_m[c("bracket_num", "money_won")], by = c("bracket_number"="bracket_num"))
  holding_money$cumulative_money_won = holding_money$cumulative_money_won + holding_money$money_won
  holding_money$money_won = NULL
  print(i/pool_sims)
}


  


# results_m = results %>% filter(!is.na(scores))%>%
#   arrange(-scores)%>%
#   group_by(sim_num)%>%
#   mutate(rank = rank(-scores, ties.method = "first"), won = ifelse(rank == 1, 400, ifelse(rank == 2, 200, ifelse(rank == 3, 100,-10))))%>%
#   group_by(bracket_num)%>%
#   summarise(avg_money_won = mean(won))%>% arrange(avg_money_won)

holding_money = holding_money %>% arrange(-cumulative_money_won)

winner_bracket = head(holding_money$bracket_number,1)

winn = get_best_bracket(bracket_created, winner_bracket)
