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
newlist = list(hold_num = hold_num, which_bracket = hold_which_bracket, sim_num = sim_num)
return(newlist)
  }
 

g = calc_bracket_score(bracket_created, round_winners)

scores = g[[1]]
bracket_num = g[[2]]
sim_num = g[[3]]

results = data.frame(scores, bracket_num, sim_num)

# pool_size = 200
# pool_sims = 1000
# options(dplyr.summarise.inform = FALSE)
# 
# holding_money = c(1:n_creation_sims)
# holding_money = as.data.frame(holding_money)
# holding_money = holding_money %>% rename("bracket_number"="holding_money")%>%
#   mutate(cumulative_money_won = 0)
# s = rep(1:(len/100),times = 100)
# jumper = rep(1:(len/100))
# s =  sort(s)
# 
# results_mirror = data.table(results)
# results_test = results
# 
# test = g
# 
# scores_test = scores
# bracket_num_test = bracket_num
# sim_num_test = sim_num
# 
# preallo = rep(NA, len/pool_size*pool_sims)
# x = 0
# 
# library(rlist)
# sim_pools = function(results_mirror){
# for (i in 1:pool_sims) {
#   
#   rands = runif(length(test[[1]]), 0, 1)
#   
#   test[["rands"]] <- rands
#   sorted = data.table(test$hold_num, test$which_bracket, test$sim_num, test$rands)
#  # sorted = sorted %>% arrange(test.rands)
#    sorted =  setorder(sorted, V3, V4)
#    sorted = cbind(sorted, s)
#   
#    
#  sorted = sorted %>% group_by(s)%>%
#    summarise(max = max(test.hold_num), first = first(test.which_bracket))
#  a <- table(sorted$first)
#  a = as.data.frame(a)
#  a$Var1 = as.numeric(a$Var1)
#   
#   
#   #sorted = test[order(unlist(test),decreasing=TRUE)]
#   
# 
#   #sorted= sort.list(test, which_bracket,test$hold_num)
#   
# 
# 
# 
#   
# #  results_m =  setorder(results_m, sim_num, rand)
# #  results_m = cbind(results_m, s)
# #  k =  results_m[results_m[, .I[which.max(scores)], by=s]$V1]
# #  k$won = 400
# #  df2 <- k[, .(sum = sum(won)), by=list(bracket_num)]
# #  df2 = as.data.frame(df2)
#   
#  #  holding_money = left_join(holding_money, a[c("Var1", "Freq")], by = c("bracket_number"="Var1"))
#  #  holding_money$Freq = ifelse(is.na(holding_money$Freq),0,holding_money$Freq)
#  #  holding_money$cumulative_money_won = holding_money$cumulative_money_won + (holding_money$Freq*400)
#  #  holding_money$Freq = NULL
#  #  print(i/pool_sims)
# }
#   
#   return(preallo)
#   
# }
#   
# holding_money = sim_pools(results_mirror)

# results_m = results %>% filter(!is.na(scores))%>%
#   arrange(-scores)%>%
#   group_by(sim_num)%>%
#   mutate(rank = rank(-scores, ties.method = "first"), won = ifelse(rank == 1, 400, ifelse(rank == 2, 200, ifelse(rank == 3, 100,-10))))%>%
#   group_by(bracket_num)%>%
#   summarise(avg_money_won = mean(won))%>% arrange(avg_money_won)

# holding_money = holding_money %>% arrange(-cumulative_money_won)


fix_mem_issues = function(results){
max_pts = results %>% group_by(sim_num)%>%
  mutate(avg = mean(scores), score_above_avg = scores - avg)%>%
  group_by(bracket_num)%>%
  summarise(mean_score_over_avg = mean(score_above_avg))%>%arrange(-mean_score_over_avg)
return(max_pts)
}

max_pts = fix_mem_issues(results)

winner_bracket = max_pts[1,] ## 2nd best
winner_bracket = winner_bracket$bracket_num

winn = get_best_bracket(bracket_created, winner_bracket)
