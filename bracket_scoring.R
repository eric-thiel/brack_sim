calc_bracket_score = function(bracket_created, round_winners){ 
  
round_1_score <- bracket_created[[1]] == round_winners[[1]]
round_1_score = length(which(round_1_score))

round_2_score <- bracket_created[[2]] == round_winners[[2]]
round_2_score = (length(which(round_2_score)))*2

round_3_score <- bracket_created[[3]] == round_winners[[3]]
round_3_score = (length(which(round_3_score)))*4

round_4_score <- bracket_created[[4]] == round_winners[[4]]
round_4_score = (length(which(round_4_score)))*8

round_5_score <- bracket_created[[5]] == round_winners[[5]]
round_5_score = (length(which(round_5_score)))*16

round_6_score <- bracket_created[[6]] == round_winners[[6]]
round_6_score = (length(which(round_6_score)))*32

score = sum(round_1_score, round_2_score, round_3_score, round_4_score, round_5_score, round_6_score)
return(score)
}
 

calc_bracket_score(bracket_created, round_winners)
 
