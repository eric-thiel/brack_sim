### get winning bracket shit ###

get_best_bracket = function(bracket_created, winner_bracket){
  
  winner_bracket_lower = winner_bracket*32+1
  winner_bracket_upper = winner_bracket*32+32
  
  find_bracket_1st = bracket_created[[1]]
  find_bracket_1st = find_bracket_1st[winner_bracket_lower:winner_bracket_upper]
  
  winner_bracket_lower = winner_bracket*16+1
  winner_bracket_upper = winner_bracket*16+16
  
  find_bracket_2nd = bracket_created[[2]]
  find_bracket_2nd = find_bracket_2nd[winner_bracket_lower:winner_bracket_upper]
 
   winner_bracket_lower = winner_bracket*8+1
  winner_bracket_upper = winner_bracket*8+8
  
  find_bracket_3rd = bracket_created[[3]]
  find_bracket_3rd = find_bracket_3rd[winner_bracket_lower:winner_bracket_upper]
  
  
  winner_bracket_lower = winner_bracket*4+1
  winner_bracket_upper = winner_bracket*4+4
  
  find_bracket_4th = bracket_created[[4]]
  find_bracket_4th = find_bracket_4th[winner_bracket_lower:winner_bracket_upper]
  
  
  winner_bracket_lower = winner_bracket*2+1
  winner_bracket_upper = winner_bracket*2+2
  
  find_bracket_5th = bracket_created[[5]]
  find_bracket_5th = find_bracket_5th[winner_bracket_lower:winner_bracket_upper]
  
  
  winner_bracket_final = winner_bracket*1+1

  find_bracket_6th = bracket_created[[6]]
  find_bracket_6th = find_bracket_6th[winner_bracket_final]
  
  finals = list(find_bracket_1st, find_bracket_2nd, find_bracket_3rd, find_bracket_4th, find_bracket_5th, find_bracket_6th)
  return(finals)
}


