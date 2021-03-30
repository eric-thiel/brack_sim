# brack_sim

R functions to generate brackets based on ESPN pick percentages and "score" the brackets pre-tournament using first round vegas odds and KenPom ratings.

Pre-tournament workflow is as follows - 

1) Get seed list from IBenz's sim (also now resides in excel, just a basic team, seed, boolean first four, region csv). 
2) Get ESPN pick percentages for each round, put into excel to clean & adjust to probability the team is picked if they make it to round 2, etc. 
3) Get KenPom Adjusted EM to simulate games beyond round 1.
4) Get vegas odds for first round games.


n_creation_sims is number of brackets to be created from espn pick percentages. These will be the brackets chosen from.

n_result_sims is the number of simulations to test the above brackets against. These are based on Odds & KP.

run generate optimal brackets

run return bracket function - just a function to sort through the lists of brackets and return a single bracket.

run return optimal brackets, changing "winner_bracket" to get the top bracket, 2nd best, etc.


Post-tournament workflow - 

1) be insane and get all brackets in your pool as a csv (inspect element shit bc im dumb)
2) discover how the ids match up to team names & fix those. 
3) run cbs_pools_cleaned, ensuring numbers are adjusted for amount of brackets in the pool.
4) generate the "results" of the rest of the tournament. This includes editing vegas_so_far so that game results are in there & new odds are as well as they are posted.
5) run live_win_prob, changing number in pool, can also change who to cheer for to get your path to a victory.



Takes a fairly long time but honestly is as optimized as I think ill get it. 

My ideal bracket (using ESPN pick percents (35% chose Zaga as winner, Win prob was more like 40%)

 [1] "Gonzaga"      "Oklahoma"        "Creighton"      "Ohio"           "USC"            "Kansas"         "VCU"            "Iowa"      
     "Michigan"     "LSU"             "Colorado"       "Florida St."    "BYU"            "Texas"          "Maryland"       "Alabama"    
     "Baylor"       "North Carolina"  "Villanova"      "Purdue"         "Texas Tech"     "Arkansas"       "Virginia Tech"  "Ohio St."    
     "Illinois"     "Loyola Chicago"  "Tennessee"      "Oklahoma St."   "San Diego St."  "West Virginia"  "Rutgers"        "Houston"       

 [1] "Gonzaga"       "Creighton"     "Kansas"         "Iowa"     
     "Michigan"      "Florida St."   "BYU"            "Alabama" 
     "Baylor"        "Villanova"     "Arkansas"       "Ohio St."     
     "Illinois"      "Oklahoma St."  "San Diego St."  "Houston"      

[1] "Gonzaga"    "Iowa"   
    "Michigan"   "Alabama" 
    "Baylor"     "Ohio St." 
    "Illinois"   "Houston" 

[1] "Gonzaga"   "Michigan" 
    "Baylor"    "Houston" 

[1] "Gonzaga" "Houston"

[1] "Gonzaga"


