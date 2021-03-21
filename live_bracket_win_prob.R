library(readr)
sc_r <- read_csv("~/Downloads/sc - r.csv")
library(dplyr)

library(dplyr)
library(readr)
files <- list.files(path="~/Documents/Brac", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 



sc_r = rbind(sc_r, files)
#sc_r = files
g = seq.int(38, 473, by = 7)
f = seq.int(37, 471, by = 7)

positions = c(2, g, f)

bind <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("df", "V2", "V3")
colnames(bind) <- x

lol = sc_r %>% select(positions)
for( i in 1:nrow(lol)){
  sub = lol[i,]
  hold_picks = t(sub[2:64])
  hold_when = t(sub[65:127])
  hold_name = sub$`data/entry/name`
  df= rep(hold_name, 63)
  together = cbind(df, hold_picks, hold_when)
  rownames(together) <- c()
  bind = rbind(bind, together)
  print(i)
}

#lol = t(lol[2:64])
#lol = as.data.frame(lol)
lol = bind
sc_game_num_map <- read_csv("~/Downloads/sc - game_num_map.csv")
lol = left_join(lol, sc_game_num_map, by = c("V3"="order"))
name_map <- read_csv("~/Downloads/sc - name_map.csv")
lol = left_join(lol, name_map, by = c("V2"="Id"))

lol = lol %>% arrange(df, game_number)
nas = subset(lol, is.na(Team))




### write.csv(nas, file = "hmm.csv")
