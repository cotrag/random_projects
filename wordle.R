library(words)
data("words")

word_ds <- words %>% 
  filter(word_length == 5)


## STEP 1: PRINT RANDOM WORD WITH NO REPEAT LETTERS 

word_ds <- word_ds %>% 
  dplyr::select(word)

word_split <- str_split(word_ds$word, "", 5)

word_split_df <- do.call(rbind.data.frame, word_split)

word_split_df$c..a....a....a....a....a....a....a....a....a....a....a....a...
word_split_df$c..a....a....a....b....b....b....b....b....b....b....b....b...
word_split_df$c..h....l....r....a....a....a....a....a....a....a....a....a...
word_split_df$c..e....i....g....c....c....c....f....k....m....s....s....t...
word_split_df$c..d....i....h....a....i....k....t....a....p....e....h....e...

word_split_df <- word_split_df %>% 
  rename(
    letter_1 = c..a....a....a....a....a....a....a....a....a....a....a....a...,
    letter_2 = c..a....a....a....b....b....b....b....b....b....b....b....b...,
    letter_3 = c..h....l....r....a....a....a....a....a....a....a....a....a...,
    letter_4 = c..e....i....g....c....c....c....f....k....m....s....s....t...,
    letter_5 = c..d....i....h....a....i....k....t....a....p....e....h....e...
    
  )

# find num unique letters in each row

word_split_df$num_uni <- apply(word_split_df,1,function(x) length(unique(x)))

first_guess_list <- word_split_df %>% 
  filter(num_uni == 5)

first_guess_list[sample(nrow(first_guess_list), 1), ]

# test: correct word is gluts
# console shows 1st guess 
# 1st guess: birks -- blank blank blank blank green

wrong_list <- c("w", "e", "a", "b", "i", "n")


second_guess_list <- word_split_df %>% 
  filter(!(letter_1 %in% wrong_list) &  
          !(letter_2 %in% wrong_list) & 
          !(letter_3 %in% wrong_list) & 
          !(letter_4 %in% wrong_list) &
          !(letter_5 %in% wrong_list) &
           letter_2 == "r" & 
           (letter_5 == "y"))

second_guess_list[sample(nrow(second_guess_list), 1), ]

# second guess: saves -- yellow blank blank blank green

wrong_list <- c("r", "i", "r", "k", "e", "a", "v")

third_guess_list <- word_split_df %>% 
  filter(!(letter_1 %in% wrong_list) &  
           !(letter_2 %in% wrong_list) & 
           !(letter_3 %in% wrong_list) & 
           !(letter_4 %in% wrong_list) &
           !(letter_5 %in% wrong_list) &
           letter_5 == "s")

third_guess_list[sample(nrow(third_guess_list), 1), ]

# third guess: gulls: green yellow yellow yellow green 
wrong_list <- c("b", "i", "r", "k", "e", "a", "v", "d")

fourth_guess_list <- word_split_df %>% 
  filter(!(letter_1 %in% wrong_list) &  
           !(letter_2 %in% wrong_list) & 
           !(letter_3 %in% wrong_list) & 
           !(letter_4 %in% wrong_list) &
           !(letter_5 %in% wrong_list) &
           letter_5 == "s" &
           letter_1 == "g" & 
           (letter_2 == "l" | letter_3 == "l" | letter_4 == "l") &
           (letter_2 == "u" | letter_3 == "u" | letter_4 == "u"))



