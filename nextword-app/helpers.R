#function that outputs the next word after a phrase is typed in 
library(stringr) 
library(dplyr)
#nxtword1<- function(word){dat %>% filter(word1 == word) %>% select(-word1)}
##function to change ngram to n-1 gram
#less1gram <- function(x){str_replace(x, "^[^ ]+ ", "")
#} 

# To print out the most common unigrams instead of a row of NAs,
##should no match be found.  The number of words in the prefix is nwords + 1
whatsnexta <- function(phrase) {
  nwords <-str_count(phrase, pattern=" ")
  ##While loop works when no matches are found in tge dictionary and there is more than one word in
  #the prefix
  while (!(phrase %in% dat$word1)  && nwords >=1){
    #phrase<- less1gram(phrase)
    phrase <-str_replace(phrase, "^[^ ]+ ", "")
    nwords<-str_count(phrase, pattern=" ")
    
    # A match is found 
    
    #{ print(nxtword1(phrase)[1:5, 1])}Output the next word
    if ((phrase %in% dat$word1)   ){
      # print(nxtword1(phrase)[1:5, 1])
      predicted_word <-dat %>% filter(word1 == phrase) %>% select(-word1)
      # print(answer[1:5, 1])
      #print(answer)
    }
    #No match found.  Output the five most common unigrams
    if (!(phrase %in% dat$word1)  && nwords==0) 
    { predicted_word <-(c("the","to", "and", "in", "of"))}
  }
  predicted_word <- predicted_word[1:5, 1]
  return(predicted_word)
}

