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
  ##get a response when the initial input is blank
 # if(phrase==""){
 #   predicted_word <-(c("the","to", "and", "in", "of"))
#}
 
  ##While loop works when no matches are found in the dictionary and there is more than one word in
  #the prefix
  while (!(phrase %in% dat$word1)  && nwords >=1){
    #phrase<- less1gram(phrase)
    phrase <-str_replace(phrase, "^[^ ]+ ", "")
    nwords<-str_count(phrase, pattern=" ")
  }
   ifelse((phrase %in% dat$word1),predicted_word <-dat %>% filter(word1 == phrase) %>% select(-word1),
          predicted_word <-data.frame(as.character(c("the","to", "and", "in", "of")),stringsAsFactors=FALSE)) 
    
   
 
  predicted_word[1:5, 1]
  return(predicted_word[1:5,1])
}

