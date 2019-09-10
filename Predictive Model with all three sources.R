# Create an n-gram dictionary,keeping stopwords
# Load required packages
library(stringr)
 library(quanteda)
 library(tidyr)
library(tm)
library(dplyr)

tfilePath <- "~/en_US/en_US.twitter.txt"
nfilePath <- "~/en_US/en_US.news.txt"
bfilePath <- "~/en_US/en_US.blogs.txt"
twitter <- readLines(tfilePath,  skipNul = TRUE)
news <- readLines(nfilePath,  skipNul = TRUE)
blogs <- readLines(bfilePath,  skipNul = TRUE)
###Reexamine raw data.Look for matches of quiz questions, using grep()
occurrence <- grep("and a case of", news)
occurrence


#Originally the Twitter feed was 2360148 characters, News was 77259 characters,
#Blogs was 899288 characters in length.  Sizes were obtained by checking the environment 
#window.The massive file sizes made it necessary to use sampling to reduce the files size. Analyze a
#random sample of 60 % of the Twitter file, 60% of the News file, 60% of the Blogs.


set.seed(1234)
twittersample <- twitter[rbinom(length(twitter)*.60,length(twitter),.60)]
newssample <- news[rbinom(length(news)*.60,length(news),.60)]
blogsample <- blogs[rbinom(length(blogs)*.60,length(blogs),.60)]

twitter_edited <- str_replace_all(string=twittersample, pattern= "[&â€œ#¦T®ðY¥™]" , replacement= "")

#Special characters in Twitter " & â ???¦T ð Y ¥ ™ " were removed.
#Next the texts were converted to a tm package corpus. tm was used to further clean the texts
#Remove special characters in news and blogs
news_edited <- str_replace_all(string=newssample, pattern= "[&â€œ#¦T®ðY¥™]" , replacement= "")
blogs_edited <- str_replace_all(blogsample, pattern= "[&â€œ#¦T®ðY¥™]" , replacement= "")



###Combine the data sources


all<- c(twitter_edited, news_edited, blogs_edited)
#Create a tm corpus
alldocs<-Corpus(VectorSource(all))
#To further clean the texts use this code:

#Convert to lower case
alldocs <-tm_map(alldocs, content_transformer(tolower))
#Remove numbers
alldocs <-tm_map(alldocs,removeNumbers)
# Leave in common stopwords  
#alldocs <- tm_map(alldocs,removeWords, stopwords("english"))
# Remove punctuations
alldocs <- tm_map(alldocs, removePunctuation)
#Eliminate extra white spaces
alldocs <- tm_map(alldocs, stripWhitespace)


# Tokenization
qalldocs<-corpus(alldocs)#Use a quanteda corpus
start.time<- Sys.time()#Timing tokenization
toks_all <- tokens(qalldocs, remove_punct=TRUE)
end.time <-Sys.time()
time.taken <-end.time - start.time
time.taken
#unigrams
start.time<- Sys.time()#Timing tokenization
toks_unigram <- tokens_ngrams(toks_all, n=1)
dfm_unigram <- dfm(toks_unigram)
unigrams <- textstat_frequency(dfm_unigram)
end.time <-Sys.time()
time.taken <-end.time - start.time
time.taken
head(unigrams)
library(dplyr)
unigram <-rename(unigrams,word1=feature) %>%
   mutate(word2=" ",nprefix=0) %>%
  select(word1, word2, frequency,nprefix)
head(unigram) 
unigram5<- unigram[1:5, ]
unigram5

# bi-grams
start.time<- Sys.time()#Timing tokenization
toks_bigram <- tokens_ngrams(toks_all, n=2)
dfm_bigram <- dfm(toks_bigram)
bi_dat <- textstat_frequency(dfm_bigram)

#separate the bigram into a first word and second word
bigram <- separate( bi_dat, feature, c("word1", "word2"), sep = "_")
bigram <- select(bigram, word1, word2)
#Add a frequency column
frequency<-bi_dat$frequency
bigram <-cbind(bigram, frequency)%>% mutate(nprefix= 1)
end.time <-Sys.time()
time.taken <-end.time - start.time
time.taken

head(bigram)

# tri-grams
start.time<- Sys.time()#Timing tokenization
toks_trigram <- tokens_ngrams(toks_all, n=3)
dfm_trigram <- dfm(toks_trigram)
tri_dat <- textstat_frequency(dfm_trigram)
#separate the trigram into bigram and third word
 
trigram <-separate(tri_dat, feature, c("word1", "word2"), sep = "_(?=[^_]+$)")
trigram <- select(trigram, word1, word2)
frequency <-tri_dat$frequency 
trigram <- cbind(trigram,frequency)%>% mutate(nprefix=2)

head(trigram)
end.time <-Sys.time()
time.taken <-end.time - start.time
time.taken

#Rewrite word1, using blank spaces instead of underscores
trigram <- trigram %>% mutate(word1 = str_replace_all(word1, "_", " "))
head(trigram)

# 4-grams
start.time<- Sys.time()#Timing tokenization
toks_4gram <- tokens_ngrams(toks_all, n=4)
system.time({tokens_ngrams})
dfm_4gram <- dfm(toks_4gram)
dat4 <- textstat_frequency(dfm_4gram)
#separate the 4gram into trigram and fourth word

all4gram <-separate(dat4, feature, c("word1", "word2"), sep = "_(?=[^_]+$)")
all4gram <- select(all4gram, word1, word2)
#Add a column of frequencies

frequency <- dat4$frequency
all4gram <- cbind(all4gram, frequency)%>% mutate(nprefix=3)
end.time <-Sys.time()
time.taken <-end.time - start.time
time.taken
head(all4gram)
#Rewrite word1, using blank spaces instead of underscores
all4gram <- all4gram %>% mutate(word1 = str_replace_all(word1, "_", " "))
head(all4gram)
# 5-grams
start.time <-Sys.time()
toks_5gram <- tokens_ngrams(toks_all, n=5)
dfm_5gram <- dfm(toks_5gram)
dat5 <- textstat_frequency(dfm_5gram)
#separate the 5gram into 4gram and fifth word

all5gram <-separate(dat5, feature, c("word1", "word2"), sep = "_(?=[^_]+$)")
all5gram <- select(all5gram, word1, word2)
#Add a column of frequencies

frequency <- dat5$frequency
all5gram <- cbind(all5gram, frequency)%>% mutate(nprefix=4)
end.time <- Sys.time()
time.taken<-end.time - start.time
time.taken
head(all5gram)
#Rewrite word1, using blank spaces instead of underscores
all5gram <- all5gram %>% mutate(word1 = str_replace_all(word1, "_", " "))
head(all5gram)
##Create a dictionary for the prediction algorithm
dictionary<- rbind( unigram5,bigram, trigram, all4gram, all5gram)
dim(dictionary)
#Dictionary had 752743 rows


#Save to RDS file.
saveRDS(dictionary,"dictionary.rds")






nxtword1<- function(word){dat %>% filter(word1 == word) %>% select(-word1)}
##function to change ngram to n-1 gram
less1gram <- function(x){str_replace(x, "^[^_]+_", "")
} 


#Attempt while loop
#Use while loop
whatsnext <- function(phrase){
  nwords <-str_count(phrase, pattern="_")
  while (!(phrase %in% dat$word1) & nwords >=0){
    
    phrase<- less1gram(phrase)
    print(phrase)
    nwords<-str_count(phrase, pattern="_")
    print(nxtword1(phrase))
  }
}
##try it
whatsnext("telling_me_about_his")

###A suggested correction
whatsnext1 <- function(phrase){
  nwords <-str_count(phrase, pattern="_")
  while (!(phrase %in% dat$word1)  && nwords >=1){
    phrase<- less1gram(phrase)
    nwords<-str_count(phrase, pattern="_")
    print(nxtword1(phrase))
  }
}
##try it
whatsnext1("telling_me_about_his")
##try it
whatsnext1("at_social")
whatsnext1("social")

##Suggested correction
whatsnext2 = function(phrase) {
  
  m = str_locate_all(phrase, "_")[[1]][,1]
  
  m = c(1, m+1)
  
  sapply(m, function(idx) {
    
    print(nxtword1(str_sub(phrase, idx)))
    
  })
  
}
##Try it
whatsnext2("telling_me_about_his")

whatsnext1("see_arctic_monkeys_this")
whatsnext2("see_arctic_monkeys_this")









# write a function that returns the second word when the first word is typed in.


nxtword1<- function(word){dat %>% filter(word1 == word) %>% select(-word1)}
##function to change ngram to n-1 gram
less1gram <- function(x){str_replace(x, "^[^_]+_", "")
}
####This function will look up a text string in a dictionary.  If no match is found,
##the text string will be shortened, then looked up again.


match<- nxtword1("dog_social")
if(nrow(match)>0){print(match)
} else(nxtword1(less1gram("dog_social")))

match<- nxtword1("dog_social")
if(nrow(match)>0){print(match)
} else(nxtword1(less1gram("dog_social")))

########################################
#Attempt while loop
#Use while loop
whatsnext <- function(phrase){
nwords <-str_count(phrase, pattern="_")
while (nwords >1){
  
  phrase<- less1gram(phrase)
  nwords<-str_count(phrase, pattern="_")
  print(nxtword1(phrase))
}
}
whatsnext("manufacturer_custom_built" )
###############################################################################  
#I created a dictionary of unigrams, bigrams, trigrams, and 4-grams.
#Here is a function to look up words in the dictionary (named dat)

nxtword1<- function(word){dat %>% filter(word1 == word) %>% select(-word1)}

#I tested this function, and it worked.

#The purpose of the following code is to look up a text string in the dictionary. Messages will be printed to the console, whether the phrase appears in the dictionary or not.
checkdic <-function(word) {ifelse(word %in% dat$word1, nxtword1(word), nxtword1(less1gram(word)))
  }
checkdic("dog_social")


##Test function body
ifelse("furnishings_solid_wood" %in% dat$word1, print("match found"), print("match not found"))


]

#I knew that the phrase "new_manufacturer_custom_built" did not appear in the dictionary. Apparently R does not like my
#conditional statement. I was hoping that the else statement would be executed. Instead an error message appears
#Error in if (row.names(predict) > 0) { : argument is of length zero
  
  else {print("no match found")}
  
  Error: unexpected 'else' in "else"
  
  predict}
it means your predict object doesn't have row names.
some data frame just doesn't have row names.
try nrow(predict) instead.

