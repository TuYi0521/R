install.packages("tm")
install.packages("textreuse")
install.packages("wordnet")
install.packages("zipfR")
install.packages("wordcloud")
install.packages("NLP")
install.packages("openNLP")
install.packages("formattable")

library(openNLP)
library(NLP)
library(tm)
library(textreuse)
library(wordnet)
library(zipfR)
library(wordcloud)
library(formattable)

#========== Try functions in lecture 9 =======#
data("acq")
acq
inspect(acq)
head(summary(acq), n=15)
dtm <- DocumentTermMatrix(acq)
termFreq(acq[[1]])
dm2 <- TermDocumentMatrix(acq, control = list(wordLengths = c(1, Inf)))
assoc <- findAssocs(dm2, "states", 0.25)
freq.terms <- findFreqTerms(dm2, lowfreq = 3)
freq.terms

#========= Get the 15 longest documents ==========#
temp <- tm_map(acq, content_transformer(tolower)) 
temp <- tm_map(temp, removeWords, stopwords("english")) 
temp <- tm_map(acq, removePunctuation) 
temp <- tm_map(temp, removeNumbers) 
temp <- tm_map(temp, stripWhitespace) 
dtm <- DocumentTermMatrix(temp)
word_count <- rowSums(as.matrix(dtm))
dtm_sort <- names(head(sort(word_count, decreasing = T), 15))
dtm_sort


#========= Show the dendrogram and wordcloud ========#
for(i in 1:50){
  test<-acq[2]
  test
  SATlow <- tm_map(test, content_transformer(tolower))
  SATlow
  removeNumPunct <-function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  SATcl <- tm_map(SATlow, content_transformer(removeNumPunct))
  myStopwords <- c(stopwords('english'))
  SATstop <- tm_map(SATcl, removeWords, myStopwords)
  SATtdm2<-TermDocumentMatrix(SATstop,control=list(wordLengths=c(1,Inf)))
  SATtdm2
  m1<-as.matrix(SATtdm2)
  word.freq<-sort(rowSums(m1),decreasing = T)
  word.freq
  pal <- brewer.pal(9, "BuGn")
  pal <- pal[-(1:4)]
  wordcloud(words = names(
    word.freq), freq = word.freq, min.freq = 2, random.order = F, colors = pal
  )
  testdf <- as.data.frame(word.freq)
  distMatrix <- dist(scale(testdf))
  fit <- hclust(distMatrix, method = "ward.D2")
  plot(fit)
  groups <- cutree(fit, k=4)
  rect.hclust(fit, k=4, border="red")
}


#======== Find the longest word and lonest sentence ==============#
sents <- tokenize_sentences(acq[[dtm_sort[1]]]$content)
word <- tokenize_words(acq[[dtm_sort[1]]]$content)
# longest word by chars
word[which.max(nchar(word))]
# longest sentence by chars
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[2]]]$content)
word <- tokenize_words(acq[[dtm_sort[2]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[3]]]$content)
word <- tokenize_words(acq[[dtm_sort[3]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[4]]]$content)
word <- tokenize_words(acq[[dtm_sort[4]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[5]]]$content)
word <- tokenize_words(acq[[dtm_sort[5]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[6]]]$content)
word <- tokenize_words(acq[[dtm_sort[6]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[7]]]$content)
word <- tokenize_words(acq[[dtm_sort[7]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[8]]]$content)
word <- tokenize_words(acq[[dtm_sort[8]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[9]]]$content)
word <- tokenize_words(acq[[dtm_sort[9]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[10]]]$content)
word <- tokenize_words(acq[[dtm_sort[10]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[11]]]$content)
word <- tokenize_words(acq[[dtm_sort[11]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[12]]]$content)
word <- tokenize_words(acq[[dtm_sort[12]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[13]]]$content)
word <- tokenize_words(acq[[dtm_sort[13]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[14]]]$content)
word <- tokenize_words(acq[[dtm_sort[14]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[15]]]$content)
word <- tokenize_words(acq[[dtm_sort[15]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

#========== Print the length of sentences of 10 largest files =====#
sents <- tokenize_sentences(acq[[dtm_sort[1]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[dtm_sort[2]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[dtm_sort[3]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[dtm_sort[4]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[dtm_sort[5]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[dtm_sort[6]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[dtm_sort[7]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[dtm_sort[8]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[dtm_sort[9]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[dtm_sort[10]]]$content)
formattable(sapply(sents, wordcount))
#======= Remove punctuation and display sentence ==========#
no_punct <- tm_map(acq, removePunctuation)
dataframe<-data.frame(text=unlist(sapply(no_punct, `[`, "content")), stringsAsFactors=F)
as.String(dataframe$text)

#================= Print part-of-speech =================#
require("NLP")
tokenized <- tokenize_words(as.String(dataframe$text))
s <-as.String(dataframe$text)
posTagger <- Maxent_POS_Tag_Annotator(language = "en", probs = F, model = NULL)
sent_token_annotator <- Maxent_Sent_Token_Annotator ()
word_token_annotator <- Maxent_Word_Token_Annotator ()
pos_tag_annotator <- Maxent_POS_Tag_Annotator ()
dir <- annotate(s, c(sent_token_annotator, word_token_annotator))
t <- annotate(s, posTagger, dir)
t
head(t, n = 50)

#============= Analyze word frequencey using zipfR =======#
library(zipfR)
?zipfR
data(acq)
summary(acq.spc)
N(acq.spc)
V(acq.spc)
#===Baayen's P
Vm(acq.spc, 1) / N(acq.spc)
plot(acq.spc)
plot(acq.spc, log = "x")
#===Looking at VGCs
summary(acq.vgc)
acq.vgc
N(acq.vgc)
plot(acq.vgc)
#=== Estimating LNRE models
???

