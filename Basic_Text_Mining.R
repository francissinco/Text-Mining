### BASIC TEXT MINING ###

#Building and cleaning the corpus from free text files, finding word frequencies and word associations, plotting word histograms, constructing word clouds


#USER
#francissinco



##########

### PREAMBLE ###

#Please install the following packages before running the script

#Loading packages
library(tm)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringi)
library(stringr)
library(cluster)
library(SnowballC)
library(wordcloud)
library(RTextTools)
library(RColorBrewer)
library(NLP)


#Setting the working directory (output directory)
setwd("~")




### BUILDING THE TEXT DIRECTORY ###

#Creating the input directory
#Please make sure to put all text files in a single folder
Data <- file.path("~")

Data

dir(Data)







### BUILDING AND CLEANING THE CORPUS ###


#Transforming the set of text files into a corpus, one document per .txt file
docs <- Corpus(DirSource(Data))
summary(docs)



#Transforming to lowercase; removing punctuations, numbers, stopwords, affixes, spaces
docs <- tm_map(docs, tolower) 
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stemDocument) 
docs <- tm_map(docs, stripWhitespace)   



#Removing special characters, metacharacters, URL, username, etc.

special_char <- c("\\$", "\\*", "\\+", "\\.", "\\?", "\\[", "\\]", "\\^", "\\{", "\\}", "\\|", "\\(", "\\)", "\\#", ":", ";", "-", "_", "&", "/", ",", "~", "@", "%", "!", "=", "<", ">", "'", "''", "`", "(RT|via)((?:\\b\\W*@\\w+)+)", "@\\w", "[[:punct:]]", "[[:digit:]]", "http\\w+", "[ \t]{2,}", "^\\s+|\\s+$", "amp")


for (i in seq(docs)) {
  for (j in seq(length(special_char))) {
    docs[[i]] <- gsub(special_char[j], "", docs[[i]])
  }
}


#Making sure that documents stay in plain text document format
docs <- tm_map(docs, PlainTextDocument)  





### WORD FREQUENCIES ###

#Term Document Matrix = incidence matrix; rows correspond to each unique term, columns correspond to each document
tdm <- TermDocumentMatrix(docs)


#Exporting the text document matrix as .csv file
tdm_mat <- as.matrix(tdm)
tdm_mat_filename <- paste(substring(gsub("-", "", Sys.Date()), 3), "Data_TermDocumentMatrix_FS.csv", sep = "_" )

write.csv(tdm_mat, tdm_mat_filename)

rm(tdm_mat_filename)



#Getting the total number of times each word appeared in the entire corpus
freq_words <- rowSums(tdm_mat)



#Minimum frequency for filtering words
min_ratio <- 0.2
freq <- round(min_ratio*(max(freq_words)), digits = -1)




#Getting the most frequently occuring words, with frequency dictated by freq
word_count <- data.frame(frequency = freq_words )

word_count <- word_count %>%
  add_rownames("words") %>%
  arrange(desc(frequency)) %>%
  subset(frequency >= freq)




#Exporting the word_count as a .csv file
word_count_filename <- paste(substring(gsub("-", "", Sys.Date()), 3), "Data_word_count_FS.csv", sep = "_" )

write.csv(word_count, word_count_filename)

rm(word_count_filename)







### WORD ASSOCIATIONS ###

#Minimum association coefficient
assoc <- 0.6
cor<- c(rep(assoc, nrow(word_count)))


#Finding associations for the most frequent words
WordAssocs <- findAssocs(tdm, word_count$words, cor)

WordAssocsList <- lapply(WordAssocs, function(x) data.frame(terms = names(x), cor = x, stringsAsFactors = FALSE))

WordAssocsDF <- dplyr::bind_rows(WordAssocsList, .id = "source")

WordAssocsDF <- data.frame(WordAssocs = WordAssocsDF)







### WORD HISTOGRAM ###


#Histogram of frequent words
wf <- data.frame(term = names(freq_words), occurrences = freq_words, row.names = NULL)

p <- ggplot(subset(wf, freq_words >= freq), aes(reorder(term, -occurrences), occurrences))
p <- p + geom_bar(stat = "identity",fill="blue")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p <- p +   xlab("Terms") + ylab("Frequency")
p



#setting the histogram filename (automated to today's date, with user initials)
word_hist_filename <- paste(substring(gsub("-", "", Sys.Date()), 3), "Data_histogram_FS.jpeg", sep = "_" )

#saving the histogram (built-in save function in ggplot2)
ggsave(word_hist_filename)

rm(word_hist_filename)








### WORD CLOUD ###

#setting the wordcloud filename (automated to today's date, with user initials)
word_cloud_filename <- paste(substring(gsub("-", "", Sys.Date()), 3), "Data_wordcloud_FS.jpeg", sep = "_" )

#assigning jpeg format to the wordcloud
jpeg(word_cloud_filename, width = 720, height = 720)

#setting the same seed each time ensures consistent look across clouds
set.seed(42)


#creating the colored wordcloud
wordcloud(names(freq_words), freq_words, min.freq = freq, colors = brewer.pal(8, "Dark2"))

dev.off()


rm(word_cloud_filename)
