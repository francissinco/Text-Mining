
#histogram of frequent words
wf <- data.frame(term = names(freq_words), occurrences = freq_words, row.names = NULL)

p <- ggplot(subset(wf, freq_words >= 10), aes(reorder(term, -occurrences), occurrences))
p <- p + geom_bar(stat = "identity",fill="blue")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p <- p +   xlab("Terms") + ylab("Frequency")
p



#setting the histogram filename (automated to today's date, with use initials)
word_hist_filename <- paste(substring(gsub("-", "", Sys.Date()), 3), "df_all_histogram_FS.jpeg", sep = "_" )

#saving the histogram (built-in save function in ggplot2)
ggsave(word_hist_filename)

rm(word_hist_filename)
