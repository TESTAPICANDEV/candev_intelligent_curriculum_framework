#visualising topics of words based on the max value of phi
set.seed(1234)
final_summary_words <- data.frame(top_terms = t(model$top_terms))
final_summary_words$topic <- rownames(final_summary_words)
rownames(final_summary_words) <- 1:nrow(final_summary_words)
final_summary_words <- final_summary_words %>% melt(id.vars = c("topic"))
final_summary_words <- final_summary_words %>% rename(word = value) %>% select(-variable)
final_summary_words <- final_summary_words %>% group_by(topic,word)
final_summary_words <- final_summary_words %>% group_by(topic, word) %>% filter(row_number() == 1) %>% 
  ungroup() %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)
word_topic_freq <- left_join(final_summary_words, original_tf, by = c("word" = "term"))

for(i in 1:length(unique(final_summary_words$topic))) {
  pdf(paste0("cluster", i, ".pdf"))
  temp <- filter(final_summary_words, topic == i)$word
  wordcloud(words = temp, freq = word_topic_freq$term_freq,
            min.freq = 1,
             max.words=200, random.order=FALSE, rot.per=0.35, 
             colors=brewer.pal(8, "Dark2"))
  dev.off()
  }

