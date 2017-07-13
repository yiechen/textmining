# enconding: UTF-8
# Julia Silge´s Jane Austen Text Mining
# created by YieChen, from:
#    https://github.com/juliasilge/old_bloggy_blog/blob/master/_R/2016-04-29-Life-Changing-Magic.Rmd

library(janeaustenr)
library(tidytext)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
        group_by(book) %>%
        mutate(linenumber = row_number(),
               chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                       ignore_case = TRUE)))) %>%
        ungroup()

original_books

tidy_books <- original_books %>%
        unnest_tokens(word, text)

tidy_books

data("stop_words")
tidy_books <- tidy_books %>%
        anti_join(stop_words)

tidy_books %>%
        count(word, sort = TRUE)

library(tidyr)
bing <- sentiments %>%
        filter(lexicon == "bing") %>%
        select(-score)

bing

janeaustensentiment <- tidy_books %>%
        inner_join(bing) %>% 
        count(book, index = linenumber %/% 80, sentiment) %>% 
        spread(sentiment, n, fill = 0) %>% 
        mutate(sentiment = positive - negative)

janeaustensentiment

library(ggplot2)
library(viridis)
ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        facet_wrap(~book, ncol = 2, scales = "free_x") +
        theme_minimal(base_size = 13) +
        labs(title = "Sentiment in Jane Austen's Novels",
             y = "Sentiment") +
        scale_fill_viridis(end = 0.75, discrete=TRUE, direction = -1) +
        scale_x_discrete(expand=c(0.02,0)) +
        theme(strip.text=element_text(hjust=0)) +
        theme(strip.text = element_text(face = "italic")) +
        theme(axis.title.x=element_blank()) +
        theme(axis.ticks.x=element_blank()) +
        theme(axis.text.x=element_blank())

austen_sentences <- austen_books() %>% 
        group_by(book) %>% 
        unnest_tokens(sentence, text, token = "sentences") %>% 
        ungroup()

austen_sentences$sentence[39]

bingnegative <- sentiments %>%
        filter(lexicon == "bing", sentiment == "negative")

wordcounts <- tidy_books %>%
        group_by(book, chapter) %>%
        summarize(words = n())

tidy_books %>%
        semi_join(bingnegative) %>%
        group_by(book, chapter) %>%
        summarize(negativewords = n()) %>%
        left_join(wordcounts, by = c("book", "chapter")) %>%
        mutate(ratio = negativewords/words) %>%
        filter(chapter != 0) %>%
        top_n(1)

library(widyr)

pride_prejudice_words <- tidy_books %>%
        filter(book == "Pride & Prejudice")
word_cooccurences <- pride_prejudice_words %>%
        pairwise_count(linenumber, word, sort = TRUE)
word_cooccurences

library(igraph)
library(ggraph)

set.seed(1813)
word_cooccurences %>%
        filter(n >= 10) %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
        geom_node_point(color = "darkslategray4", size = 5) +
        geom_node_text(aes(label = name), vjust = 1.8) +
        ggtitle(expression(paste("Word Network in Jane Austen's ", 
                                 italic("Pride and Prejudice")))) +
        theme_void()

pride_prejudice_words <- tidy_books %>%
        filter(book == "Emma")
word_cooccurences <- pride_prejudice_words %>%
        pairwise_count(linenumber, word, sort = TRUE)
set.seed(2016)
word_cooccurences %>%
        filter(n >= 10) %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
        geom_node_point(color = "plum4", size = 5) +
        geom_node_text(aes(label = name), vjust = 1.8) +
        ggtitle(expression(paste("Word Network in Jane Austen's ", 
                                 italic("Emma")))) +
        theme_void()

#### TF-IDF ##################
# https://www.youtube.com/watch?v=-uVo0Xvmimw&t=847s

book_words <-  austen_books() %>%
        unnest_tokens(word, text) %>%
        count(book, word, sort = T) %>%
        ungroup()

total_words <- book_words %>%
        group_by(book) %>%
        summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

# plotting needs some fine tuning... (x-axis label, and many others)
ggplot(book_words, aes(book, word, fill = book)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        facet_wrap(~book, ncol = 2, scales = "free_x") +
        theme_minimal(base_size = 13) +
        labs(title = "Term Frequency Distribution in Jane Austen's Novels",
             y = "count", x = "n/total") +
        scale_fill_viridis(end = 0.75, discrete=TRUE, direction = -1) +
        scale_x_discrete(expand=c(0.02,0)) +
        theme(strip.text=element_text(hjust=0)) +
        theme(strip.text = element_text(face = "italic")) +
        theme(axis.title.x=element_blank()) +
        theme(axis.ticks.x=element_blank()) +
        theme(axis.text.x=element_blank())

book_words <- book_words %>%
        bind_tf_idf(word, book, n)
book_words

book_words %>%
        select(-total) %>%
        arrange(desc(tf_idf))

# insert plot here

