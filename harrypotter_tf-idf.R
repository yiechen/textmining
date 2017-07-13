# enconding: UTF-8
# Julia Silge´s Jane Austen Text Mining adaptation for Harry Potter
# created by YieChen, from:
#    https://github.com/juliasilge/old_bloggy_blog/blob/master/_R/2016-04-29-Life-Changing-Magic.Rmd
#       https://github.com/bradleyboehmke/harrypotter

library(harrypotter)
library(tidytext)
library(dplyr)
library(stringr)

harry_potter <- function(){
        books <- list(
                "Philosophers Stone" = harrypotter::philosophers_stone,
                "Chamber of Secrets" = harrypotter::chamber_of_secrets,
                "Prisoner of Azkaban" = harrypotter::prisoner_of_azkaban,
                "Goblet of Fire" = harrypotter::goblet_of_fire,
                "Order of the Phoenix" = harrypotter::order_of_the_phoenix,
                "Half-Blood Prince" = harrypotter::half_blood_prince,
                "Deathly Hallows" = harrypotter::deathly_hallows
        )
        ret <- data.frame(text = unlist(books, use.names = FALSE), 
                          stringsAsFactors = FALSE)
        ret$book <- factor(rep(names(books), sapply(books, length)))
        ret$book <- factor(ret$book, levels = unique(ret$book))
        structure(ret, class = c("tbl_df", "tbl", "data.frame"))
}

harry_potter()

original_books <- harry_potter() %>%
        group_by(book) %>%
        mutate(chapter = row_number() #,
               #chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                       #ignore_case = TRUE)))
               ) %>%
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

harrypottersentiment <- tidy_books %>%
        inner_join(bing) %>% 
        count(book, index = chapter %/% 1, sentiment) %>% 
        spread(sentiment, n, fill = 0) %>% 
        mutate(sentiment = positive - negative)

harrypottersentiment

library(ggplot2)
library(viridis)
ggplot(harrypottersentiment, aes(index, sentiment, fill = book)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        facet_wrap(~book, ncol = 2, scales = "free_x") +
        theme_minimal(base_size = 13) +
        labs(title = "Sentiment in Harry Potter Saga",
             y = "Sentiment") +
        scale_fill_viridis(end = 0.75, discrete=TRUE, direction = -1) +
        scale_x_discrete(expand=c(0.02,0)) +
        theme(strip.text=element_text(hjust=0)) +
        theme(strip.text = element_text(face = "italic")) +
        theme(axis.title.x=element_blank()) +
        theme(axis.ticks.x=element_blank()) +
        theme(axis.text.x=element_blank())



hp_sentences <- harry_potter() %>% 
        group_by(book) %>% 
        unnest_tokens(sentence, text, token = "sentences") %>% 
        ungroup()

hp_sentences$sentence[39]


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

p_stone_words <- tidy_books %>%
        filter(book == "Philosophers Stone")
word_cooccurences <- p_stone_words %>%
        pairwise_count(chapter, word, sort = TRUE)
word_cooccurences

library(igraph)
library(ggraph)

set.seed(1813)
word_cooccurences %>%
        filter(n >= 100) %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
        geom_node_point(color = "darkslategray4", size = 5) +
        geom_node_text(aes(label = name), vjust = 1.8) +
        ggtitle(expression(paste("Word Network in J.K. Rowling's ", 
                                 italic("Philosophers Stone")))) +
        theme_void()


dh_words <- tidy_books %>%
        filter(book == "Deathly Hallows")
word_cooccurences <- dh_words %>%
        pairwise_count(chapter, word, sort = TRUE)
set.seed(2016)
word_cooccurences %>%
        filter(n >= 400) %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
        geom_node_point(color = "darkslategray4", size = 5) +
        geom_node_text(aes(label = name), vjust = 1.8) +
        ggtitle(expression(paste("Word Network in J.K. Rowling's ", 
                                 italic("Deathly Hallows")))) +
        theme_void()



####################################################################################################
# # scraps from initial commitment
# 
# # loading book
# dh <- deathly_hallows
# 
# # converting each book into unnested iso-words, by chapter of the book (deathly hallows has 37 chapters)
# text_dh <- data_frame(chapter = 1:37, text = dh) %>%
#         unnest_tokens(word, text)
# 
# tidy_dh <- text_dh %>%
#         anti_join(stop_words)
# 
# # look at word count frequencies
# tidy_dh %>%
#         count(word, sort=T)
# 
# # loading sentiment lexicon
# library(tidyr)
# bing <- sentiments %>%
#         filter(lexicon == "bing") %>%
#         select(-score)
# 
# bing
# 
# #
# 
# 
# dhsentiment <- tidy_dh %>%
#         inner_join(bing) %>% 
#         count(chapter, index = chapter %/% 80, sentiment) %>% 
#         spread(sentiment, n, fill = 0) %>% 
#         mutate(sentiment = positive - negative)
# 
# dhsentiment

chunk_into_sentences <- function(text) {
        break_points <- c(1, as.numeric(gregexpr('[[:alnum:] ][.!?]', text)[[1]]) + 1)
        sentences <- NULL
        for(i in 1:length(break_points)) {
                res <- substr(text, break_points[i], break_points[i+1]) 
                if(i>1) { sentences[i] <- sub('. ', '', res) } else { sentences[i] <- res }
        }
        sentences <- sentences[sentences=!is.na(sentences)]
        return(sentences)
}

dt <- philosophers_stone %>%
        chunk_into_sentences %>%
        as.data.frame
