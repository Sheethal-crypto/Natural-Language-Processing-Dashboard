library(shiny)
shinyServer(function(input, output) {
    
    library(textreadr)
    library(stringr)
    library(tidytext)
    library(textreadr)
    library(textshape)
    library(scales)
    library(ggplot2)
    library(reshape2)
    library(tidyverse)
    library(topicmodels)
    library(dplyr)
    library(ggraph)
    library(igraph)
    library(quanteda)
    library(tidyr)
    library(wordcloud)
    
    output$bigramnet <- renderPlot({
        
        survey <- read_document(file="C:/Users/SHEETHAL/Desktop/Text analytics/text analytics_edited.docx")
        
        a <- 76 #how many observations to you have
        b <- 6 #how many variables do you have
        my_sur <- as.data.frame(matrix(nrow=a, ncol=b))
        
        #for loop for structuring the unstructured data
        for(z in 1:b){
            for(i in 1:a){
                my_sur[i,z]<- survey[i*b+z-b]
            }
        }
        
        df <- data.frame(my_sur$V1, my_sur$V3, person_id = rep(1:76, each=1), stringsAsFactors = FALSE)
        
        g1 <- df %>%
            filter(my_sur.V1 %in% (14:29))
        
        bigram_2dB_g1 <- g1 %>%
            unnest_tokens(bigram, my_sur.V3, token = "ngrams", n=2)
        
        bigram_2dB_g1_count <- bigram_2dB_g1 %>%
            count(bigram, sort = TRUE) 
        
        bigram_2dB_g1_separated <- bigram_2dB_g1_count %>%
            separate(bigram, c("word1", "word2"), sep = " ")
        
        bigram_2dB_g1_filtered <- bigram_2dB_g1_separated %>%
            filter(!word1 %in% stop_words$word) %>%
            filter(!word2 %in% stop_words$word)
        #filter(!word2 %in% cust_stop_2dB$word)
        
        bigram_2dB_g1_counts <- bigram_2dB_g1_filtered %>%
            unite(bigram, word1, word2, sep=' ')
        
        bigram_2dB_g1_graph <- bigram_2dB_g1_counts %>%
            #filter(n<30) %>%
            #filter(n>1) %>%
            graph_from_data_frame()
        #####################
        g2 <- df %>%
            filter(my_sur.V1 %in% (30:40))
        
        bigram_2dB_g2 <- g2 %>%
            unnest_tokens(bigram, my_sur.V3, token = "ngrams", n=2)
        
        bigram_2dB_g2_count <- bigram_2dB_g2 %>%
            count(bigram, sort = TRUE) 
        
        bigram_2dB_g2_separated <- bigram_2dB_g2_count %>%
            separate(bigram, c("word1", "word2"), sep = " ")
        
        bigram_2dB_g2_filtered <- bigram_2dB_g2_separated %>%
            filter(!word1 %in% stop_words$word) %>%
            filter(!word2 %in% stop_words$word)
        #filter(!word2 %in% cust_stop_2dB$word)
        
        bigram_2dB_g2_counts <- bigram_2dB_g2_filtered %>%
            unite(bigram, word1, word2, sep=' ')
        
        bigram_2dB_g2_graph <- bigram_2dB_g2_counts %>%
            #filter(n<30) %>%
            #filter(n>1) %>%
            graph_from_data_frame()
        #############################
        g3 <- df %>%
            filter(my_sur.V1 %in% (41:70))
        
        cust_stop_2dB <- data_frame(word=c("NA"),
                                    lexicon=c("x"))
        
        bigram_2dB_g3 <- g3 %>%
            unnest_tokens(bigram, my_sur.V3, token = "ngrams", n=2)
        
        bigram_2dB_g3_count <- bigram_2dB_g3 %>%
            count(bigram, sort = TRUE) 
        
        bigram_2dB_g3_separated <- bigram_2dB_g3_count %>%
            separate(bigram, c("word1", "word2"), sep = " ")
        
        bigram_2dB_g3_filtered <- bigram_2dB_g3_separated %>%
            filter(!word1 %in% stop_words$word) %>%
            filter(!word2 %in% stop_words$word) %>%
            filter(!word2 %in% cust_stop_2dB$word)
        
        bigram_2dB_g3_counts <- bigram_2dB_g3_filtered %>%
            unite(bigram, word1, word2, sep=' ')
        
        bigram_2dB_g3_graph <- bigram_2dB_g3_counts %>%
            #filter(n<30) %>%
            #filter(n>1) %>%
            graph_from_data_frame()
        
        if (input$Choice=="< 30"){
            
            ggraph(bigram_2dB_g1_graph, layout = "fr") +
                geom_edge_link(edge_colour="Cyan4")+
                geom_node_point()+
                geom_node_text(aes(label=name), repel=TRUE, vjust =1, hjust=1)
        }
        
        else if(input$Choice=="30-40"){
            
            ggraph(bigram_2dB_g2_graph, layout = "fr") +
                geom_edge_link(edge_colour="Cyan4")+
                geom_node_point()+
                geom_node_text(aes(label=name), repel=TRUE, vjust =1, hjust=1)
        }
        else if(input$Choice==">40"){
            
            ggraph(bigram_2dB_g3_graph, layout = "fr") +
                geom_edge_link(edge_colour="Cyan4")+
                geom_node_point()+
                geom_node_text(aes(label=name), repel=TRUE, vjust =1, hjust=1)
        }
        
        
    })   
    output$tfidf <- renderPlot({
        survey <- read_document(file="C:/Users/SHEETHAL/Desktop/Text analytics/text analytics_edited.docx")
        
        a <- 75 #how many observations to you have
        b <- 6 #how many variables do you have
        my_surtf <- as.data.frame(matrix(nrow=a, ncol=b))
        
        #for loop for structuring the unstructured data
        for(z in 1:b){
            for(i in 1:a){
                my_surtf[i,z]<- survey[i*b+z-b]
            }#closing z loop
        }#closing i loop
        
        ##################################################################
        
        my_txt <- my_surtf$V1
        
        mysurtf1 <- data_frame(line=1:a, text=my_txt)
        #print(mysur1)
        
        cust_stoptf1 <- data_frame(word=c("i'm","soeven","na"),
                                   lexicon=c("l","l","l"))
        data(stop_words)
        frequencies_tokens_nostop1 <- mysurtf1 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>% 
            anti_join(cust_stoptf1) %>%
            count(word, sort=TRUE)
        
        #print(frequencies_tokens_nostop1)
        
        ## Age range of the participants is 25 to 31, with most people
        #of the age 25 & 26yrs
        
        ##################################################################
        
        my_txt <- my_surtf$V2
        
        mysurtf2 <- data_frame(line=1:a, text=my_txt)
        
        cust_stoptf2 <- data_frame(word=c("i'm","don't","you've"),
                                   lexicon=c("l","l","l"))
        data(stop_words)
        frequencies_tokens_nostop2 <- mysurtf2 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>% 
            anti_join(cust_stoptf2) %>%
            count(word, sort=TRUE)
        
        ##From the frequent words, we conclude that most people are/striving to
        #have healthy life.  Since "dont" appears only twice we discard the 
        #alternate fact of not having healthy lifestyle
        
        ###################################################################
        
        my_txt <- my_surtf$V3
        
        mysurtf3 <- data_frame(line=1:a, text=my_txt)
        
        cust_stoptf3<- data_frame(word=c("don't","i'm"),
                                  lexicon=c("l","l"))
        data(stop_words)
        frequencies_tokens_nostop3 <- mysurtf3 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>% #here's where we remove tokens
            anti_join(cust_stoptf3) %>% 
            count(word, sort=TRUE)
        
        ###################################################################
        
        my_txt <- my_surtf$V4
        
        cust_stoptf4<- data_frame(word=c("it's","i'm"),
                                  lexicon=c("l","l"))
        
        mysurtf4 <- data_frame(line=1:a, text=my_txt)
        #print(mysur4)
        
        data(stop_words)
        frequencies_tokens_nostop4 <- mysurtf4 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>% #here's where we remove tokens
            anti_join(cust_stoptf4) %>%
            count(word, sort=TRUE)
        
        ###################################################################
        
        my_txt <- my_surtf$V5
        
        cust_stoptf5 <- data_frame(word=c("it's","i'm"),
                                   lexicon=c("l","l"))
        
        mysurtf5 <- data_frame(line=1:a, text=my_txt)
        
        data(stop_words)
        frequencies_tokens_nostop5 <- mysurtf5 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>% #here's where we remove tokens
            anti_join(cust_stoptf5) %>% 
            count(word, sort=TRUE)
        
        ##################################################################
        
        my_txt <- my_surtf$V6
        
        mysurtf6 <- data_frame(line=1:a, text=my_txt)
        
        cust_stoptf6 <- data_frame(word=c("na"),
                                   lexicon=c("l"))
        data(stop_words)
        frequencies_tokens_nostop6 <- mysurtf6 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>% #here's where we remove tokens
            anti_join(cust_stoptf6) %>%
            count(word, sort=TRUE)
        
        ##################################################################
        my_surtf_final <- bind_rows(mutate(frequencies_tokens_nostop1, question ="Age"),
                                    mutate(frequencies_tokens_nostop2, question ="Vegetarian"),
                                    mutate(frequencies_tokens_nostop3, question ="Lifestyle"),
                                    mutate(frequencies_tokens_nostop4, question ="Diet"),
                                    mutate(frequencies_tokens_nostop5, question ="Greenhouse_gases"),
                                    mutate(frequencies_tokens_nostop6, question ="Meals"))
        
        my_surtf_tfidf <- my_surtf_final %>%
            bind_tf_idf(word, question, n)%>%
            arrange(desc(tf_idf))
        
        my_surtf_tfidf%>%
            arrange(desc(tf_idf)) %>%
            mutate(word=factor(word, levels=rev(unique(word)))) %>%
            group_by(question) %>%
            top_n(5) %>%
            ungroup %>%
            ggplot(aes(word, tf_idf, fill=question))+
            geom_col(show.legend=FALSE)+
            labs(x=NULL, y="tf-idf")+
            facet_wrap(~question, ncol=2, scales="free")+
            coord_flip()
    })
    output$vegYorN <- renderPlot({
        
        if (input$Decision=="Yes"){
            survey <- read_document(file="C:/Users/SHEETHAL/Desktop/Text analytics/yes.docx")
            
            a <- 30 #how many observations to you have
            b <- 6 #how many variables do you have
            my_sur <- as.data.frame(matrix(nrow=a, ncol=b))
            
            #for loop for structuring the unstructured data
            for(z in 1:b){
                for(i in 1:a){
                    my_sur[i,z]<- survey[i*b+z-b]
                }#closing z loop
            }#closing i loop
            
            ##################################################################
            
            my_txt <- my_sur$V1
            
            mysur1 <- data_frame(line=1:a, text=my_txt)
            #print(mysur1)
            
            cust_stop1 <- data_frame(word=c("i'm","soeven","na"),
                                     lexicon=c("l","l","l"))
            data(stop_words)
            frequencies_tokens_nostop1 <- mysur1 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% 
                anti_join(cust_stop1) %>%
                count(word, sort=TRUE)
            
            #print(frequencies_tokens_nostop1)
            
            ## Age range of the participants is 25 to 31, with most people
            #of the age 25 & 26yrs
            
            ##################################################################
            
            my_txt <- my_sur$V2
            
            mysur2 <- data_frame(line=1:a, text=my_txt)
            
            cust_stop2 <- data_frame(word=c("i'm","don't","you've"),
                                     lexicon=c("l","l","l"))
            data(stop_words)
            frequencies_tokens_nostop2 <- mysur2 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% 
                anti_join(cust_stop2) %>%
                count(word, sort=TRUE)
            
            ##From the frequent words, we conclude that most people are/striving to
            #have healthy life.  Since "dont" appears only twice we discard the 
            #alternate fact of not having healthy lifestyle
            
            ###################################################################
            
            my_txt <- my_sur$V3
            
            mysur3 <- data_frame(line=1:a, text=my_txt)
            
            cust_stop3<- data_frame(word=c("don't","i'm"),
                                    lexicon=c("l","l"))
            data(stop_words)
            frequencies_tokens_nostop3 <- mysur3 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% #here's where we remove tokens
                anti_join(cust_stop3) %>% 
                count(word, sort=TRUE)
            
            ###################################################################
            
            my_txt <- my_sur$V4
            
            cust_stop4<- data_frame(word=c("it's","i'm"),
                                    lexicon=c("l","l"))
            
            mysur4 <- data_frame(line=1:a, text=my_txt)
            #print(mysur4)
            
            data(stop_words)
            frequencies_tokens_nostop4 <- mysur4 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% #here's where we remove tokens
                anti_join(cust_stop4) %>%
                count(word, sort=TRUE)
            
            ###################################################################
            
            my_txt <- my_sur$V5
            
            cust_stop5 <- data_frame(word=c("it's","i'm"),
                                     lexicon=c("l","l"))
            
            mysur5 <- data_frame(line=1:a, text=my_txt)
            
            data(stop_words)
            frequencies_tokens_nostop5 <- mysur5 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% #here's where we remove tokens
                anti_join(cust_stop5) %>% 
                count(word, sort=TRUE)
            
            ##################################################################
            
            my_txt <- my_sur$V6
            
            mysur6 <- data_frame(line=1:a, text=my_txt)
            
            cust_stop6 <- data_frame(word=c("na"),
                                     lexicon=c("l"))
            data(stop_words)
            frequencies_tokens_nostop6 <- mysur6 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% #here's where we remove tokens
                anti_join(cust_stop6) %>%
                count(word, sort=TRUE)
            
            ##################################################################
            my_sur_final <- bind_rows(mutate(frequencies_tokens_nostop1, question ="Age"),
                                      mutate(frequencies_tokens_nostop2, question ="Vegetarian"),
                                      mutate(frequencies_tokens_nostop3, question ="Lifestyle"),
                                      mutate(frequencies_tokens_nostop4, question ="Diet"),
                                      mutate(frequencies_tokens_nostop5, question ="Greenhouse_gases"),
                                      mutate(frequencies_tokens_nostop6, question ="Meals"))
            
            my_sur_woq1 <- bind_rows(mutate(frequencies_tokens_nostop2, question ="Vegetarian"),
                                     mutate(frequencies_tokens_nostop3, question ="Lifestyle"),
                                     mutate(frequencies_tokens_nostop4, question ="Diet"),
                                     mutate(frequencies_tokens_nostop5, question ="Greenhouse_gases"),
                                     mutate(frequencies_tokens_nostop6, question ="Meals"))
            
            
            ##################################################################################
            
            ##### Sentiment Graph ###################
            
            Q1_bing <- mysur1 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            Q2_bing <- mysur2 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            Q3_bing <- mysur3 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            Q4_bing <- mysur4 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            Q5_bing <- mysur5 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            Q6_bing <- mysur6 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            Q6_bing_1 <<- Q6_bing
            
            Q5_bing %>%
                group_by(sentiment) %>%
                top_n(10) %>%
                ungroup() %>%
                mutate(word=reorder(word, n)) %>%  
                ggplot(aes(word, n, fill=sentiment)) +   geom_col(show.legend = FALSE) +facet_wrap(~sentiment, scales = "free_y")+ 
                labs(y="People's take on greenhouse gases", x=NULL)+coord_flip()
            
            
        }
        else if (input$Decision=="No"){
            
            survey <- read_document(file="C:/Users/SHEETHAL/Desktop/Text analytics/no.docx")
            
            a <- 46 #how many observations to you have
            b <- 6 #how many variables do you have
            my_sur <- as.data.frame(matrix(nrow=a, ncol=b))
            
            #for loop for structuring the unstructured data
            for(z in 1:b){
                for(i in 1:a){
                    my_sur[i,z]<- survey[i*b+z-b]
                }#closing z loop
            }#closing i loop
            
            ##################################################################
            
            my_txt <- my_sur$V1
            
            mysur1 <- data_frame(line=1:a, text=my_txt)
            #print(mysur1)
            
            cust_stop1 <- data_frame(word=c("i'm","soeven","na"),
                                     lexicon=c("l","l","l"))
            data(stop_words)
            frequencies_tokens_nostop1 <- mysur1 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% 
                anti_join(cust_stop1) %>%
                count(word, sort=TRUE)
            
            #print(frequencies_tokens_nostop1)
            
            ## Age range of the participants is 25 to 31, with most people
            #of the age 25 & 26yrs
            
            ##################################################################
            
            my_txt <- my_sur$V2
            
            mysur2 <- data_frame(line=1:a, text=my_txt)
            
            cust_stop2 <- data_frame(word=c("i'm","don't","you've"),
                                     lexicon=c("l","l","l"))
            data(stop_words)
            frequencies_tokens_nostop2 <- mysur2 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% 
                anti_join(cust_stop2) %>%
                count(word, sort=TRUE)
            
            ##From the frequent words, we conclude that most people are/striving to
            #have healthy life.  Since "dont" appears only twice we discard the 
            #alternate fact of not having healthy lifestyle
            
            ###################################################################
            
            my_txt <- my_sur$V3
            
            mysur3 <- data_frame(line=1:a, text=my_txt)
            
            cust_stop3<- data_frame(word=c("don't","i'm"),
                                    lexicon=c("l","l"))
            data(stop_words)
            frequencies_tokens_nostop3 <- mysur3 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% #here's where we remove tokens
                anti_join(cust_stop3) %>% 
                count(word, sort=TRUE)
            
            ###################################################################
            
            my_txt <- my_sur$V4
            
            cust_stop4<- data_frame(word=c("it's","i'm"),
                                    lexicon=c("l","l"))
            
            mysur4 <- data_frame(line=1:a, text=my_txt)
            #print(mysur4)
            
            data(stop_words)
            frequencies_tokens_nostop4 <- mysur4 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% #here's where we remove tokens
                anti_join(cust_stop4) %>%
                count(word, sort=TRUE)
            
            ###################################################################
            
            my_txt <- my_sur$V5
            
            cust_stop5 <- data_frame(word=c("it's","i'm"),
                                     lexicon=c("l","l"))
            
            mysur5 <- data_frame(line=1:a, text=my_txt)
            
            data(stop_words)
            frequencies_tokens_nostop5 <- mysur5 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% #here's where we remove tokens
                anti_join(cust_stop5) %>% 
                count(word, sort=TRUE)
            
            ##################################################################
            
            my_txt <- my_sur$V6
            
            mysur6 <- data_frame(line=1:a, text=my_txt)
            
            cust_stop6 <- data_frame(word=c("na"),
                                     lexicon=c("l"))
            data(stop_words)
            frequencies_tokens_nostop6 <- mysur6 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% #here's where we remove tokens
                anti_join(cust_stop6) %>%
                count(word, sort=TRUE)
            
            ##################################################################
            my_sur_final <- bind_rows(mutate(frequencies_tokens_nostop1, question ="Age"),
                                      mutate(frequencies_tokens_nostop2, question ="Vegetarian"),
                                      mutate(frequencies_tokens_nostop3, question ="Lifestyle"),
                                      mutate(frequencies_tokens_nostop4, question ="Diet"),
                                      mutate(frequencies_tokens_nostop5, question ="Greenhouse_gases"),
                                      mutate(frequencies_tokens_nostop6, question ="Meals"))
            
            my_sur_woq1 <- bind_rows(mutate(frequencies_tokens_nostop2, question ="Vegetarian"),
                                     mutate(frequencies_tokens_nostop3, question ="Lifestyle"),
                                     mutate(frequencies_tokens_nostop4, question ="Diet"),
                                     mutate(frequencies_tokens_nostop5, question ="Greenhouse_gases"),
                                     mutate(frequencies_tokens_nostop6, question ="Meals"))
            
            
            ##################################################################################
            
            ##### Sentiment Graph ###################
            
            Q1_bing <- mysur1 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            Q2_bing <- mysur2 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            Q3_bing <- mysur3 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            Q4_bing <- mysur4 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            Q5_bing <- mysur5 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            Q6_bing <- mysur6 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            Q3_nrc <- mysur3 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("nrc")) %>%
                count(word, sentiment, sort=TRUE) %>% 
                acast(word ~sentiment, value.var="n", fill=0) %>%
                comparison.cloud(colors =  c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown","#cc0096","#cc7000"),
                                 max.words=200,
                                 scale=c(1.0,1.0), 
                                 fixed.asp=TRUE,
                                 title.size=1
                )
            
            Q4_nrc <- mysur4 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("nrc")) %>%
                count(word, sentiment, sort=TRUE) %>% 
                acast(word ~sentiment, value.var="n", fill=0) %>%
                comparison.cloud(colors =  c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown","#cc0096","#cc7000"),
                                 max.words=200,
                                 scale=c(1.0,1.0), 
                                 fixed.asp=TRUE,
                                 title.size=1
                )               
            
            Q5_nrc <- mysur5 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("nrc")) %>%
                count(word, sentiment, sort=TRUE) %>% 
                acast(word ~sentiment, value.var="n", fill=0) %>%
                comparison.cloud(colors =  c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown","#cc0096","#cc7000"),
                                 max.words=200,
                                 scale=c(1.0,1.0), 
                                 fixed.asp=TRUE,
                                 title.size=1
                )
            
            Q6_nrc <- mysur6 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("nrc")) %>%
                count(word, sentiment, sort=TRUE) %>% 
                acast(word ~sentiment, value.var="n", fill=0) %>%
                comparison.cloud(colors =  c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown","#cc0096","#cc7000"),
                                 max.words=200,
                                 scale=c(1.0,1.0), 
                                 fixed.asp=TRUE,
                                 title.size=1
                )
            
            Q6_bing_1 <<- Q6_bing
            
            Q5_bing %>%
                group_by(sentiment) %>%
                top_n(10) %>%
                ungroup() %>%
                mutate(word=reorder(word, n)) %>%  
                ggplot(aes(word, n, fill=sentiment)) +   geom_col(show.legend = FALSE) +facet_wrap(~sentiment, scales = "free_y")+ 
                labs(y="People's take on greenhouse gases", x=NULL)+coord_flip()
            
            
            
            
        }
    })
    output$QQ6 <- renderPlot({
        
        if (input$Decision=="Yes"){
            
            Q6_bing_1 %>%
                group_by(sentiment) %>%
                top_n(10) %>%
                ungroup() %>%
                mutate(word=reorder(word, n)) %>%  
                ggplot(aes(word, n, fill=sentiment)) +   geom_col(show.legend = FALSE) +facet_wrap(~sentiment, scales = "free_y")+ 
                labs(y="Description of everyday meal", x=NULL)+coord_flip()
            
            
            
        }
        
        else if (input$Decision=="No"){
            Q6_bing_1 %>%
                group_by(sentiment) %>%
                top_n(10) %>%
                ungroup() %>%
                mutate(word=reorder(word, n)) %>%  
                ggplot(aes(word, n, fill=sentiment)) +   geom_col(show.legend = FALSE) +facet_wrap(~sentiment, scales = "free_y")+ 
                labs(y="Description of everyday meal", x=NULL)+coord_flip()
            
        }
    })
    
    output$nv <- renderPrint({#Naive Bayes Model
        s <- read_document(file="C:/Users/SHEETHAL/Desktop/Text analytics/text analytics_edited.docx")
        
        df_nv <- data.frame(text = s, person_id = rep(1:76, each=6), stringsAsFactors = FALSE) %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            count(person_id, word) %>%
            cast_dfm(person_id, word, n)
        
        my_df_nv <- dfm_trim(df_nv, min_term =
                                 2, min_docfreq = 1)
        msg.dfm <- dfm_weight(my_df_nv)
        
        msg.dfm
        #let's split the docs into training and testing data
        msg.dfm.train<-msg.dfm[1:64,]
        msg.dfm.test<-msg.dfm[65:76,]
        
        #building the Naive Bayes model:
        NB_classifier <- textmodel_nb(msg.dfm.train, c(0,0,0,0,0,0,0,1,0,1,0,1,1,0,0,0,1,0,0,0,1,1,1,0,1,1,0,0,0,1,0,1,0,0,1,0,0,1,0,1,0,0,0,1,0,0,1,0,0,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0))
        NB_classifier
        summary(NB_classifier)
        # predicting the testing data
        pred <- predict(NB_classifier, msg.dfm.test)
        print(pred)
        
        perc <- (10/12)*100
        print(perc)
    })
})