library(twitteR)
library(plyr)
library(stringr)
library(ggvis)
library(ggplot2)
library(memoise)
library(gridExtra)

options(shiny.trace=TRUE)
n_tweets <- 180
n_summary <- 10

consumerKey <- "xxxx"
consumerSecret <- "xxxx"
acessToken <- "xxxx"
accessTokenSecret <- "xxxx"
setup_twitter_oauth(consumerKey, consumerSecret, acessToken, accessTokenSecret)

shinyServer(function(input, output, session) {
  tryTolower = function(x){
    y = NA
    try_error = tryCatch(tolower(x), error = function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
  {
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
      sentence = gsub('[[:punct:]]', '', sentence)
      sentence = gsub('[[:cntrl:]]', '', sentence)
      sentence = gsub('\\d+', '', sentence)
      word.list = str_split(sentence, '\\s+')
      words = unlist(word.list)
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      score = sum(pos.matches) - sum(neg.matches)
      return(score)
    }, pos.words, neg.words, .progress=.progress )
    
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
  }
  
  cleanFun <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
  }
  

  get_source <- function(x){
    X <- cleanFun(x[["statusSource"]])
    X
  }
  
  tweets_df <- reactive({
    input$plot_feel
    isolate({
      withProgress({
        setProgress(message = "Processing sentiment......")
        if(input$lang=="All")
          tweets <- searchTwitter(input$source1, n=n_tweets)
        else
          tweets <- searchTwitter(input$source1, n=n_tweets, lang=input$lang)
        tweets <- strip_retweets(tweets, strip_manual=TRUE, strip_mt=TRUE)
        
        df <- twListToDF(tweets)
        df$Search <- input$source1
        
        if( (input$show_source2 == TRUE) && (input$source2 != ""))
        {
          if(input$lang=="All")
            tweets2 <- searchTwitter(input$source2, n=n_tweets)
          else
            tweets2 <- searchTwitter(input$source2, n=n_tweets, lang=input$lang)
        
          tweets2 <- strip_retweets(tweets2, strip_manual=TRUE, strip_mt=TRUE)
          df2 <- twListToDF(tweets2)
          df2$Search <- input$source2
          df <- rbind(df, df2)
          tweets <- c(tweets, tweets2)
        }
        
        df$Date <- format(df$created,'%m/%d/%Y %H:%I:%S')
        df$Source <-  apply(df, 1, get_source)
      
        sentences <- sapply(df$text, function(x) tryTolower(x))
        scores <- score.sentiment(sentences, pos.words, neg.words)
        df <- cbind(df, scores)
        df <- df[, c("id", "text", "Source", "Date", "Search", "created", "score")]
        names(df) <- c("id", "Post", "Source", "Date", "Search", "created", "score")
        df
      })
    })
  })
  
  output$plot <- renderPlot({
    df <- tweets_df()
    sources <- df$Source
    sources <- sapply(sources, function(x) ifelse(length(x) > 1, x[2], x[1]))
    source_table <- table(sources)
    s_t <- source_table[source_table > 10]
    pie(s_t, col = rainbow(length(s_t)))
  })
  
  output$trends <- renderPlot({
    df <- tweets_df()
    source1 <- df[df$Search==input$source1,]
    p1 <- ggplot(source1, aes(x=created, y=score)) + geom_point(shape=1, size=0)+geom_smooth(se=F)+labs(title=input$source1, x = "Date /Time", y = "Popularity") + ylim(-5, 5)
  
    if( (input$show_source2 == TRUE) && (input$source2 != ""))
    {
      source2 <- df[df$Search==input$source2,]
      p2 <- ggplot(source2, aes(x=created, y=score)) + geom_point(shape=1, size=0)+geom_smooth(se=F)+labs(title=input$source2, x = "Date /Time", y = "Popularity") + ylim(-5, 5)
      grid.arrange(p1, p2, nrow=1, ncol=2)
    }
    else
      print(p1)
  })

  
  output$twitter_view <- renderPrint({
    if( (input$show_source2 == TRUE) && (input$source2 != ""))
      cat(paste(input$source1, " vs. ", input$source2))
    else
      cat(input$source1)
  })
  
  
  output$view <- renderTable({
    df <- tweets_df()
    df <- df[df$Search==input$source1,]
    head(df, n = n_summary, addrownums=F)
  })
  
  
  
  output$vs_view <- renderTable({
    if( (input$show_source2 == TRUE) && (input$source2 != ""))
    {
      df <- tweets_df()
      df <- df[df$Search==input$source2,]
      head(df, n = n_summary, addrownums=F)
    }
  })
  
  movie_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$id)) return(NULL)
    all_tweets <- isolate(tweets_df())
    tweet <- all_tweets[all_tweets$id == x$id, ]
    paste0("<b>", tweet$Post, "</b><br><em><small>from ", tweet$Source, " (", tweet$Date, ")</small></em>")
  }
  
  vis2 <- reactive({
    df <- tweets_df()
    df[df$Search==input$source2,] %>%  ggvis(~created, ~score) %>% layer_points(fill = ~Search, key := ~id)  %>% layer_lines(stroke=~Search) %>% add_legend(c("fill", "stroke"), orient="left") %>% add_axis("x", title = "Date Time") %>% add_axis("y", title = "Popularity") %>% set_options(width = 800, height = 300) %>% add_tooltip(movie_tooltip, "click")
    if( (input$show_source2 != TRUE) || (input$source2 == "") )
      invisible()
  })
  
  vis <- reactive({
    legend_val <- c(input$source1)
    if( (input$show_source2 == TRUE) && (input$source2 != ""))
      legend_val <- c(input$source1, input$source2)
    df <- tweets_df()
    df %>%  ggvis(~created, ~score) %>% layer_points(fill = ~Search, key := ~id)  %>% layer_lines(stroke=~Search) %>% add_legend(c("fill", "stroke"), orient="left") %>% add_axis("x", title = "Date Time") %>% add_axis("y", title = "Popularity") %>% set_options(width = 800, height = 300) %>% add_tooltip(movie_tooltip, "click")
  })
  
  vis %>% bind_shiny("plot1")
})
