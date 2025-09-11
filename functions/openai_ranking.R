rank_articles_with_openai <- function(articles_df, top_n = 10) {
  if (nrow(articles_df) <= top_n) {
    return(articles_df)
  }
  
  # Create prompt
  articles_text <- ""
  for (i in 1:nrow(articles_df)) {
    articles_text <- paste0(articles_text, 
                           "Article ", i, ":\n",
                           "Type: ", articles_df$type[i], "\n",
                           "Title: ", articles_df$title[i], "\n",
                           "Source: ", articles_df$source[i], "\n",
                           "Description: ", substr(articles_df$description[i], 1, 300), "\n\n")
  }
  
  prompt <- paste0(
    "You are helping curate content for a Jewish studies research feed. ",
    "The goal is to highlight the most important and relevant academic research and news ",
    "for scholars, students, and practitioners in Jewish studies. ",
    "This includes topics like Jewish history, religion, culture, contemporary Jewish life, ",
    "antisemitism, Israel studies, Holocaust studies, Jewish literature, and Jewish communities worldwide.\n\n",
    "Please rank the following articles by relevance and importance to Jewish studies. ",
    "Select the top ", top_n, " articles. Prioritize:\n",
    "1. Academic articles with significant scholarly contributions\n",
    "2. News articles about major developments affecting Jewish communities\n",
    "3. Content that would be valuable for Jewish studies researchers\n\n",
    "Articles to rank:\n", articles_text, "\n",
    "Please respond with only the numbers of the top ", top_n, " articles, ",
    "separated by commas (e.g., '1,3,5,7,9,2,4,6,8,10'):"
  )
  
  # Make OpenAI API call
  tryCatch({
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers("Authorization" = paste("Bearer", openai_apikey)),
      content_type_json(),
      body = list(
        model = "gpt-3.5-turbo",
        messages = list(list(
          role = "user", 
          content = prompt
        )),
        max_tokens = 100,
        temperature = 0.1
      ),
      encode = "json"
    )
    
    if (status_code(response) == 200) {
      result <- content(response)
      rankings <- result$choices[[1]]$message$content
      
      # Parse the rankings
      selected_indices <- as.numeric(unlist(strsplit(gsub("[^0-9,]", "", rankings), ",")))
      selected_indices <- selected_indices[!is.na(selected_indices)]
      selected_indices <- selected_indices[selected_indices <= nrow(articles_df)]
      
      if (length(selected_indices) > 0) {
        return(articles_df[selected_indices[1:min(top_n, length(selected_indices))], ])
      }
    }
    
    cat("OpenAI ranking failed, using all articles\n")
    return(articles_df)
    
  }, error = function(e) {
    cat("Error with OpenAI:", e$message, "\n")
    return(articles_df)
  })
}
