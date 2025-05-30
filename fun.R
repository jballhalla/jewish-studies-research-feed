# Main 
########

retrieve_crossref_issn_data <- function(issn_list, start_date, end_date, verbose=FALSE, polite_endpoint=TRUE){
    K <- length(issn_list)
    out <- list()

    for(i in 1:K){
        issn <- issn_list[i]
        if(verbose) cat(issn, "\n")
        j <- 0
        tmp <- list()
        for(type in c("created", "published")){
            j <- j + 1
            tmp[[j]] <- call_crossref_api(
                id=issn, 
                type="issn", 
                start=start_date, 
                end=end_date, 
                date_type=type, 
                polite_endpoint=polite_endpoint)
        }
        tmp_articles <- rbind(
                get_crossref_articles(tmp[[1]]), 
                get_crossref_articles(tmp[[2]])
            )
        if(!is.null(tmp_articles)) {
            tmp_articles$issn <- issn
            out[[i]] <- tmp_articles[!duplicated(tmp_articles$url),]
        }
    }

    # Filter out NULL entries
    out <- out[!sapply(out, is.null)]
    
    if(length(out) == 0) return(NULL)

    out <- do.call(rbind, out)
    return(out)
}


# Filter 
#########

add_multidisciplinary_filter <- function(row){
    row_nam <- names(row)
        cat(row[row_nam=="url"], "\n")
    row[row_nam=="filter"] <- as.integer(row[row_nam=="filter"])
    if(row[row_nam=="filter"]!=0) return(row[row_nam=="filter"])
    else{
        res <- call_openai_api(
            system_prompt=prompt_socsci_classifier, 
            user_prompt=paste(
                "Journal Name:", row[row_nam=="journal_full"], "\n",
                "Title:", row[row_nam=="title"], "\n",
                row[row_nam=="abstract"]
            ),
            model="gpt-4o-mini")
        if( get_openai_finish_reason(res)!="stop" ) return(-1)
        if( tolower(get_openai_response(res))=="no" ) return(2)
        return(0)
    }
}

dispatch_special_filter <- function(data){
    FUN <- unique(data$filter_function)
    if(FUN=="" | FUN=="none") return(data)
    else {
        filter_fun <- match.fun(FUN)
        return(filter_fun(data))
    }
    }

add_science_filter <- function(data){
    flag <- as.numeric(is.na(data$abstract)) 
    flag <- ifelse(flag==0, as.numeric(nchar(data$abstract)<200), flag) 
    data$filter <- flag*3
    return(data)
    }

add_nature_filter <- function(data){
    data$filter <- as.numeric(!grepl("/s", data$url))*4
    return(data)
    }

add_standard_filter <- function(data){
    str <- data$title
    flag <- rep(0, length(str))
    flag <- ifelse(is.na(str),1, flag) # ToCs have no title 
    flag <- ifelse(str=="Editorial Board",1, flag)
    flag <- ifelse(str=="Issue Information",1, flag)
    flag <- ifelse(str=="Forthcoming Papers",1, flag)
    flag <- ifelse(grepl("ERRATUM|ERRATA|Frontmatter|Front matter|Backmatter|Back matter", str, ignore.case = TRUE),1, flag)
    data$filter <- flag
    return(data)
    }


# Helpers 
##########

# Add this debug function to fun.R
debug_nul_characters <- function(obj, name = "object") {
    if (is.data.frame(obj)) {
        for (col_name in names(obj)) {
            col_data <- obj[[col_name]]
            if (is.character(col_data)) {
                nul_indices <- which(grepl("\u0000", col_data, fixed = TRUE))
                if (length(nul_indices) > 0) {
                    cat("Found NUL in", name, "column", col_name, "at rows:", nul_indices, "\n")
                    cat("Sample problematic data:", substr(col_data[nul_indices[1]], 1, 100), "\n")
                }
            }
        }
    } else if (is.list(obj)) {
        for (i in seq_along(obj)) {
            debug_nul_characters(obj[[i]], paste0(name, "[[", i, "]]"))
        }
    } else if (is.character(obj)) {
        nul_indices <- which(grepl("\u0000", obj, fixed = TRUE))
        if (length(nul_indices) > 0) {
            cat("Found NUL in", name, "at positions:", nul_indices, "\n")
        }
    }
}

# Replace the render_json function with this more robust version:
render_json <- function(df, date) {
    # Ultra-aggressive character cleaning
    ultra_clean <- function(x) {
        if (is.null(x) || length(x) == 0) return(x)
        if (is.character(x)) {
            # Convert to raw bytes and back to remove any problematic characters
            x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
            # Remove ALL control characters including NUL
            x <- gsub("[\u0000-\u001F\u007F-\u009F]", "", x, perl = TRUE)
            # Additional NUL removal with different approaches
            x <- gsub("\0", "", x, fixed = TRUE)
            x <- gsub("\\x00", "", x)
            # Ensure only printable characters remain
            x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
            x <- iconv(x, from = "ASCII", to = "UTF-8", sub = "")
            return(x)
        }
        return(x)
    }
    
    cat("Starting render_json with", nrow(df), "rows\n")
    
    # Debug: Check for NUL characters before processing
    debug_nul_characters(df, "input_dataframe")
    
    # Apply ultra cleaning to all character columns
    char_cols <- sapply(df, is.character)
    df[char_cols] <- lapply(df[char_cols], ultra_clean)
    
    # Debug: Check again after cleaning
    debug_nul_characters(df, "cleaned_dataframe")
    
    # Split by journal
    df_split <- split(df, df$journal_full)
    to_json <- list()
    
    for(i in 1:length(df_split)){
        articles <- df_split[[i]]
        journal_full <- ultra_clean(unique(articles$journal_full))
        journal_short <- ultra_clean(unique(articles$journal_short))
        
        # Select and clean specific columns
        articles <- articles[c("title", "authors", "abstract", "url", "doi", "filter")]
        articles[] <- lapply(articles, ultra_clean)
        
        # Debug: Check articles data
        debug_nul_characters(articles, paste0("articles_", i))
        
        articles_hidden <- subset(articles, !(filter==0 | filter==-1))
        articles_visible <- subset(articles, (filter==0 | filter==-1))
        
        to_json[[i]] <- list(
            "journal_full" = journal_full, 
            "journal_short" = journal_short,
            "articles" = articles_visible, 
            "articles_hidden" = articles_hidden
        )
    }
    
    final_obj <- list("update" = as.character(date), "content" = to_json)
    
    # Debug: Final check
    debug_nul_characters(final_obj, "final_json_object")

    # In render_json function, replace the toJSON call with:
    tryCatch({
        # Convert to JSON using base R to avoid jsonlite issues
        json_result <- jsonlite::toJSON(final_obj, pretty = TRUE, auto_unbox = TRUE, null = "null")
        # Final NUL strip on the JSON string itself
        json_result <- gsub("\\x00", "", json_result, fixed = TRUE)
        json_result <- gsub("\u0000", "", json_result, fixed = TRUE)
        Encoding(json_result) <- "UTF-8"
        return(json_result)
    }, error = function(e) {
        # Serialize each piece individually to isolate the problem
        safe_obj <- list(
            "update" = as.character(date),
            "content" = list()
        )
        return(jsonlite::toJSON(safe_obj, pretty = TRUE, auto_unbox = TRUE))
})
}

safe_write_json <- function(data, file_path) {
    tryCatch({
        write(data, file_path)
        return(TRUE)
    }, error = function(e) {
        warning(paste("Failed to write JSON:", e$message))
        # Write emergency fallback
        emergency_json <- toJSON(list("update" = Sys.Date(), "content" = list(), 
                                    "error" = "Character encoding issue"), 
                               pretty = TRUE, auto_unbox = TRUE)
        write(emergency_json, file_path)
        return(FALSE)
    })
}

extract_doi_id <- function(url){
    return(gsub("http(|s)://dx.doi.org/", "", url))
    }

clean_nul_characters <- function(x) {
    if (is.null(x) || is.na(x)) return(x)
    if (is.character(x)) {
        # Remove NUL characters (\u0000) and other problematic control characters
        x <- gsub("[\u0000-\u001F\u007F-\u009F]", "", x, perl = TRUE)
        return(x)
    }
    return(x)
}

strip_html <- function(str) {
   if(is.null(str)) return(NA)
   else {
    str <- gsub("<.*?>", " ", str)
    str <- gsub("\\s+", " ", str)
    str <- trimws(str)
    return(str)
   }
}

strip_whitespace <- function(str) {
   if(is.null(str)) return(NA)
   else {
    str <- gsub("\\s+", " ", str)
    return(trimws(str))
   }
}

file_empty <- function(file){
    length(readLines(file))==0
    }

read.csv2_check <- function(file, ...){
    if(!file_empty(file)){ 
        return(read.csv2(file, ...))
    } else { 
        return(NULL)
    }
}

# Crossref 
call_crossref_api <- function(id,type="issn",start,end,date_type="created", rows=1000, polite_endpoint){
    if( sum(type %in% c("issn", "doi"))!=1 ) stop("type must be either 'issn' or 'doi'")
    if( sum(date_type %in% c("created", "published"))!=1 ) stop("date_type must be either 'created' or 'published'")
    if(type=="issn"){
        endpoint <- paste0("https://api.crossref.org/journals/", id, "/works")
    }
    if(type=="doi"){
        endpoint <- paste0("https://api.crossref.org/prefixes/", id, "/works")
    }
    if(date_type=="created") {
        filter <- paste0("from-created-date:", start, ",until-created-date:", end)
    }
    if(date_type=="published") {
        filter <- paste0("from-pub-date:", start, ",until-pub-date:", end)
    }
    param = list(
        "filter"=filter, 
        "select"="title,author,abstract,URL,created", 
        rows=rows)
    if(polite_endpoint==TRUE){
        param$mailto <- crossref_email
    }
    res = RETRY("GET", url=endpoint, query=param, pause_base=5)
    
    # Add this error checking:
    if (status_code(res) != 200) {
        warning(paste("API request failed for", id, "with status:", status_code(res)))
        return(list(message = list(items = list())))  # Return empty structure that matches expected format
    }
    
    # Clean the raw response before JSON parsing
    raw_content <- content(res, "text", encoding = "UTF-8")
    # Remove NUL and other control characters from the raw JSON string
    clean_content <- stringi::stri_replace_all_regex(raw_content, "[\\p{C}]", "", vectorize_all = FALSE)
    clean_content <- iconv(clean_content, from = "UTF-8", to = "UTF-8", sub = "")
    clean_content <- gsub("[\u0000-\u001F\u007F-\u009F]", "", raw_content, perl = TRUE)
    clean_content <- gsub("\\x00", "", clean_content, fixed = TRUE)
    
    # Parse the cleaned JSON manually
    return(jsonlite::fromJSON(clean_content))
}

get_crossref_articles <- function(items){
    # Handle case where API returned error structure
    if(is.null(items$message) || is.null(items$message$items) || length(items$message$items) == 0) {
        return(NULL)
    }
    
    ll <- lapply(items$message$items, get_crossref_article_info)
    ll <- do.call(rbind, lapply(ll, function(x) as.data.frame(t(x))))
    return(ll)
}

get_crossref_article_info <- function(item){
    # Extract data first
    title <- get_crossref_title(item)
    authors <- get_crossref_authors(item)
    created <- get_crossref_date(item, "created")
    abstract <- get_crossref_abstract(item)
    url <- get_crossref_url(item)
    
    # Clean all control characters including NUL immediately
    if(!is.na(title)) title <- gsub("[\u0000-\u001F\u007F-\u009F]", "", title, perl = TRUE)
    if(!is.na(authors)) authors <- gsub("[\u0000-\u001F\u007F-\u009F]", "", authors, perl = TRUE)
    if(!is.na(abstract)) abstract <- gsub("[\u0000-\u001F\u007F-\u009F]", "", abstract, perl = TRUE)
    if(!is.na(url)) url <- gsub("[\u0000-\u001F\u007F-\u009F]", "", url, perl = TRUE)
    
    return(c(
        title = title,
        authors = authors,
        created = created,
        abstract = abstract, 
        url = url
    ))
}

get_crossref_abstract <- function(item){
    if(is.null(item$abstract)) return(NA)
    else return(item$abstract)
}

get_crossref_authors <- function(item){
    if(is.null(item$author)) return(NA)
    else return(paste(lapply(item$author, get_crossref_author), collapse=", "))
}

get_crossref_author <- function(item){
    paste(item$given, item$family)
}

get_crossref_date <- function(item, name){
    if(is.null(item[[name]])) return(NA)
    else paste(unlist(item[[name]][["date-parts"]]), collapse="-")
}

get_crossref_title <- function(item){
    if(is.null(item$title)) return(NA)
    else unlist(item$title)
}

get_crossref_journal <- function(item){
    if(is.null(item$`container-title`)) return(NA)
    else unlist(item$`container-title`)
}

get_crossref_url <- function(item){
    if(is.null(item$URL)) return(NA)
    else unlist(item$URL)
}

get_crossref_api_limits <- function(response){
    out <- headers(response)
    limit <- out$`x-ratelimit-limit`
    interval <- out$`x-ratelimit-interval`
    return(c("limit"=limit, "interval"=interval))
    }

# Crossref API response time 
httr_get_timed <- function(url, timout, query) {
    start_time <- Sys.time()

    result <- tryCatch(
        {
            response <- GET(url,
                timeout(timout),
                query = query
            )

            end_time <- Sys.time()
            time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))

            list(success = TRUE, time = time_taken)
        },
        error = function(e) {
            list(success = FALSE, error = as.character(e))
        }
    )

    return(result)
}

is_crossref_endpoint_polite_faster <- function(start, end, timeout) {

    issn <- sample(c("1476-4989", "0048-5829", "1554-0626", "0010-4159", "1460-3667", "0962-6298", 
                     "0043-8871", "1545-1577", "0140-2382", "1743-9655", "0020-8833", "1047-1987", 
                     "0362-9805", "1537-5943", "1469-2112"), size=1)

    url <- paste0("https://api.crossref.org/journals/",issn,"/works")
    filter <- paste0("from-created-date:", start, ",until-created-date:", end)

    query <- list(
        "filter" = filter,
        "select" = "title,author,abstract,URL,created",
        rows = 1000
    )

    result1 <- httr_get_timed(url, timeout, query)

    query$mailto <- crossref_email
    result2 <- httr_get_timed(url, timeout, query)

    cat("\tPublic API response time:", result1$time, "seconds\n")
    cat("\tPolite API response time:", result2$time, "seconds\n")

    if (result1$success && !result2$success) {
        return(1) 
    } else if (!result1$success && result2$success) {
        return(2) 
    } else if (!result1$success && !result2$success) {
        return(0)
    } else {
        return(ifelse(result1$time < result2$time, 1, 2))
    }
}

crossref_endpoint_polite_faster <- function(crawl_start_date, crawl_end_date) {
    res <- 0
    timeout <- 1
    while (res == 0) {
        cat("Testing crossref API with timeout:", timeout, "seconds\n")
        res <- is_crossref_endpoint_polite_faster(crawl_start_date, crawl_end_date, timeout = timeout)
        if (res != 0) break
        timeout <- timeout + 5
    }
    return(ifelse(res == 2, TRUE, FALSE))
    }
