field <- "jewish_studies" # from commandArgs(trailingOnly = TRUE)

library(httr)
library(jsonlite)

source("fun.R")
source("credentials.R")
source("./parameters/prompts.R")

now <- Sys.time()
crawl_start_date <- as.Date(now) - 14
crawl_end_date <- as.Date(now) - 1

journals <- read.csv(paste0("./parameters/", field, "_journals.csv"))
past_urls <- read.csv(paste0("./memory/", field, "_urls.csv"))

use_polite <- crossref_endpoint_polite_faster(crawl_start_date,crawl_end_date)

# Crawl Crossref API 
out <- retrieve_crossref_issn_data(
    issn_list=journals$issn, 
    start_date=crawl_start_date, 
    end_date=crawl_end_date, 
    verbose=TRUE, polite_endpoint=use_polite)

# Remove duplicates
out <- out[!duplicated(out$url),] 
# Remove past papers
out <- out[!(out$url %in% past_urls$url), ]

if(!is.null(out) && nrow(out) > 0) {
    cat("Processing", nrow(out), "articles\n")
    
    # Ultra-safe character cleaning function
    safe_clean <- function(x) {
        if (is.null(x) || is.na(x)) return(x)
        if (is.character(x)) {
            # Multiple passes of cleaning
            x <- iconv(x, to = "UTF-8", sub = "")
            x <- gsub("[\u0000-\u001F\u007F-\u009F]", "", x, perl = TRUE)
            x <- gsub("\0", "", x, fixed = TRUE)
            return(x)
        }
        return(x)
    }
    
    # Apply to all character columns first
    char_cols <- sapply(out, is.character)
    out[char_cols] <- lapply(out[char_cols], safe_clean)
    
    # Then do specific cleaning
    out$abstract <- strip_html(out$abstract)
    out$abstract <- gsub("^(Abstract|ABSTRACT) ", "", out$abstract)
    out$title <- strip_html(out$title)
    out$doi <- extract_doi_id(out$url)
    
    # Final safety clean
    out[char_cols] <- lapply(out[char_cols], safe_clean)
    
    cat("Data cleaning completed\n")
} else {
    cat("No data to process\n")
}

if(is.null(out)) {
    json <- toJSON(list("update"=as.Date(now), "content"=list()), 
        pretty=TRUE, auto_unbox=TRUE)
    write(json, paste0("./output/", field, ".json"))
    quit(save="no")
    } 

# Merge in journal information 
out <- merge(out, journals, by="issn")

# Apply standard filter flags 
out <- add_standard_filter(out) 

# Add this debug section right before the JSON writing:
cat("=== DEBUGGING SECTION ===\n")
cat("Data dimensions:", dim(out), "\n")
cat("Columns:", names(out), "\n")

# Save the data as RDS for inspection
saveRDS(out, "debug_data.rds")
cat("Debug data saved as debug_data.rds\n")

# Check each column for NUL characters
for(col_name in names(out)) {
    if(is.character(out[[col_name]])) {
        nul_count <- sum(grepl("\u0000", out[[col_name]], fixed = TRUE))
        if(nul_count > 0) {
            cat("Column", col_name, "has", nul_count, "entries with NUL characters\n")
        }
    }
}

# Try a minimal JSON first
test_json <- toJSON(list("test" = "simple"), pretty = TRUE)
cat("Simple JSON test successful\n")

# Output JSON
out_json <- render_json(out, date=as.Date(now)) 
safe_write_json(out_json, paste0("./output/", field, ".json"))

# Update past urls
write.table(out[,"url"], 
    file=paste0("./memory/", field, "_urls.csv"), 
    na="", 
    sep=";", 
    append=TRUE, 
    quote=FALSE, 
    col.names=FALSE,
    row.names=FALSE)

# Write journal short list
journals_out <- unique(journals[,c("journal_full","journal_short")])
journals_out <- journals_out[order(journals_out$journal_full),]
journals_out <- gsub("\\x00", "", jsonlite::toJSON(journals_out, pretty=TRUE, auto_unbox=TRUE))
write(journals_out, paste0("./output/", field, "_journals.json"))
