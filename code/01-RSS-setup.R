library(pacman)
p_load(tidyverse, magrittr, tidyRSS, blastula, knitr, DBI, RSQLite)

#file path
file_path <- "/Users/sophiewill/Documents/data_projects/feedsandalerts"

#set wd to fix connection error
setwd(file_path)

#function to send RSS emails
rss_fn <- function() {
  
  #set today
  today <- as.Date(Sys.Date())
  
  #set connection
  con <- dbConnect(RSQLite::SQLite(), "./data/created/feeds_database.db")
  
  #set up table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS all_fed_reg (
      feed_pub_date TEXT,
      item_title TEXT,
      item_link TEXT,
      item_description TEXT,
      item_pub_date TEXT
    )
  ")
  
  #set up table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS filtered_fed_reg (
      feed_pub_date TEXT,
      item_title TEXT,
      item_link TEXT,
      item_description TEXT,
      item_pub_date TEXT
    )
  ")
  

  #### FEDERAL REGISTER ####
  #functions to get feeds, clean, and narrow down
  make_lists_fed_reg <- function(name, link){
    
    ## helper function to make a md list instead of a table ##
    make_md_list <- function(df) {
      if (nrow(df) == 0) return("_No updates for today._")
      
      df %>%
        mutate(
          #creating a cleaner bulleted list with extra lines
          bullet = glue::glue("* **{item_title}**\n  Published: {item_pub_date} | [View Document]({item_link})\n _{item_description}_\n\n")
        ) %>%
        pull(bullet) %>%
        paste(collapse = "\n") 
    }
    #pull fed reg and simplify
      df <- tidyfeed(link) %>% 
        select(item_pub_date, feed_pub_date, item_title, item_link, item_description)
      
    # pull what's already in the database to do a diff against
    existing <- dbGetQuery(con, "
    SELECT item_title, item_link, item_description, item_pub_date
    FROM all_fed_reg
  ")
      
      # keep only rows where the main columns don't already exist
      new_rows <- df %>%
        mutate(item_pub_date = as.character(item_pub_date)) %>%
        anti_join(
          existing %>% mutate(item_pub_date = as.character(item_pub_date)),
          by = c("item_title", "item_link", "item_description", "item_pub_date")
        )
      
      # ensure formatting is correct
      new_rows <- new_rows %>%
        mutate(
        item_pub_date  = as.character(item_pub_date),
        feed_pub_date  = as.character(feed_pub_date)
      )
      
      # insert new rows into all_fed_reg
      if (nrow(new_rows) > 0) {
        dbAppendTable(con, "all_fed_reg", new_rows %>%
                        select(feed_pub_date, item_title,
                               item_link, item_description, item_pub_date))
      }
      
      # keyword filter on only the new rows
      filtered_new <- new_rows %>%
        filter(str_detect(item_description,
                          regex("technology|website|telecommunications|surveillance|IT network|internet network|artificial intelligence|computer|data|privacy|cyber|modernization|fedramp|onegov|online|network|cloud|digitization|USDS|DOGE|a\\.i\\.|u\\.s\\.d\\.s\\.|d\\.o\\.g\\.e\\.|\\btech\\b",
                                ignore_case = TRUE)))
      
      
      # insert filtered new rows into filtered_fed_reg
      if (nrow(filtered_new) > 0) {
        dbAppendTable(con, "filtered_fed_reg", filtered_new %>%
                        select(feed_pub_date, item_title,
                               item_link, item_description, item_pub_date))
      }
      
      # only make bullets from filtered new rows
      filtered_new %>% make_md_list()
  }
  

  #read in registers 
  registers <- read.csv(paste0(file_path, "/data/created/fedreg.csv"))
  
  #run function and set names 
  registers_results <- map2(registers$name, registers$link, ~make_lists_fed_reg(.x, .y)) %>%
    setNames(registers$name)
  
  #put them each into the environment
  list2env(registers_results, envir = .GlobalEnv)
  
  
  #### GAO ####
  #set up table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS all_gao (
      feed_pub_date TEXT,
      item_title TEXT,
      item_link TEXT,
      item_description TEXT,
      item_pub_date TEXT
    )
  ")
  
  #set up table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS filtered_gao (
      feed_pub_date TEXT,
      item_title TEXT,
      item_link TEXT,
      item_description TEXT,
      item_pub_date TEXT
    )
  ")
  
  #functions to get feeds, clean, and narrow down
  make_lists_gao <- function(name, link){
    
    #pull fed reg and simplify
    df <- tidyfeed(link) %>% 
      select(item_pub_date, feed_pub_date, item_title, item_link, item_description)
    
    # pull what's already in the database to do a diff against
    existing <- dbGetQuery(con, "
    SELECT item_title, item_link, item_description, item_pub_date
    FROM all_gao
  ")
    
    # keep only rows where the main columns don't already exist
    new_rows <- df %>%
      mutate(item_pub_date = as.character(item_pub_date)) %>%
      anti_join(
        existing %>% mutate(item_pub_date = as.character(item_pub_date)),
        by = c("item_title", "item_link", "item_description", "item_pub_date")
      )
    
    # ensure formatting is correct
    new_rows <- new_rows %>%
      mutate(
        item_pub_date  = as.character(item_pub_date),
        feed_pub_date  = as.character(feed_pub_date)
      )
    
    # insert new rows into all_fed_reg
    if (nrow(new_rows) > 0) {
      dbAppendTable(con, "all_gao", new_rows %>%
                      select(feed_pub_date, item_title,
                             item_link, item_description, item_pub_date))
    }
    
    # keyword filter on only the new rows
    filtered_new <- new_rows %>%
      filter(str_detect(item_description,
                        regex("technology|website|telecommunications|surveillance|IT network|internet network|artificial intelligence|computer|data|privacy|cyber|modernization|fedramp|onegov|online|network|cloud|digitization|USDS|DOGE|a\\.i\\.|u\\.s\\.d\\.s\\.|d\\.o\\.g\\.e\\.|\\btech\\b",
                              ignore_case = TRUE)))
    
    # insert filtered new rows into filtered_fed_reg
    if (nrow(filtered_new) > 0) {
      dbAppendTable(con, "filtered_gao", filtered_new %>%
                      select(feed_pub_date, item_title,
                             item_link, item_description, item_pub_date))
    }
    
    # only make bullets from filtered new rows
    filtered_new %>% make_md_list()
  }

  #read in registers 
  gao <- read.csv(paste0(file_path, "/data/created/gao.csv"))
  
  #run function and set names 
  gao_results <- map2(gao$name, gao$link, ~make_lists_gao(.x, .y)) %>%
    setNames(gao$name)
  
  #put them each into the environment
  list2env(gao_results, envir = .GlobalEnv)
  
  ###### set up email #####
  email <- compose_email(
    body = md(glue::glue(
      "# **🌞 RSS Feeds for {today} 🌞**:
      
      -----
      
      # _📜FEDERAL REGISTER:📜_ 
      
      ## 🏢 GSA 🏢
      {gsa_fed_register}
      
      ## 🌾 Agriculture 🌾
      {ag_fed_register}
      
      ### 🌾 Agriculture significant docs 🌾
      {ag_sig_fed_register}
      
      ## 📚 Education 📚
      {edu_fed_register}
      
      ## 🪖 VA 🪖
      {va_fed_register}
      
      ## 🌎 EPA 🌎
      {epa_fed_register}
      
      ### 🌎 EPA significant docs 🌎
      {epa_sig_fed_register}
      
      ## 🏜️ Interior 🏜
      {interior_fed_register}
      
      ### 🏜️ Interior significant docs 🏜
      {interior_sig_fed_register}
      
      ## 💌 USPS 💌
      {usps_fed_register}
      
      ## 🏛️ Archives 🏛
      {nara_fed_register}
      
      ## 🔬 Patents & Trademarks 🔬
      {uspto_fed_register}
      
      -----
      # _📜GAO:📜_
      
      ## Reports
      {gao_reports}
      
      ## Legal
      {gao_legal}
      
      ## Legal Rules
      {gao_legal_rules}
      
      ## Press releases
      {gao_press}
      
      ## Blog
      {gao_blog}
      
      -----
      _This is an automated message sent from KSW's Work Laptop_"
    ))
  )
  
  #check that there is a password via smtp
  #set up creds here https://myaccount.google.com/u/1/apppasswords and put in renviron
  #usethis::edit_r_environ() if not set up
  smtp_password <- Sys.getenv("SMTP_PASSWORD")
  
  #set up email send log
  log_file <- "./data/created/email_log.txt"
  
  #send email
  log_output <- capture.output({
    tryCatch({
      email %>%  smtp_send(
        to = "sophie.will@fedscoop.com",
        from = "ksophiewill@gmail.com",
        subject = "🌞 RSS Updates 🌞",
        verbose = TRUE,
        credentials = creds_envvar(
          user = "ksophiewill@gmail.com",
          pass_envvar = "SMTP_PASSWORD",
          provider = "gmail")
      )
      cat(sprintf("[%s] SUCCESS: Email sent\n", Sys.time()))
    }, error = function(e) {
      cat(sprintf("[%s] ERROR: %s\n", Sys.time(), e$message))
    })
  })
  
  #write to log file
  cat(log_output, file = log_file, sep = "\n", append = TRUE)
  

}