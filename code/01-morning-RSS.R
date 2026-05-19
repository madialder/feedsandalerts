library(pacman)
p_load(tidyverse, magrittr, tidyRSS, blastula, knitr)

#set wd to fix connection error
setwd("/Users/sophiewill/Documents/data_projects/feedsandalerts")

morning_fn <- function() {
  #set today
  today <- as.Date(Sys.Date())
  
  #functions to get feeds, clean, and narrow down
  make_lists <- function(name, link){
    
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
    #combine all bullets into one long string
    result <- tidyfeed(link) %>% 
      filter(item_pub_date >= today) %>% #only items published today
      select(item_pub_date, feed_pub_date, item_title, item_link, item_description) %>% #just which columns I'm interested in
      filter(str_detect(item_description, 
                        regex("technology|website|artificial intelligence|computer|data|privacy|cyber|modernization|fedramp|onegov|online|network|cloud|digitization|USDS|DOGE|a\\.i\\.|u\\.s\\.d\\.s\\.|d\\.o\\.g\\.e\\.|\\btech\\b", 
                              ignore_case = TRUE))) %>% #get only key words
      ## SOPHIE ADD AGENCY FILTERING
      make_md_list()
    
    return(result)
  }
  
  #### FEDERAL REGISTER ####
  #read in registers 
  registers <- read.csv("/Users/sophiewill/Documents/data_projects/feedsandalerts/data/created/fedreg/fedreg.csv")
  
  
  #run function and set names 
  registers_results <- map2(registers$name, registers$link, make_lists) %>%
    setNames(registers$name)
  
    #write that
  write_rds(registers_results, "/Users/sophiewill/Documents/data_projects/feedsandalerts/data/created/fedreg/morning_register.rds")
  
  #put them each into the environment
  list2env(registers_results, envir = .GlobalEnv)
  
  
  #### GAO ####

  #read in registers 
  gao <- read.csv("/Users/sophiewill/Documents/data_projects/feedsandalerts/data/created/gao/gao.csv")
  
  #run function and set names 
  gao_results <- map2(gao$name, gao$link, make_lists) %>%
    setNames(gao$name)
  
  #write that
  write_rds(gao_results, "/Users/sophiewill/Documents/data_projects/feedsandalerts/data/created/gao/morning_gao.rds")
  
  #put them each into the environment
  list2env(gao_results, envir = .GlobalEnv)
  
  
  #### GRANTS ####
  
  
  
  
  #### USASPENDING ####
  
  
  
  
  #### COURTLISTENER ####
  
  
  
  ###### set up email #####
  email <- compose_email(
    body = md(glue::glue(
      "# **🌞 Morning RSS Feeds for {today} 🌞**:
      
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
      # _📜GRANTS:📜_
      
      -----
      # _📜USA SPENDING:📜_
      
      -----
      # _📜COURTS:📜_
      
      -----
      _This is an automated message sent at 7:00 a.m. {today} from KSW's Work Laptop_"
    ))
  )
  
  #check that there is a password via smtp
  #set up creds here https://myaccount.google.com/u/1/apppasswords and put in renviron
  #usethis::edit_r_environ() if not set up
  smtp_password <- Sys.getenv("SMTP_PASSWORD")
  
  # #send email
  email %>%  smtp_send(
    to = "sophie.will@fedscoop.com",
    from = "ksophiewill@gmail.com",
    subject = "🌞 Morning RSS Updates 🌞",
    credentials = creds_envvar(
      user = "ksophiewill@gmail.com",
      pass_envvar = "SMTP_PASSWORD",
      provider = "gmail"
    )
  )
}