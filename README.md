# Feeds and Alerts

#### By K. Sophie Will

This repository automates RSS feeds to customized email alerts.

### Repo structure:

- code:

  - 01-morning-RSS.R - code to pull RSS feeds and format it to be readable at 7am

  - 02-afternoon-RSS.R - code to pull RSS feeds and format it to be readable at 1pm

  - 03-afternoon-RSS.R - code to pull RSS feeds and format it to be readable at 11:59pm

  - 04-scheduler.R - schedule for screen & caffeinate to use

- docs:

  - data-dictionary.qmd - daily log of changes made to this repo

- data

  - created

    - fed_reg

      - fedreg.csv - list of names and links to the federal register RSS feeds

      - morning_register.rds - morning federal register

      - afternoon_register.rds - afternoon federal register

      - evening_register.rds - evening federal register

    - gao

      - gao.csv - list of names and links to the gao RSS feeds

      - morning_gao.rds - morning gao reports

      - afternoon_gao.rds - afternoon gao reports

      - evening_gao.rds - evening gao reports

### INFO FOR CAFFEINATE

###install screen if needed

brew install screen

###start a named session

screen -S RSSfeeds

###run caffeinate in screen

caffeinate -ism Rscript /path/to/scheduler.R
