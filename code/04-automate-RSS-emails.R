library(pacman)
p_load(tidyverse, magrittr, tidyRSS, blastula, cronR)

#set up path
morning <- cron_rscript("/Users/sophiewill/Documents/Data Projects/feedsandalerts/code/01-setting-up-morning-RSS.R")
afternoon <- cron_rscript("/Users/sophiewill/Documents/Data Projects/feedsandalerts/code/02-afternoon-RSS.R")
evening <- cron_rscript("/Users/sophiewill/Documents/Data Projects/feedsandalerts/code/03-evening-RSS.R")

# #cron docs https://cran.r-project.org/web/packages/cronR/cronR.pdf
# #test cron
# cron_add(command = cmd, frequency = 'daily', at = "10:21", id = "test1",
         # description = "testing cron")
# #then remove it
# cron_clear(ask = TRUE, user = "")
# #and check it's gone
cron_ls()

#add actual schedule
cron_add(command = morning, frequency = 'daily', at = "07:00", id = "Morning Daily FedReg",
         description = "Sending daily federal register for my agencies, morning check")

cron_add(command = afternoon, frequency = 'daily', at = "13:00", id = "Afternoon Daily FedReg",
         description = "Sending daily federal register for my agencies, afternoon check")

cron_add(command = evening, frequency = 'daily', at = "23:59", id = "Evening Daily FedReg",
         description = "Sending daily federal register for my agencies, evening check")