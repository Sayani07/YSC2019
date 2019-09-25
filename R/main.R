## ---- load
source("R/theme.R")
library(tidyverse)
library(sugrrants)
library(tsibble)
library(gravitas)
library(kableExtra)
library(gganimate)
#remotes::install_github("njtierney/palap")
library(palap)

smart_meter50 <- read_rds("~/Documents/paper-gravitas/data/sm_cust50.rds") %>%  
  select(customer_id, 
         reading_datetime,
         general_supply_kwh, 
         everything())

smart_meter50

##----search_gran


smart_meter50 %>%
  dynamic_search_gran(lowest_unit = "hhour", highest_unit =  "month")



##----create_gran


smart_meter50 %>%
  dynamic_create_gran("hour_week") %>% select(customer_id, 
                                              reading_datetime,
                                              hour_week,
                                              general_supply_kwh, 
                                              everything())


##----isharmony

smart_meter50 %>% is.harmony("hour_week","day_fortnight")

##----harmony


smart_meter50 %>% 
  harmony(ugran = "month",
          lgran = "hhour",
          filter_out = c("fortnight", "hhour"))

##----granplotoverlay
smart_meter50   %>% 
  granplot("month_year", 
           "hour_day",
           plot_type = "quantile", 
           response = "general_supply_kwh",
           quantile_prob = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99),
           overlay = TRUE) + ggtitle("") +
  scale_x_discrete(breaks = seq(0,23,2)) + 
  theme_remark()




##----granplot

# p1 = smart_meter50 %>% 
#   mutate(Demand = general_supply_kwh*10) %>% 
#   granplot("wknd_wday", 
#            "hour_day",
#            response = "Demand",
#            plot_type = "quantile", 
#            quantile_prob = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), overlay = FALSE) +
#   ggtitle("") + 
#   scale_colour_palap_d(palette = "lajolla",
#                        begin = 0.5,
#                        end = 1, direction = -1) + 
#   scale_x_discrete(breaks = seq(0,23,2)) + 
#   theme_remark() 
# 
# anim1 <-  p1 + transition_reveal(as.numeric(hour_day)) 
# 
# gganimate::animate(anim1, fps = 10, width = 1000, height = 600)
# 
# 
# anim_save("~/Documents/YSc2019/images/palap_quantile.gif")

knitr::include_graphics("images/palap_quantile.gif")

##----cricket


cricket <- read_rds("~/Documents/paper-gravitas/data/cricket_tsibble.rds") %>%  
  select(season, 
         match_id,
         inning,
         over,
         ball,
         winner,
         total_runs,
         everything())

glimpse(cricket)






##----cricketex

# 
#  cricket_per_over <- cricket %>%
#    group_by(season, match_id, batting_team, bowling_team,  inning, over) %>%
#    summarise(runs_per_over = sum(total_runs), run_rate = sum(total_runs)/length(total_runs))
# 
#  cricket_tsibble_all <- cricket_per_over %>%
#    ungroup() %>%
#    mutate(data_index = row_number()) %>%
#    as_tsibble(index = data_index)
# 
# 
#  p = cricket_tsibble_all %>%
#    filter(season %in% c(2010:2015),
#           batting_team %in% c("Mumbai Indians", "Kolkata Knight Riders"),
#                               inning %in% c(1,2)) %>%
#    granplot("inning", "over",
#             hierarchy_model,
#             response = "run_rate",
#             plot_type = "lv",
#             quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9))
# 


#anim <-  p + gganimate::transition_states(batting_team)+ labs(title = "{closest_state}")
# 
#gganimate::animate(anim, fps = 10, width = 1000, height = 600)


#anim_save("~/Documents/YSc2019/images/cricketex.gif")
# 

#
  

knitr::include_graphics("images/cricketex.gif")


  