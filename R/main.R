## ---- load
source("R/theme.R")
library(tidyverse)
library(sugrrants)
library(tsibble)
library(gravitas)
library(kableExtra)
library(gganimate)
library(lubridate)
library(ggridges)
library(ggpubr)
#remotes::install_github("njtierney/palap")
library(palap)

smart_meter50 <- read_rds("data/sm_cust50.rds")%>%  
  select(customer_id, 
         reading_datetime,
         general_supply_kwh, 
         everything())

##----motivation1

smart_meter50 %>% filter(customer_id %in% 10006414) %>% ggplot() + geom_line(aes(x =reading_datetime, y = general_supply_kwh), color = "#1B9E77")+ theme(legend.position = "bottom")


##----motivation2
smart_meter50 %>% filter(customer_id %in% c(10018254)) %>%  ggplot() + geom_line(aes(x =reading_datetime, y = general_supply_kwh, color = customer_id), color = "#D95F02") + theme(legend.position = "bottom")

##----motivation3

# smart_p <- smart_meter50 %>%  ggplot() + geom_line(aes(x =reading_datetime, y = general_supply_kwh, color = customer_id))  + theme(legend.position = "None")
# 
# smart_anim <-  smart_p + gganimate::transition_states(customer_id)+ labs(title = "{closest_state}")
# 
# gganimate::animate(smart_anim, fps = 10, width = 1000, height = 600)
# 
# 
# anim_save("images/smart_allcust.gif")

knitr::include_graphics("images/smart_allcust.gif")


##----motivation5
smart_meter50 %>%
  filter(customer_id %in% 10006414) %>%
  mutate(hour_day = hour(reading_datetime)) %>% 
  ggplot() + geom_point(aes(x = hour_day, y = general_supply_kwh)) + 
  theme(legend.position = "bottom") + ggtitle("10006414")


##----motivation6

# smart_p_period <- smart_meter50 %>%
#   mutate(hour_day = hour(reading_datetime)) %>% 
#   ggplot() + geom_point(aes(x = hour_day, y = general_supply_kwh, color = customer_id))  +  theme(legend.position = "None")
# 
# 
# 
# smart_anim_period <-  smart_p_period + gganimate::transition_states(customer_id)+ labs(title = "{closest_state}")
# 
# gganimate::animate(smart_anim_period, fps = 10, width = 1000, height = 600)
# 
# 
# anim_save("images/smart_allcust_period.gif")

knitr::include_graphics("images/smart_allcust_period.gif")


##----motivation7
# smart_meter50  %>%
#   mutate(hour_day = hour(reading_datetime)) %>% 
#   ggplot() + geom_point(aes(x = hour_day, y = general_supply_kwh, color = customer_id)) +  theme(legend.position = "none")

# knitr::include_graphics("images/hd_allcust.png")

##----read
smart_meter50


##----search_gran


smart_meter50 %>%
  dynamic_search_gran(lowest_unit = "hhour", highest_unit =  "month")


##----create_gran

smart_meter50 %>%
  create_gran("hour_week") %>% select(customer_id, 
                                              reading_datetime,
                                              hour_week,
                                              general_supply_kwh, 
                                              everything())


##----isharmony

smart_meter50 %>% is.harmony("hour_week","day_fortnight")

##----harmony
cust1 <- smart_meter50 %>% distinct(customer_id) %>% head(10)
cust2 <- smart_meter50 %>% distinct(customer_id) %>% head(20) %>% filter(!(customer_id %in% cust1))

cust3 <- smart_meter50 %>% distinct(customer_id) %>% head(30)%>% filter(!(customer_id %in% c(cust1, cust2)))
cust4 <- smart_meter50 %>% distinct(customer_id) %>% head(10)


smart_meter50 %>% 
  harmony(ugran = "month",
          lgran = "hhour",
          filter_out = c("fortnight", "hhour")) %>% knitr::kable()

##----granplotoverlay1


smart_meter50   %>% 
  filter(customer_id %in% cust1$customer_id) %>% 
  granplot("month_year", 
           "hour_day",
           plot_type = "quantile", 
           response = "general_supply_kwh",
           quantile_prob = c(0.25, 0.5, 0.75),
           overlay = TRUE) + ggtitle("") +
  scale_x_discrete(breaks = seq(0,23,2)) + 
  theme_remark() 


##----granplotoverlay2
smart_meter50   %>% 
  filter(customer_id %in% cust1$customer_id) %>% 
  granplot("month_year", 
           "hour_day",
           plot_type = "quantile", 
           response = "general_supply_kwh",
           quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9),
           overlay = TRUE) + ggtitle("") +
  scale_x_discrete(breaks = seq(0,23,2)) + 
  theme_remark()

##----granplotoverlay3
smart_meter50   %>% 
  filter(customer_id %in% cust1$customer_id) %>% 
  granplot("month_year", 
           "hour_day",
           plot_type = "quantile", 
           response = "general_supply_kwh",
           quantile_prob = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
           overlay = TRUE) + ggtitle("") +
  scale_x_discrete(breaks = seq(0,23,2)) + 
  theme_remark()



##----granplotoutlier

# smart_meter50   %>% 
#   filter(customer_id %in% cust1$customer_id) %>% 
#   granplot("day_week", 
#            "day_month",
#            plot_type = "violin", 
#            response = "general_supply_kwh",
#            quantile_prob = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
#            overlay = TRUE) + ggtitle("") + 
#   theme_remark()


# smart_meter50  %>% create_gran("day_month") %>% filter(day_month %in% c(1:12)) %>% 
#   granplot("day_week", 
#            "day_month",
#            plot_type = "violin", 
#            response = "general_supply_kwh",
#            quantile_prob = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
#            overlay = TRUE) + ggtitle("") + 
#   theme_remark() + geom_boxplot(width = 0.01, color =" Blue")
# 
# smart_meter50  %>% create_gran("hour_day") %>% filter(hour_day %in% c(0:12)) %>% 
#   granplot("week_month", 
#            "hour_day",
#            plot_type = "violin", 
#            response = "general_supply_kwh",
#            quantile_prob = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
#            overlay = TRUE) + ggtitle("") + 
#   theme_remark() + geom_boxplot(width = 0.01, color =" Blue")



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


cricket <- read_rds("data/cricket_tsibble.rds")%>%  
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


##----lineartime

knitr::include_graphics("images/linear-time.png")


##----circulartime
knitr::include_graphics("images/circular.png")


##----calendar
knitr::include_graphics("images/calendar_new.jpg")


##----allplot

pbox <- smart_meter50 %>% 
  create_gran("hour_day") %>% 
  filter(hour_day %in% c(20, 10, 2, 15)) %>% 
  ggplot(aes(x = hour_day, y = log(general_supply_kwh))) +
  geom_boxplot() + ylab("") + xlab("")


pviolin <- smart_meter50 %>% 
  create_gran("hour_day") %>% 
  filter(hour_day %in% c(20, 10, 2, 15)) %>% 
  ggplot(aes(x = hour_day, y = log(general_supply_kwh))) +
  geom_violin() + ylab("") + xlab("")
  
plv <- smart_meter50 %>% 
  create_gran("hour_day") %>% 
  filter(hour_day %in% c(20, 10, 2, 15)) %>% 
  ggplot(aes(x = hour_day, y = log(general_supply_kwh))) + 
  geom_lv(aes(fill = ..LV..), outlier.colour = "red", outlier.shape = 1) +
  ylab("") + xlab("") + theme(legend.position = "None") 


mpg <- mpg %>% filter (class %in% c("compact", "midsize", "suv","minivan", "pickup")) %>% 
  mutate(cls = 
           case_when(
             class == "compact" ~ "A",
             class == "midsize" ~ "B",
             class == "suv" ~ "C",
             class == "minivan"  ~ "D"))

pridge <-  ggplot(mpg, aes( hwy, cls)) + geom_density_ridges2()+  xlab("") + ylab("")


 p4_quantile <- smart_meter50 %>% 
   create_gran("hour_day") %>% 
   mutate(Demand = log(general_supply_kwh)) %>% 
   group_by(hour_day) %>%  do({
    x <- .$Demand
    map_dfr(
      .x = c(0.1, 0.25, 0.5, 0.75, 0.9),
      .f = ~ tibble(
        Quantile = .x,
        Value = quantile(x, probs = .x, na.rm = TRUE)
      )
    )
  })
  
  pquant <- p4_quantile %>% ggplot(aes(x = hour_day, y = Value, group=Quantile,  col = as.factor(Quantile))) + geom_line() +   xlab("") + ylab("") + theme(legend.position = "None") + scale_color_brewer(palette = "Dark2") +   ylab("") + xlab("") + scale_x_discrete(breaks = seq(0, 23, 5))
  
  
ggarrange(pbox, pviolin, plv, pridge, pquant, nrow = 1, ncol =5, labels = c("box", "violin", "letter-value", "ridge", "quantile"))


##----box
mpg <- mpg %>% filter (class %in% c("compact", "midsize", "suv","minivan", "pickup")) %>% 
                   mutate(cls = 
                        case_when(
                          class == "compact" ~ "A",
                          class == "midsize" ~ "B",
                          class == "suv" ~ "C",
                          class == "minivan"  ~ "D",
                          class == "pickup" ~"E"))
                          
                          

p <- ggplot(mpg, aes(cls, hwy))


p1 = p + geom_boxplot(fill = "white", colour = "#3366FF")+ xlab("") + ylab("")
p1 

##----violin
p2 = p + geom_violin(fill = "white", colour = "#3366FF")+  xlab("") + ylab("")
p2 
##----lv
p3 = p + geom_lv(aes(fill = ..LV..), outlier.colour = "red", outlier.shape = 1)+  xlab("") + ylab("")  + theme(legend.position = "None")
p3 

##----quantile
p4_quantile <- mpg %>% group_by(cls) %>%  do({
  x <- .$hwy
  map_dfr(
    .x = seq(0.1, 0.9, 0.1),
    .f = ~ tibble(
      Quantile = .x,
      Value = quantile(x, probs = .x, na.rm = TRUE)
    )
  )
})

p4 <- p4_quantile %>% ggplot(aes(x = cls, y = Value, group=Quantile,  col = as.factor(Quantile))) + geom_line() +   xlab("") + ylab("") + theme(legend.position = "None") + scale_color_brewer(palette = "Dark2")

p4 

#----ridge
p5 <- ggplot(mpg, aes( hwy, cls)) + geom_density_ridges2()+  xlab("") + ylab("")
p5 

#----overlay

# p4_overlay <- mpg %>% group_by(cls) %>%  do({
#   x <- .$hwy
#   map_dfr(
#     .x = c(0.1, 0.25, 0.5, 0.75, 0.9),
#     .f = ~ tibble(
#       Quantile = .x,
#       Value = quantile(x, probs = .x, na.rm = TRUE)
#     )
#   )
# })
# 
# 
# p4_spread <- p4_overlay %>% spread(Quantile, Value)
# 
# ggplot(p4_spread) %>% geom_line(x = cls, y='0.5')

##---types

#ggarrange(p1, p2, p3, p4, p5, nrow = 2, ncol =3, labels = c("box", "violin", "lv", "decile", "ridge"))

#----clash

knitr::include_graphics("images/clash.png")


##----di

knitr::include_graphics("images/di.jpg")


##----rob

knitr::include_graphics("images/rob.jpg")




