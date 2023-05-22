# changes the location
setwd("F:/project/R/testovoe/data")

# Sys.setlocale("LC_ALL", "Russian")
# Sys.setlocale("LC_ALL", "Russian_Russia.1252")
Sys.setlocale("LC_CTYPE", "en_RU.UTF-8")

#if u need install package
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyverse")

# Load package
# library("dplyr")                             
library("ggplot2") 
library("tidyverse") 

# changes the location path
data_all <- list.files(path = "F:/project/R/testovoe/data", pattern = "*.csv",
                       full.names = TRUE)
df <- bind_rows(lapply(data_all, read.csv2))

stolbcov = length(df)
strok_do = nrow(df)


df_clear = df[ ! duplicated(df), ]

df_clear$taster_name <- gsub('Е', 'E', df_clear$taster_name)
df_clear$taster_name <- gsub('е', 'e', df_clear$taster_name)
df_clear$taster_name <- gsub('Т', 'T', df_clear$taster_name)
df_clear$taster_name <- gsub('у', 'y', df_clear$taster_name)
df_clear$taster_name <- gsub('О', 'O', df_clear$taster_name)
df_clear$taster_name <- gsub('о', 'o', df_clear$taster_name)
df_clear$taster_name <- gsub('Р', 'P', df_clear$taster_name)
df_clear$taster_name <- gsub('р', 'p', df_clear$taster_name)
df_clear$taster_name <- gsub('а', 'a', df_clear$taster_name)
df_clear$taster_name <- gsub('А', 'A', df_clear$taster_name)
df_clear$taster_name <- gsub('Н', 'H', df_clear$taster_name)
df_clear$taster_name <- gsub('к', 'k', df_clear$taster_name)
df_clear$taster_name <- gsub('К', 'K', df_clear$taster_name)
df_clear$taster_name <- gsub('х', 'x', df_clear$taster_name)
df_clear$taster_name <- gsub('Х', 'X', df_clear$taster_name)
df_clear$taster_name <- gsub('с', 'c', df_clear$taster_name)
df_clear$taster_name <- gsub('С', 'C', df_clear$taster_name)
df_clear$taster_name <- gsub('В', 'B', df_clear$taster_name)
df_clear$taster_name <- gsub('М', 'M', df_clear$taster_name)

df_clear = df_clear[ ! duplicated(df_clear), ]

strok_posle = nrow(df_clear)
unikal_testerov = length(unique(df_clear$taster_name, na.rm = TRUE))

points_mean = mean(df_clear$points, na.rm = TRUE)
price_mean = mean(df_clear$price, na.rm = TRUE)

points_median = median(df_clear$points, na.rm = TRUE)
price_median = median(df_clear$price, na.rm = TRUE)

points_sd = sd(df_clear$points, na.rm = TRUE)
price_sd = sd(df_clear$price, na.rm = TRUE)

varr <- data.frame(table(df_clear$taster_name, na.rm = TRUE))
top_tester <- head(varr[order(-varr$Freq),], 5)
names(top_tester)[1] <- "tester_name"
names(top_tester)[2] <- "connt_point"


#cor test 
x = df_clear$points
y = df_clear$price
plot(x, y, pch= 16 )
correl_test <- cor.test(x, y)

df_clear %>% ggplot(aes(x = points, y = price)) + 
  geom_point(color = 'navy') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20))+
    scale_y_continuous(limits = c(0,1111))+
    geom_smooth(method=lm , color="red", se=FALSE) +
    labs(title = "Dependence of price on points(lim price 1111)",
          x = 'Points',
          y = 'Price')

df_clear %>% ggplot(aes(x = points, y = price)) + 
  geom_point(color = 'navy') +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20))+
  geom_smooth(method=lm , color="red", se=FALSE) +
  labs(title = "Dependence of price on points",
       x = 'Points',
       y = 'Price')

distribution_price_bt_c <- df_clear %>% ggplot(aes(x = price)) +
  geom_histogram(fill = 'lightblue', color = 'blue') +
  scale_y_continuous(limits = c(0,500))+
  scale_x_continuous(limits = c(0,150))+
  facet_wrap(~country) + labs(title = "Distribution of price by country",
                              x = 'Price',
                              y = 'Count')
distribution_price_bt_c


distribution_point_bt_c <- df_clear %>% ggplot(aes(x = points)) +
  geom_histogram(fill = 'lightblue', color = 'blue') +
    facet_wrap(~country)+ 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
    scale_y_continuous(limits = c(0,1500))+
    labs(title = "Distribution of points by country",
                                x = 'Points',
                                y = 'Count')
distribution_point_bt_c

graf_points_bulgar <- df_clear %>% filter(country == "Bulgaria") %>% 
  ggplot(aes(x = points)) + 
  geom_histogram(fill = 'lightblue', color = 'blue') +
  facet_wrap(~country) + scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  labs(title = "Spread of points in Bulgaria", 
                                                x = 'Points',
                                                y = 'Count')
graf_points_bulgar

graf_price_bulgar <- df_clear %>% filter(country == "Bulgaria") %>% 
  ggplot(aes(x = price)) + 
  geom_histogram(fill = 'lightblue', color = 'blue') +
  facet_wrap(~country) + scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(title = "Spread of price in Bulgaria", 
                                                x = 'Price',
                                                y = 'Count')
graf_price_bulgar


graf_somelie_in_country <- df_clear %>% 
  filter(df_clear$taster_name == top_tester$tester_name) %>%
    ggplot(aes(x = country)) +
      geom_bar(fill = 'lightblue', color = 'blue') +
      facet_grid(~taster_name, scales="free_x") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      labs(title = "Top-5 score by country", 
           x = 'Country',
           y = 'Count') 
graf_somelie_in_country


graf_somelie_in_points <- df_clear %>% 
  filter(df_clear$taster_name == top_tester$tester_name ) %>%
  ggplot(aes(x = points)) +
    geom_bar(fill = 'lightblue', color = 'blue') +
    facet_grid(~taster_name, scales="free_x") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    labs(title = "Wine point range by TOP-5", 
         x = 'Points',
         y = 'Count') 
graf_somelie_in_points

graf_somelie_in_price <- df_clear %>% 
  filter(df_clear$taster_name == top_tester$tester_name ) %>%
  ggplot(aes(x = price)) +
  geom_bar(fill = 'lightblue', color = 'blue') +
  facet_grid(~taster_name, scales="free_x" ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20), limits = c(0,150))+
  labs(title = "Wine price range by TOP-5", 
       x = 'Price',
       y = 'Count') 
graf_somelie_in_price