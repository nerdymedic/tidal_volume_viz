library(tidyverse)

data <- seq(58, 78, 1) %>% 
  tibble() %>% 
  select("height" = 1) %>% 
  mutate(male_wt = 50 + 2.3*(height - 60), female_wt = 45.5 + 2.3*(height - 60)) %>% 
  mutate(male_tv = 6*male_wt, female_tv = 6*female_wt)

data %>% 
  ggplot(aes(x = height)) +
  geom_point(aes(y = male_tv), col = 'blue')+
  geom_point(aes(y = female_tv), col = "pink") +
  labs(x = "Height (in)", y = "Volume (ml)", title = "6mL/kg TV by height")

male_mean_height <- 70
male_sd_height  <- 3
female_mean_height <- 64.5
female_sd_height <- 2.5

male_empiric_tv <- 450
female_empiric_tv <- 350
no_SD <- 1

data %>% 
  mutate(error_m = male_empiric_tv - male_tv, error_f = female_empiric_tv - female_tv) %>% 
  ggplot(aes(x = height))  +
  geom_point(aes(y = error_m), col = "blue") +
  geom_point(aes(y = error_f), col = "pink") +
  labs(x = "Height (in)", 
       y = "Volume Error (cc)", 
       title = paste0("Error of empiric TV ","(",male_empiric_tv,"cc male, ", female_empiric_tv,"cc female) vs 6cc/kg"), 
       subtitle = "by @nerdymedic",
       caption = paste("Dotted lines are",no_SD,"SD of height")) +
  geom_vline(xintercept = c(male_mean_height - no_SD * male_sd_height, 
                            male_mean_height + no_SD * male_sd_height), 
             color = "blue", 
             linetype = "dotted") +
  geom_vline(xintercept = c(female_mean_height - no_SD * female_sd_height, 
                            female_mean_height + no_SD * female_sd_height), 
             color = "pink", 
             linetype = "dotted") +
  theme_bw()
  

