# Defining the R File
library(dplyr)
library(stringr)
library(ggplot2)

# 1. Create a data frame of available hiking and camping gear
data_gear <- tibble(
  item_category = c("backpack", "tent", "sleeping bag", "stove", "cookset", "water filter", "thermos", "lantern", "knife", "hatchet", "first aid kit", "emergency shelter"),
  item_name = c("Fjallraven Kanken Classic", "MSR Hubba Hubba NX", "Marmot Trestles", "MSR Pocket Rocket Stove", "GSI Outdoors Halulite Cookset", "Sawyer Mini Water Filter", "Stanley Classic Thermos", "BioLite BaseLantern", "Gerber Freeman Guide Knife", "Fiskars X7 Hatchet", "First Aid Only All-Purpose First Aid Kit", "SOL Emergency Bivvy"),
  eco_friendly = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE),
  sustainability_score = c(10, 9, 3, 8, 8, 10, 3, 10, 8, 4, 9, 9),
  price = c(70, 250, 70, 30, 55, 23, 20, 80, 24, 25, 25, 20)
)

# 2. Create a function to calculate the overall sustainability score of outdoor gear 
calc_sustainability_overall <- function(data){
 
  # define a factor to account for eco-friendly/sustainability factor
  eco_friendly_factor <- data$eco_friendly * 0.5
  sustainability_factor <- data$sustainability_score * 0.5
  
  # calculate the overall sustainability score
  sustainability_overall <- eco_friendly_factor + sustainability_factor
  
  # return the overall sustainability score
  return (sustainability_overall)
}

# 3. Add a new column to the data frame with the overall sustainability score
data_gear <- data_gear %>%
  mutate(sustainability_overall = calc_sustainability_overall(.))

# 4. Create summary statistics for the overall sustainability score
sustainability_summary <- data_gear %>% 
  summarise(
    min_score = min(sustainability_overall),
    mean_score = mean(sustainability_overall),
    max_score = max(sustainability_overall)
  )

# 5. Create a bar chart of the sustainability score
ggplot(data_gear, aes(x = item_name, y = sustainability_overall)) +
  geom_bar(stat = "identity") +
  labs(title = "Sustainability Score for Hiking and Camping Gear",
       x = "Item",
       y = "Sustainability Score")

# 6. Create a summary table of the data
table_gear <- data_gear %>% 
  select(item_category, item_name, eco_friendly, sustainability_score, sustainability_overall, price) %>%
  group_by(item_category) %>%
  summarise(
    count = n(),
    mean_score = mean(sustainability_score),
    mean_overall = mean(sustainability_overall),
    mean_price = mean(price)
  )

# 7. Create a function to filter for eco-friendly/sustainable items
eco_filter <- function(data){
  
  data_eco <- data %>%
    filter(eco_friendly == TRUE)
    
  # return the filtered data
  return (data_eco)
}

# 8. Filter the data frame for eco-friendly items
data_eco <- eco_filter(data_gear)

# 9. Create a summary table of the filtered data
table_eco <- data_eco %>%
  select(item_category, item_name, eco_friendly, sustainability_score, sustainability_overall, price) %>%
  group_by(item_category) %>%
  summarise(
    count = n(),
    mean_score = mean(sustainability_score),
    mean_overall = mean(sustainability_overall),
    mean_price = mean(price)
  )

# 10. Create a function to filter for items with a high sustainability score
score_filter <- function(data, thresh){
  
  data_score <- data %>%
    filter(sustainability_score >= thresh)
    
  # return the filtered data
  return (data_score)
}

# 11. Filter the data frame for items with a high sustainability score
data_score <- score_filter(data_gear, thresh = 8)

# 12. Create a summary table of the filtered data
table_score <- data_score %>%
  select(item_category, item_name, eco_friendly, sustainability_score, sustainability_overall, price) %>%
  group_by(item_category) %>%
  summarise(
    count = n(),
    mean_score = mean(sustainability_score),
    mean_overall = mean(sustainability_overall),
    mean_price = mean(price)
  )

# 13. Create a function to filter for items with a high overall sustainability score
overall_filter <- function(data, thresh){
  
  data_overall <- data %>%
    filter(sustainability_overall >= thresh)
    
  # return the filtered data
  return (data_overall)
}

# 14. Filter the data frame for items with a high overall sustainability score
data_overall <- overall_filter(data_gear, thresh = 8)

# 15. Create a summary table of the filtered data
table_overall <- data_overall %>%
  select(item_category, item_name, eco_friendly, sustainability_score, sustainability_overall, price) %>%
  group_by(item_category) %>%
  summarise(
    count = n(),
    mean_score = mean(sustainability_score),
    mean_overall = mean(sustainability_overall),
    mean_price = mean(price)
  )

# 16. Create a function to filter for items within a particular price range
price_filter <- function(data, lower, upper){
  
  data_price <- data %>%
    filter(price >= lower, price <= upper)
    
  # return the filtered data
  return (data_price)
}

# 17. Filter the data frame for items within a particular price range
data_price <- price_filter(data_gear, lower = 20, upper = 100)

# 18. Create a summary table of the filtered data
table_price <- data_price %>%
  select(item_category, item_name, eco_friendly, sustainability_score, sustainability_overall, price) %>%
  group_by(item_category) %>%
  summarise(
    count = n(),
    mean_score = mean(sustainability_score),
    mean_overall = mean(sustainability_overall),
    mean_price = mean(price)
  )

# 19. Create a function to filter for items that meet a certain combination of criteria
criteria_filter <- function(data, eco, score, overall, lower, upper){
  
  data_criteria <- data %>%
    filter(eco_friendly == eco, sustainability_score >= score, sustainability_overall >= overall,
           price >= lower, price <= upper)
    
  # return the filtered data
  return (data_criteria)
}

# 20. Filter the data frame for items that meet a certain combination of criteria
data_criteria <- criteria_filter(data_gear, eco = TRUE, score = 8, overall = 8, lower = 20, upper = 100)

# 21. Create a summary table of the filtered data
table_criteria <- data_criteria %>%
  select(item_category, item_name, eco_friendly, sustainability_score, sustainability_overall, price) %>%
  group_by(item_category) %>%
  summarise(
    count = n(),
    mean_score = mean(sustainability_score),
    mean_overall = mean(sustainability_overall),
    mean_price = mean(price)
  )

# 22. Create a summary table of the overall sustainability scores for all items
sustainability_all <- data_gear %>% 
  summarise(
    min_score = min(sustainability_overall),
    mean_score = mean(sustainability_overall),
    max_score = max(sustainability_overall)
  )

# 23. Create a summary table of the overall sustainability scores for eco-friendly items
sustainability_eco <- data_eco %>% 
  summarise(
    min_score = min(sustainability_overall),
    mean_score = mean(sustainability_overall),
    max_score = max(sustainability_overall)
  )

# 24. Create a summary table of the overall sustainability scores for items with a high sustainability score
sustainability_score <- data_score %>% 
  summarise(
    min_score = min(sustainability_overall),
    mean_score = mean(sustainability_overall),
    max_score = max(sustainability_overall)
  )

# 25. Create a summary table of the overall sustainability scores for items with a high overall sustainability score
sustainability_overall <- data_overall %>% 
  summarise(
    min_score = min(sustainability_overall),
    mean_score = mean(sustainability_overall),
    max_score = max(sustainability_overall)
  )

# 26. Create a summary table of the overall sustainability scores for items within a particular price range
sustainability_price <- data_price %>% 
  summarise(
    min_score = min(sustainability_overall),
    mean_score = mean(sustainability_overall),
    max_score = max(sustainability_overall)
  )

# 27. Create a summary table of the overall sustainability score for items that meet a certain combination of criteria
sustainability_criteria <- data_criteria %>% 
  summarise(
    min_score = min(sustainability_overall),
    mean_score = mean(sustainability_overall),
    max_score = max(sustainability_overall)
  )

# 28. Create a function to filter for items that are both high-scoring and eco-friendly
score_eco_filter <- function(data, score, eco){
  
  data_score_eco <- data %>%
    filter(sustainability_score >= score, eco_friendly == eco)
    
  # return the filtered data
  return (data_score_eco)
}

# 29. Filter the data frame for items that are both high-scoring and eco-friendly
data_score_eco <- score_eco_filter(data_gear, score = 8, eco = TRUE)

# 30. Create a summary table of the filtered data
table_score_eco <- data_score_eco %>%
  select(item_category, item_name, eco_friendly, sustainability_score, sustainability_overall, price) %>%
  group_by(item_category) %>%
  summarise(
    count = n(),
    mean_score = mean(sustainability_score),
    mean_overall = mean(sustainability_overall),
    mean_price = mean(price)
  )

# 31. Create a summary table of the overall sustainability scores for items that are both high-scoring and eco-friendly
sustainability_score_eco <- data_score_eco %>% 
  summarise(
    min_score = min(sustainability_overall),
    mean_score = mean(sustainability_overall),
    max_score = max(sustainability_overall)
  )

# 32. Create a function to filter for items that are both budget-friendly and eco-friendly
budget_eco_filter <- function(data, lower, eco){
  
  data_budget_eco <- data %>%
    filter(price >= lower, eco_friendly == eco)
    
  # return the filtered data
  return (data_budget_eco)
}

# 33. Filter the data frame for items that are both budget-friendly and eco-friendly
data_budget_eco <- budget_eco_filter(data_gear, lower = 20, eco = TRUE)

# 34. Create a summary table of the filtered data
table_budget_eco <- data_budget_eco %>%
  select(item_category, item_name, eco_friendly, sustainability_score, sustainability_overall, price) %>%
  group_by(item_category) %>%
  summarise(
    count = n(),
    mean_score = mean(sustainability_score),
    mean_overall = mean(sustainability_overall),
    mean_price = mean(price)
  )

# 35. Create a summary table of the overall sustainability scores for items that are both budget-friendly and eco-friendly
sustainability_budget_eco <- data_budget_eco %>% 
  summarise(
    min_score = min(sustainability_overall),
    mean_score = mean(sustainability_overall),
    max_score = max(sustainability_overall)
  )

# 36. Create a function to filter for items that are both budget-friendly, eco-friendly, and have a high sustainability score
budget_eco_score_filter <- function(data, lower, eco, score){
  
  data_budget_eco_score <- data %>%
    filter(price >= lower, eco_friendly == eco, sustainability_score >= score)
    
  # return the filtered data
  return (data_budget_eco_score)
}

# 37. Filter the data frame for items that are both budget-friendly, eco-friendly, and have a high sustainability score
data_budget_eco_score <- budget_eco_score_filter(data_gear, lower = 20, eco = TRUE, score = 8)

# 38. Create a summary table of the filtered data
table_budget_eco_score <- data_budget_eco_score %>%
  select(item_category, item_name, eco_friendly, sustainability_score, sustainability_overall, price) %>%
  group_by(item_category) %>%
  summarise(
    count = n(),
    mean_score = mean(sustainability_score),
    mean_