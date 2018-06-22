library(readr)
library(magrittr)
library(tidyr)
library(dplyr)
library(stringr)

content_to_check <- read_csv("data/content_clear_1.csv")

#checking for 2 approaches
content_trial_1 <- content_to_check[1:10000,]

#first, without a bracket after

#cheking max length of a function - result is 44
max(nchar(functions$functions))

content_trial_1$content <- str_extract_all(content_trial_1$content, 
                                           "[a-zA-Z][a-zA-Z0-9_.]{0,43}")

content_trial_1 <- content_trial_1 %>%
  unnest(content) %>%
  group_by(library, content) %>%
  summarise(count = n()) %>%
  inner_join(functions, by = c("library" = "V1", "content" = "functions")) %>%
  arrange(desc(count)) %>%
  head(100)

#2nd approach

content_trial_2 <- content_to_check[1:10000,]


content_trial_2$content <- str_extract_all(content_trial_2$content, 
                                           "([a-zA-Z][a-zA-Z0-9_.]{0,43}[(])|([a-zA-Z][a-zA-Z0-9_.]{0,43}[ ][(])")

content_trial_2 <- content_trial_2 %>%
  unnest(content)

content_trial_2$content <- str_replace_all(content_trial_2$content, c(" " = "", "[(]" = ""))  

content_trial_2 <- content_trial_2 %>%
  group_by(library, content) %>%
  summarise(count = n()) %>%
  inner_join(functions, by = c("library" = "V1", "content" = "functions")) %>%
  arrange(desc(count)) %>%
  head(100)



difference <- content_trial_1 %>%
  full_join(content_trial_2, by = c("library", "content")) %>%
  mutate(difference = round((count.x - count.y)/count.y,2)) %>%
  filter(difference > 0.2) %>%
  arrange(desc(difference)) 

colnames(difference) <- c("library", "function", "Count 1 approach", "Count 2 approach", "Difference")

difference %>%
  View()

#checking other 10000 rows due to unexpected variations with h2o package


content_test_1 <- content_to_check[10000:20000,]

#first, without a bracket after



content_test_1$content <- str_extract_all(content_test_1$content, 
                                          "[a-zA-Z][a-zA-Z0-9_.]{0,43}")

content_test_1 <- content_test_1 %>%
  unnest(content) %>%
  group_by(library, content) %>%
  summarise(count = n()) %>%
  inner_join(functions, by = c("library" = "V1", "content" = "functions")) %>%
  arrange(desc(count)) %>%
  head(100)

#2nd approach

content_test_2 <- content_to_check[10000:20000,]


content_test_2$content <- str_extract_all(content_test_2$content, 
                                          "([a-zA-Z][a-zA-Z0-9_.]{0,43}[(])|([a-zA-Z][a-zA-Z0-9_.]{0,43}[ ][(])")

content_test_2 <- content_test_2 %>%
  unnest(content)

content_test_2$content <- str_replace_all(content_test_2$content, c(" " = "", "[(]" = ""))  

content_test_2 <- content_test_2 %>%
  group_by(library, content) %>%
  summarise(count = n()) %>%
  inner_join(functions, by = c("library" = "V1", "content" = "functions")) %>%
  arrange(desc(count)) %>%
  head(100)



difference2 <- content_test_1 %>%
  full_join(content_test_2, by = c("library", "content")) %>%
  mutate(difference = round((count.x - count.y)/count.y,2)) %>%
  filter(difference > 0.2) %>%
  arrange(desc(difference)) 

colnames(difference2) <- c("library", "function", "Count 1 approach", "Count 2 approach", "Difference")

difference2 %>%
  View()
