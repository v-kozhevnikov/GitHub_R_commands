library(readr)
library(magrittr)
library(tidyr)
library(dplyr)
library(stringr)

#checking for non-uploaded packages
content_trial_3 <- content_r[1:10000,]

content_trial_3 <- content_trial_3 %>%
  mutate(lib_content = str_extract_all(content, "(library\\([^\\(]+\\)|library \\([^\\(]+\\)|loadNamespace\\([^\\(]+\\)|loadNamespace \\([^\\(]+\\)|require\\([^\\(]+\\)|require \\([^\\(]+\\)|requireNamespace\\([^\\(]+\\)|requireNamespace \\([^\\(]+\\))"))

content_trial_3 <- content_trial_3 %>%
  unnest(lib_content)

content_trial_3$lib_content <- gsub(
  ",.*$", "",
  str_replace_all(
    str_extract(content_trial_3 $lib_content,  "\\([^()]+\\)"),
    c("\"" = "", "\'" = "", "\\(" = "", "\\)" = "")
  )
)

content_trial_3$content <- str_extract_all(content_trial_3$content,                                    "([a-zA-Z][a-zA-Z0-9_.]{0,43}[(])|([a-zA-Z][a-zA-Z0-9_.]{0,43}[ ][(])")

content_trial_3 <- content_trial_3 %>%
  unnest(content)

content_trial_3$content <- str_replace_all(content_trial_3$content, c(" " = "", "[(]" = ""))

#checking with packages

content_trial_3_packs <- content_trial_3 %>%
  group_by(lib_content, content) %>%
  summarise(count = n()) %>%
  inner_join(functions, by = c("lib_content" = "V1", "content" = "functions")) %>%
  arrange(desc(count)) %>%
  head(1000) 

#checking without packages
content_trial_3_no_packs <- content_trial_3 %>%
  group_by(content) %>%
  summarise(count = n()) %>%
  inner_join(functions, by = c("content" = "functions")) %>%
  arrange(desc(count)) %>%
  head(1000) 


difference_without_packages <- content_trial_3_packs %>%
  full_join(content_trial_3_no_packs, by = "content") 

colnames(difference_without_packages) <- c("Package", "Function", "Count with packages", "Count without packages", "Package Option")

View(difference_without_packages)
    
