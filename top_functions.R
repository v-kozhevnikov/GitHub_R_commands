library(readr)
library(magrittr)
library(tidyr)
library(dplyr)
library(stringr)
library(bigrquery)

#getting files from Google BigQuery

big_query_data <- "certain-torus-206419"

sql <- "SELECT content FROM [certain-torus-206419:Github.content_copy]"

destination_table <- "certain-torus-206419:Github.r_2"

content_r <- query_exec(sql, project = big_query_data, useLegacySql = FALSE,
                      destination_table = destination_table, max_pages = Inf)


#getting packages used in the code

content_r <- content_r %>%
  mutate(lib_content = str_extract_all(content, "(library\\([^\\(]+\\)|library \\([^\\(]+\\)|loadNamespace\\([^\\(]+\\)|loadNamespace \\([^\\(]+\\)|require\\([^\\(]+\\)|require \\([^\\(]+\\)|requireNamespace\\([^\\(]+\\)|requireNamespace \\([^\\(]+\\))"))

libraries <- content_r[,2] %>%
  unnest(lib_content)

libraries$lib_content <- gsub(
  ",.*$", "",
  str_replace_all(
    str_extract(libraries$lib_content,  "\\([^()]+\\)"),
    c("\"" = "", "\'" = "", "\\(" = "", "\\)" = "")
  )
)

#counting the usage of packages

packages_100 <- libraries %>%
  group_by(lib_content) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(100)

write_csv(packages_100, "data/top_100_packages.csv")


#visualisation of top 50 packages

packages_100 %>%
  arrange(desc(count)) %>%
  head(40) %>%
  ggplot() +
  geom_bar(aes(reorder(lib_content, count), count), stat = "identity", fill = "lightblue") +
  labs(x = "R packages", y = "Usage on Github") +
  coord_flip()

#checking which packages are not installed in my R

inst_pack <- as.data.frame(installed.packages()[, 1])
colnames(inst_pack) <- c("Package")
inst_pack$Package <- as.character(inst_pack$Package)

missing <- anti_join(packages_100, inst_pack, by = c("lib_content" = "Package" ))

#insalling packages, done manually to control the process

#adding packages to the environment
lapply(packages_100$lib_content, require, character.only = TRUE)

#finding all installed packages with their functions
functions <- sapply(search(), ls)

# turning "functions" into a table with the names of packages

functions <- setNames(unlist(functions, use.names=F),rep(names(functions), lengths(functions)))

functions <- as.tibble(
  cbind(
    functions, names(functions)
    )
) %>%
  filter(!(V1 %in% c(".GlobalEnv", "package:datasets") ))

functions$V1 <- str_sub(functions$V1, 9)

# clearing original dataset to include only lines with mentioned packages

content_clear <- content_r %>%
  filter(!(lib_content == "character(0)")) %>%
  unnest(lib_content) %>%
  mutate(
    library = gsub(
      ",.*$", "",
      str_replace_all(
        str_extract(lib_content,  "\\([^()]+\\)"),
        c("\"" = "", "\'" = "", "\\(" = "", "\\)" = "")
      )
    )
  )

#getting packages for the analysis

packages_for_proj <- as.tibble(unique(functions$V1))

# filtering dataset to incluse only needed packages

content_clear_1 <- content_clear[,c(1,3)] %>%
  inner_join(packages_for_proj, by = c("library" = "value")) 

write_csv(content_clear_1, "data/content_clear_1.csv")

#splitting strings into supposedly functions


content_clear_2 <- content_clear_1

content_clear_2$content <- str_extract_all(content_clear_2$content,                                    "([a-zA-Z][a-zA-Z0-9_.]{0,43}[(])|([a-zA-Z][a-zA-Z0-9_.]{0,43}[ ][(])")


content_clear_2_unnested <- content_clear_2 %>%
  unnest(content) 

content_clear_2_unnested$content <- str_replace_all(content_clear_2_unnested$content, c(" " = "", "[(]" = ""))

content_clear_3 <- content_clear_2_unnested %>%
  group_by(library,content) %>%
  summarise(count = n()) 

package_functions <- content_clear_3 %>%
  inner_join(functions, by = c("library" = "V1", "content" = "functions")) 

#visualising top 100 functions from not-base packages

package_functions %>%
  unite(col = "Package_function",library, content, sep = "::", remove = FALSE) %>%
  arrange(desc(count)) %>%
  head(50) %>%
  ggplot()+
  geom_bar(aes(x = reorder(Package_function, count), y = count, fill = library), stat = "identity")+
  labs(x = "Functions") +
  scale_fill_brewer(palette = "Paired") +
  coord_flip()
  

#base functions

all_content <- content_r

all_content$content <- str_extract_all(all_content$content,                                    "([a-zA-Z][a-zA-Z0-9_.]{0,43}[(])|([a-zA-Z][a-zA-Z0-9_.]{0,43}[ ][(])")


all_content <- all_content %>%
  unnest(content)

all_content$content <- str_replace_all(all_content$content, c(" " = "", "[(]" = ""))

all_content <- all_content %>%
  group_by(content) %>%
  summarise(count = n())

base_functions <- functions %>%
  filter(V1 == "base")

top_base_functions <- all_content %>%
  inner_join(base_functions, by = c("content"="functions")) %>%
  arrange(desc(count)) 

colnames(top_base_functions)[3] <- "library"
top_base_functions <- top_base_functions[,c(3,1,2)]

#visualising top 50 base functions

top_100_base_functions %>%
  arrange(desc(count)) %>%
  head(100) %>%
  View()
  ggplot() +
  geom_bar(aes(x = reorder(content, count), y = count), fill = "lightblue", stat = "identity") +
  labs(x = "Functions") +
  coord_flip()

#deriving functions from all packages
  
all_top_functions <- rbind(top_base_functions,package_functions, deparse.level = 2)

all_top_functions %>%
  unite(col = "Package_function",library, content, sep = "::", remove = FALSE) %>%
  arrange(desc(count)) %>%
  head(100) %>%
  ggplot()+
  geom_bar(aes(x = reorder(Package_function, count), y = count, fill = library), stat = "identity")+
  labs(x = "Functions") +
  scale_fill_brewer(palette = "Paired") +
  coord_flip()

ggsave("top_100_functions.jpeg",height = 16)

top_2000_functions <- all_top_functions %>%
  arrange(desc(count)) %>%
  head(2000)

write_csv(top_2000_functions, "top_2000_functions.csv")
