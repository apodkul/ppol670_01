library(dplyr)

### Working with API 
# Example: FDA Food Enforcement API 
# Documentation link: https://open.fda.gov/apis/food/enforcement/how-to-use-the-endpoint/
url <- 'https://api.fda.gov/'
end_point <- 'food/enforcement.json'

library(httr)
fda_call <- GET(url = stringr::str_c(url, end_point))
http_status(fda_call)

content(fda_call)

fda_call <- GET(url = stringr::str_c(url, end_point), 
                query = list(search = "distribution_pattern = 'nationwide'", 
                             limit = 10)
                )

content(fda_call, as = 'text')
content(fda_call, as = 'parsed')




### Working with an API wrapper 
#Notes: https://ropengov.github.io/eurostat/articles/website/eurostat_tutorial.html
#install.packages('restatapi')
library(restatapi)

get_eurostat_toc() %>% 
  View()

out <- get_eurostat_data(id = 'hlth_rs_bdsrg', 
                         lang = 'en', type = 'both')


### Scraping an html table 
# Example: BLS CPI Figures
library(rvest)
url <- 'https://www.bls.gov/news.release/cpi.t01.htm'

outputs <- url %>% 
  read_html()

outputs %>% 
  html_table()

outputs %>% 
  html_table(header = F) %>%
  .[[1]] %>%
  filter(!(row_number() %in% 1:2))



### Scraping Section Headers
# Example: https://www.r-bloggers.com
url <- 'https://www.r-bloggers.com'
item <- '.loop-title'

output <- url %>% 
  read_html()

output %>%
  html_elements(item)

output %>%
  html_elements(item) %>%
  html_text2()

titles <- output %>%
  html_elements(item) %>%
  html_text2()

output %>%
  html_elements(item) %>% 
  html_elements('a') %>% 
  html_attr("href") 

hyperlinks <- output %>%
  html_elements(item) %>% 
  html_elements('a') %>% 
  html_attr("href") 

file <- data.frame(titles, hyperlinks)
View(file)