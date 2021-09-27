library(dplyr)

### Working with API 
# Example: COVID-19 API 
# Documentation link: https://covid19-api.com/docs
url <- 'https://covid19-api.com'
end_point <- '/country/all'

library(httr)
covid_call <- GET(url = stringr::str_c(url, end_point),
                  query = list(format = 'json'))
status_code(covid_call)

content(covid_call)

content(covid_call, as = 'text')
content(covid_call, as = 'parsed')

data.frame(Reduce(rbind, 
                  content(covid_call, as = 'parsed'
                          )
                  )
           )



### Working with an API wrapper 
#Notes: https://ropengov.github.io/eurostat/articles/website/eurostat_tutorial.html
#install.packages('eurostat')
library(eurostat)

get_eurostat_toc() %>% 
  View()

out <- get_eurostat_json(id = 'hlth_rs_bdsrg', 
                  lang = 'en', type = 'both')


### Scraping an html table 
# Example: https://www.realclearpolitics.com/epolls/other/president-biden-job-approval-7320.html
library(rvest)
url <- 'https://www.realclearpolitics.com/epolls/other/president-biden-job-approval-7320.html'

outputs <- url %>% 
  read_html()

outputs %>% 
  html_table()

html_table(outputs)[[4]]


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