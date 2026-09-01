### KO: Download Frankfurt URLs ###

#### 1. Preamble ####
packages = c("httr2", "stringr", "curl")
library("groundhog")
groundhog.library(packages, "2026-08-26")

#Parameters
user_agent = "AcademicResearchScraper"
page_url_template = paste0("https://www.cashmarket.deutsche-boerse.com",
                           "/cash-de/Daten-Tech/statistiken/Zertifikate-und-Optionsscheine/",
                           "tagesstatistik-boerse-frankfurt-zertifikate-und-optionsscheine/",
                           "4098942!search?pageNum={page}&hitsPerPage=50&sort=sDate%20desc")
pages = 64
#Note: pages can be adjusted if one does not wish to download data dating back until 2014
#(which is not necessary for the replication of the results in the paper).
#For the replication, it suffices to download data starting March 20, 2023.
#Check https://www.cashmarket.deutsche-boerse.com/cash-de/Daten-Tech/statistiken/Zertifikate-und-Optionsscheine/tagesstatistik-boerse-frankfurt-zertifikate-und-optionsscheine
#and set the number of files per page to 50 to figure out how many pages you need to download 
#(In August 2026, pages = 64 downloads data dating back until 2014).

#Functions
extract_links = function(txt) {
  txt  = gsub("\\\\/", "/", txt)
  hits = str_extract_all(txt, "/resource/blob/\\d+/[0-9a-fA-F]+/data/[^\"'[:space:]<>)]+")[[1]]
  hits = unique(hits[!is.na(hits) & nzchar(hits)])
  if (length(hits) == 0) return(character(0))
  paste0("https://www.cashmarket.deutsche-boerse.com", hits)
}

#### 2. List of links to excel files ####
link_list = list()
for (i in 1:pages) {
  url = gsub("\\{page\\}", i-1, page_url_template)
  resp = request(url) |>
    req_user_agent(user_agent) |>
    req_retry(max_tries = 4, backoff = ~ 2^.x) |>
    req_perform()
  links = extract_links(resp_body_string(resp))
  links = links[substr(links, nchar(links) - 3, nchar(links)) == ".xls"]
  link_list[[i]] = links
  print(i)
  Sys.sleep(runif(1, 2, 5))
  rm(url, resp, links)
}
links_master = unlist(link_list)

#### 3. Save list of URLs ####
write.table(links_master, "Data/Frankfurt/Frankfurt_URLs.csv", row.names = FALSE, col.names = FALSE)

