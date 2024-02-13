library(conflicted)
library(tidyverse)
library(robotstxt)
library(rvest)

setup_scraping_env <- function(ROOT_LINK) {
  ROBOTS <- robotstxt(ROOT_LINK)
  SESSION <- session(ROOT_LINK)
  if (!ROBOTS$check(ROOT_LINK)) {stop(paste0("robots.txt does not allow scraping: ", ROOT_LINK))}
  SESSION <- session_jump_to(SESSION, ROOT_LINK)
  page <- read_html(SESSION)
}

get_all_links <- function(page) {
	elements <- html_elements(page, "a")
	texts <- tibble(text = html_text(elements))
	links <- tibble(link = sapply(html_attrs(elements), function(x) {return(x["href"])}))
	data <- bind_cols(texts, links)
	return(data)
}
