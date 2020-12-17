#ライブラリの読み込み-----
library(rvest)
library(stringr)
library(tidyverse)
library(XML)

#ディレクトリの設定-----
dir_path <- paste0(getwd(), "/data/politifact")
dir.create(dir_path)

#リンクの取得-----
link <- paste("https://www.politifact.com/personalities/") #全員の一覧
page <- read_html(link)
persons_url = page %>%
  html_nodes(xpath = "//*[@id=\"top\"]/main") %>%   ## find all links
  html_nodes("a") %>%
  html_attr("href") %>% ## pull out url
  str_subset("^/personalities/")
persons_url <- persons_url[-1:-2]

persons_belongs = page %>%
  html_nodes(".c-chyron__subline") %>% ## pull out 
  html_text()
mat_urls <- bind_cols("urls" = persons_url, 
                      "belongs" = persons_belongs %>%
                        str_remove_all("\n"))

##各自のスコアシートを取得-----
#空のデータフレームを作成
fact_check_list <- data.frame("name"=NULL, "belong"=NULL, "True_percentage"=NULL, 
                              "url" = NULL, "Mostly True_percentage"=NULL,
                              "Half True_percentage"=NULL, 
                              "Mostly False_percentage"=NULL,
                              "False_percentage"=NULL,
                              "Pants on Fire_percentage"=NULL,
                              "True_cheks"=NULL,"Mostly True_cheks"=NULL,
                              "Half True_cheks"=NULL,"Mostly False_cheks"=NULL,
                              "False_cheks"=NULL,"Pants on Fire_cheks"=NULL)

##個別のページにアクセス
for(i in 1:nrow(mat_urls)){
  link_each <- paste0("https://www.politifact.com", mat_urls[i ,"urls"])
  page_each <- read_html(link_each)
  name_person <- page_each %>%
    html_node("h1.m-pageheader__title") %>%
    html_text()
  Sys.sleep(3)
  
  scorecard_per <- page_each %>% # pull out fact-check score
    html_nodes("span.m-scorecard__value") %>%
    html_text() %>%
    str_remove_all("\n0%\n|\n| |%") %>%
    as.integer() 
  scorecard_checks <- page_each %>% # number of fact checks
    html_nodes(xpath = "//*[@id=\"top\"]/main/section[4]/div/article/div") %>%
    html_nodes("a") %>%
    html_text() %>%
    str_remove_all(" Checks") %>%
    as.integer()
  
  score_names <- page_each %>% #fact check ランクの名称
    html_nodes("h4.m-scorecard__title") %>%
    html_text() %>%
    str_remove_all("\n")
  names(scorecard_per) <-  score_names %>% #fact check percentage
    paste(., "percentage", sep = "_")
  names(scorecard_checks) <- score_names %>% #number of checks
    paste(., "cheks", sep = "_")
  
  vec <- c("name" = name_person, "belong" = mat_urls$belongs[i], 
           "url" = link_each,
           scorecard_per, scorecard_checks)
  fact_check_list <- bind_rows(fact_check_list, vec)
  cat("done", i, "/", nrow(mat_urls), "\n")
}
write_csv(fact_check_list, 
          paste(dir_path, "politifact_allmem_scoresheet.csv", sep = "/"))


