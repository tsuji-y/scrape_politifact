library(lubridate)
library(rvest)
library(stringr)
library(tidyverse)
library(XML)

#ディレクトリの設定
dir_path <- paste0(getwd(), "/data/politifct")
dir.create(dir_path)

#politifactのスクレイピング
pageNum <- 1:3 #全体を表示できないので手動
rank <- c("true", "mostly-true", "half-true", #真実度合いのスコア化
          "mostly-false", "false", "pants-fire")
df <- data.frame("description"=NULL, "url"=NULL, "person"=NULL, "text"=NULL, "rank"=NULL)
for(i in 1:length(rank)){
  for(j in 1:(pageNum)){
    link <- paste0("https://www.politifact.com/factchecks/list/?page=", 
                   j, "&category=truth-o-meter&ruling=", rank[i])
    page <- read_html(link) #ページにアクセス
    Sys.sleep(2)
    factcheck_url = page %>%
      html_nodes("a") %>%   ## find all links
      html_attr("href") %>% ## pull out url
      str_subset("^/factchecks/\\d{4}")
    for(k in 1:length(factcheck_url)){
      link_each <- paste0("http://www.politifact.com", factcheck_url[k]) #リンクの指定
      page_each <- read_html(link_each) #fact checkページの読み込み
      Sys.sleep(2)
      txt <- page_each %>%              #statementの抽出
        html_node(".m-statement__quote") %>%
        html_text()
      person <- page_each %>%           #発言者の抽出
        html_node(".m-statement__name") %>%
        html_attr("title")
      dsc <- page_each %>%              #contextの抽出
        html_node(".m-statement__desc") %>%
        html_text()
      df <- bind_rows(df,               #データフレームに記憶
                      data.frame("description"=dsc, "url"=link_each, "person"=person, 
                           "text"=txt, "rank"=rank[i]))
    }
  }
}
write_csv(df, paste(dir_path, "politifact.csv", sep = "/"))

