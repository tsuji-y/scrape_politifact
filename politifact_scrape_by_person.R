##個人のスコアシートを全て読み込みたい時に使うやつ
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
fact_check_list <- data.frame("name"=NA, "belong"=NA, "True_percentage"=NA, 
                              "url" = NA, "MostlyTrue_percentage"=NA,
                              "HalfTrue_percentage"=NA, 
                              "MostlyFalse_percentage"=NA,
                              "False_percentage"=NA,
                              "PantsOnFire_percentage"=NA,
                              "True_cheks"=NA,"MostlyTrue_cheks"=NA,
                              "HalfTrue_cheks"=NA,"MostlyFalse_cheks"=NA,
                              "False_cheks"=NA,"PantsOnFire_cheks"=NA)

##個別のページにアクセス
for(i in 1:nrow(mat_urls)){
  link_each <- paste0("https://www.politifact.com", mat_urls[i ,"urls"])
  page_each <- tryCatch(read_html(link_each), 
                        error = function(e) e)
  Sys.sleep(2)
  if(!inherits(page_each, "error")){
    #REAL WORK
    name_person <- page_each %>%
      html_node("h1.m-pageheader__title") %>%
      html_text()
    
    scorecard_per <- page_each %>% #fact check スコア(%)
      html_nodes(".m-scorecard__value") %>%
      html_nodes("span")%>%
      html_text() %>%
      as.integer()
    
    scorecard_checks <- page_each %>% #fact check チェック数
      html_nodes(".m-scorecard__checks") %>%
      html_nodes("a") %>%
      html_text() %>%
      str_remove_all("\\W| |Checks")
    
    vec <- c(name_person, mat_urls$belongs[i], link_each,
                      scorecard_per, scorecard_checks)
    fact_check_list <- rbind(fact_check_list, vec)
    cat("done", i, "/", nrow(mat_urls), "\n")
  }else{
    #IF ERROR OCCURS
    cat("EMPTY PAGE: done", i, "/", nrow(mat_urls), "\n")
  }
}
fact_check_list <- fact_check_list[-1,] #1行目にNA入れちゃったから削除
write_csv(fact_check_list, 
          paste(dir_path, "politifact_allmem_scoresheet.csv", sep = "/"))

