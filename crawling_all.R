
# 0. 패키지 설치 ----------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, rvest, magrittr
)

# 1.1.링크 저장 및 만들기 --------------------------------------

"https://search.naver.com/search.naver?where=news&sm=tab_pge&query=%EC%9D%B4%ED%95%B4%EC%B6%A9%EB%8F%8C%EB%B0%A9%EC%A7%80&sort=2&photo=3&field=0&pd=3&ds=2011.01.01&de=2021.04.29&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:from20110101to20210429,a:all&start=" -> url
# 검색어와 기간설정 후 검색한 뒤 링크 통째로 따기

PAGE <- seq(from=1,to=3141,by=10) # (to에 검색결과창 맨 마지막 페이지 번호 입력)
naver_url_list <- c()
for (page_i in PAGE) {
  naver_url <- paste0(url,page_i)
  naver_url_list <- c(naver_url_list, naver_url)
  
}
naver_url_list

# 1.3. url 에서 관련기사 url 추출 ----------------------------------
news_url <- c()

for (page_i in PAGE) {
  naver_url <- paste0(url, page_i)
  html <- read_html(naver_url)
  temp <- unique(
    html_nodes(html, '#main_pack') %>% # id= 는 #
      html_nodes(css = '.group_news') %>% # class= 는 css= 붙이고 . 찍어주기
      html_nodes(css = '.news_info') %>%
      html_nodes('a') %>%
      html_attr('href')
  )
  news_url <- c(news_url, temp)
}
    
news_url %>% 
  as_tibble() %>% 
  filter(value %>% str_detect("news.naver.com")) -> NEWS

NEWS %>% 
  rename(url = value) %>% 
  mutate(id =row_number(),
         title = "",
         content = "",
         date = "",
         press = "") %>% 
  select(id, date, title, content, url) -> NEWS_prep

for (i in 1:dim(NEWS_prep)[1]) {
  html <- read_html(as.character(NEWS_prep$url[i]))
  temp_news_title   <- repair_encoding(html_text(html_nodes(html, '#articleTitle')), from = 'utf-8')
  temp_news_content <- repair_encoding(html_text(html_nodes(html, '#articleBodyContents')), from = 'utf-8')
  temp_news_date <- repair_encoding(html_text(html_nodes(html, css = ".t11")), from = 'utf-8')
  temp_news_press <- repair_encoding(html_text(html_nodes(html, css = ".copyright")), from = 'utf-8')
  if (length(temp_news_title) > 0) {
    NEWS_prep$title[i]   <- temp_news_title
    NEWS_prep$content[i] <- temp_news_content
    NEWS_prep$date[i] <- temp_news_date
    NEWS_prep$press[i] <- temp_news_press
  }
}
NEWS_prep -> NEWS
NEWS %>% 
  select(press)
NEWS %>% 
  write_excel_csv("news.csv")
