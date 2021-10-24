
# 0. 패키지 설치 ----------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, rvest, magrittr
)

# 1.1.링크 저장 및 만들기 --------------------------------------

"https://search.naver.com/search.naver?where=news&sm=tab_pge&query=%22%EC%9D%B4%ED%95%B4%EC%B6%A9%EB%8F%8C%EB%B0%A9%EC%A7%80%22&sort=2&photo=0&field=0&pd=3&ds=2011.01.01&de=2021.04.29&mynews=1&office_type=1&office_section_code=2&news_office_checked=1421&nso=so:r,p:from20110101to20210429,a:all&start=" -> url

PAGE <- seq(from=1,to=1001,by=10) # 시작값과 종료값을 지정해줄 수 있습니다.

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
    html_nodes(html, '#main_pack') %>% # id= 는 # 을 붙인다
      html_nodes(css = '.group_news') %>%    # class= 는 css= 를 붙인다
      html_nodes(css = '.news_info') %>%
      html_nodes('a') %>%
      html_attr('href')
  )
  news_url <- c(news_url, temp)
}
    
news_url %>% 
  as_tibble() %>% 
  filter(value %>% str_detect("news.naver.com")) -> NEWS_news1

NEWS_news1 %>% 
  rename(url = value) %>% 
  mutate(id =row_number(),
         title = "",
         content = "",
         date = "") %>% 
  select(id, date, title, content, url) -> NEWS_prep_news1

for (i in 1:dim(NEWS_prep_news1)[1]) {
  html <- read_html(as.character(NEWS_prep_news1$url[i]))
  temp_news_title   <-
    repair_encoding(html_text(html_nodes(html, '#articleTitle')), from = 'utf-8')
  temp_news_content <-
    repair_encoding(html_text(html_nodes(html, '#articleBodyContents')), from = 'utf-8')
  temp_news_date <-
    repair_encoding(html_text(html_nodes(html, css = ".t11")), from = 'utf-8')
  if (length(temp_news_title) > 0) {
    NEWS_prep_news1$title[i]   <- temp_news_title
    NEWS_prep_news1$content[i] <- temp_news_content
    NEWS_prep_news1$date[i] <- temp_news_date
  }
}
NEWS_prep_news1 -> NEWS_news1
NEWS_news1 %>% 
  write_excel_csv("news1.csv")
