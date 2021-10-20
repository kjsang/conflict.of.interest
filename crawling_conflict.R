
# 패키지 설치 ----------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse
)

readxl::read_xlsx("bigkinds.xlsx") -> bigkinds # 1149 건의 언론기사 수집
bigkinds %>% 
  select(일자, 언론사, 제목, 키워드, URL) %>% 
  mutate(id = 1:length(제목),
         date = 일자,
         press = 언론사,
         title = 제목,
         keyword = 키워드,
         url = URL) %>% 
  mutate(press = press %>% as.factor()) %>% 
  select(id, date, press, title, keyword, url) -> base # 언론사를 펙터로 변경


