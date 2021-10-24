
# 0. 패키지 로드 -------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  tidytext,
  KoNLP, lubridate, tidylo, rvest,
  tm, furrr, topicmodels
)

RStudio.Version() # R Studio 버전 확인
useNIADic() # 사전 불러오기: NIAdic 사용

# 1. 데이터 불러오기 --------------------------------------------
read_csv("YH.csv") %>% 
  mutate(press = "연합뉴스",
         id = paste0(id, "_연합뉴스")) -> YH
read_csv("news1.csv") %>% 
  mutate(press = "뉴스1",
         id = paste0(id, "_뉴스1")) -> news1
read_csv("newsis.csv") %>% 
  mutate(press = "뉴시스",
         id = paste0(id, "_뉴시스")) -> newsis
YH %>% bind_rows(news1, newsis) -> data # 세 신문사 데이터 병합


# 2. 데이터 정제: 본문 ---------------------------------------

data %>% 
  mutate(date = date %>% 
           str_replace_all("(오전|오후) [0-9]{1,2}:[0-9]{1,2}", "") %>% # 시간 제거
           lubridate::as_date()) %>% # 날짜변수를 날짜로 정제
  mutate(content = content %>% 
           str_replace_all("\n\t\n\t\n\n\n\n// flash 오류를 우회하기 위한 함수 추가\nfunction _flash_removeCallback()", "") %>% 
           str_replace_all("\n|\t|\\(\\)|\\{\\}", "") %>% 
           str_replace_all("\\([가-힣]{1,}=[가-힣]{1,}\\)", "") %>% # (서울=연합뉴스) 등 제거
           str_replace_all("[가-힣]{2,4} (기자|특파원)", "") %>% 
           str_replace_all("[a-zA-Z0-9]{1,}@yna.co.kr", "") %>% 
           str_replace_all("[a-zA-Z0-9]{1,}@news1.kr", "") %>% 
           str_replace_all("[a-zA-Z0-9]{1,}@newsis.com", "") %>% 
           str_replace_all("[a-zA-Z0-9]{1,}@", "") %>% 
           str_replace_all("\\[관련기사\\].*", "") %>% 
           str_replace_all("\\[이 시각 많이 본 기사\\].*", "") %>% 
           str_replace_all("\\[뉴시스 이시간 핫 뉴스\\].*|최신 유행 트렌드 총집결.*|기자가 기다려요.*", "") %>% 
           str_replace_all("☞.*", "") %>% 
           str_replace_all("▶.*", "") %>%
           str_replace_all("★.*", "") %>%
           str_replace_all("\\(공동취재사진\\)", "") %>% 
           str_replace_all("\\[사진 영상 제보받습니다\\].*", "") %>% 
           str_replace_all(".*=", "") %>%
           str_replace_all("뉴시스 뉴스, 네이버 뉴스.*", "") %>% 
           str_replace_all("[0-9]{4}\\.[0-9]{1,2}\\.[0-9]{1,2}", "") %>% 
           str_replace_all("[0-9]{4}\\,[0-9]{1,2}\\,[0-9]{1,2}", "") %>% 
           str_replace_all("\\[.*\\]", "") %>% 
           str_replace_all("\\<.*\\>", "") %>% 
           str_replace_all("뉴스1\\)|/(뉴스1|뉴스)|뉴시스】|AP/뉴시스】|뉴시스\\]|AP/뉴시스\\]|뉴시스DB\\)|newiss.com|© News1.*", "") %>% 
           str_replace_all("naver.com|nate.com", "") %>% 
           str_replace_all("연합뉴스(\\)|\\]| 자료사진\\])", "") %>% 
           str_replace_all("zenism|sowon|photo", "") %>% 
           str_replace_all("전문보기: [:/a-zA-Z0-9]{5,}", "")) -> data_prep_ver1
data_prep_ver1 %>% write_excel_csv("data_prep_ver1.csv") # 데이터 중간 저장


# 3. 기사 제목을 기준으로 필터링 ----------------------------------

data_prep_ver1 %>% 
  filter(!title %>% 
           str_detect("톱뉴스|헤드라인|<표>|주요 신문 사설|PRNewswire|\\게시판[\\]|사진톡톡") %>% # 연합뉴스 전처리 
           str_detect("오늘의 주요일정|\\[표\\]") %>%
           str_detect("트럼프|충남소식")
         ) -> data_prep_ver2
  
