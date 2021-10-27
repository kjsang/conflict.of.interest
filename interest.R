# 0. 패키지 로드 -------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  tidytext,
  KoNLP, 
  lubridate, scales, gridExtra,
  tidylo, rvest,
  tm, furrr, topicmodels
)

RStudio.Version() # R Studio 버전 확인
useNIADic() # 사전 불러오기: NIAdic 사용

# 1. 데이터 불러오기 --------------------------------------------
read_csv("news.csv") -> data # 데이터 불러오기: 전체 언론기사 중 지면기사 수집
data %>% select(press) # press 확인


# 2. 데이터 정제 -------------------------------------------

# 2.1. 데이터 정제: 날짜 및 신문사 -------------------------------

data %>% # 총 3,148 건 데이터 중
  mutate(date = date %>% 
           str_replace_all("(오전|오후) [0-9]{1,2}:[0-9]{1,2}", "") %>% # 날짜에 붙은 시간 제거
           lubridate::as_date(), # 날짜변수를 날짜로 정제
         press = press %>%  # 신문사를
           str_replace_all("\\..*|\n| |(.*| )ⓒ", ""))%>% # . 뒷부분 모두 제거 & \n 태그 제거 & ⓒ 앞부분 모두 제거 
  filter(press %in% c("경향신문", "국민일보", "내일신문", "동아일보", "문화일보", "서울신문", "세계일보", "조선일보", "중앙일보", "한겨레", "한국일보")) -> data_prep_ver1 # 일간지만 뽑기
data_prep_ver1 %>% # 일간지만 뽑아서 1,816 건
  mutate(content = content %>%
           str_replace_all("\n\t\n\t\n\n\n\n// flash 오류를 우회하기 위한 함수 추가\nfunction _flash_removeCallback()", "") %>%
           str_replace_all("\n|\t|\\(\\)|\\{\\}", "") %>% 
           str_replace_all("\\[(동아일보|서울신문)\\]", "")) %>% 
  filter(!(year(date) == 2011 & month(date) <= 5 & day(date) <= 30) # 2011년 5월 31일 이전 기사 제외
         ) -> data_prep_ver2 # 1,784 건

# 2.2. 데이터 정제: 기사제목 기준 필터링 (키워드) ----------------------------

data_prep_ver2 
data_prep_ver2 %>% write_excel_csv("data_prep_ver2.csv")

data_prep_ver2 %>% 
  filter(!title %>% str_detect("북한|중국|해외|북핵|외교|국제")) %>% # 1,742 건
  filter(!title %>% str_detect("수상|회고록|In&Out|왜냐면")) %>% # 1,731 건
  filter(title %>% str_detect("이해충돌|이해관계|공무원|청탁|공직|뇌물|부패|비리|공정|LH|특권|관피아")) -> data_prep_ver3 # 584 건
data_prep_ver3 %>% write_excel_csv("data_prep_ver3.csv") # 중간 저장


# 2.3. 데이터 정제: 기사제목 기준 필터링 (내용분석) ---------------------

data_prep_ver3 %>% # 584 건 중 내용분석을 통해 걸러내기
  filter(!title %>% str_detect("특임검사|파워우먼|공직열전|보관신탁|휴직|인재개발")) -> data_prep_ver4
data_prep_ver4 # 576 건
data_prep_ver4 %>% write_excel_csv("data_prep_ver4.csv")


# 3. 탐색적 분석 -------------------------------------------

par(family = "AppleGothic") # 시각화를 위한 설정 1: 기본 글꼴 지정
theme_set(theme_gray(base_family = 'AppleGothic')) # 시각화를 위한 설정 2: plot용 글꼴 지정

# 3.1. 신문사별 기사 수 --------------------------------------

data_prep_ver4 %>% 
  count(press) %>% 
  arrange(desc(n))
# # A tibble: 11 x 2
#    press        n
#    <chr>    <int>
#  1 세계일보   135
#  2 서울신문   132
#  3 문화일보   130
#  4 한겨레      91
#  5 경향신문    87
#  6 동아일보    85
#  7 한국일보    81
#  8 국민일보    78
#  9 내일신문     8
# 10 조선일보     8
# 11 중앙일보     4


# 3.2. 날짜별 기사 수: 월별 ---------------------------------------
data_prep_ver4 %>%
  mutate(
    year = year(date),
    month = month(date),
    quarter = lubridate::quarter(date),
    yq = paste0(year, ": ", quarter, "Q"),
    quarterly = lubridate::yq(yq))  %>%
  count(quarterly) %>%
  ggplot(aes(x = quarterly, y = n)) +
  geom_line(alpha = 0.8, color = "darkgreen") +
  ylim(0, 150)

# data_prep_ver4 %>% 
#   mutate(
#     year = year(date),
#     month = month(date),
#     quarter = lubridate::quarter(date),
#     quarterly = paste0(year, "년_", quarter, "분기")) %>% 
#   count(quarterly) %>% 
#   ggplot(aes(x = quarterly, y = n)) +
#   geom_col() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
#   scale_x_discrete("Cut")

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
           str_replace_all("\\<.*\\>", "") %>%
           str_replace_all("뉴스1\\)|/(뉴스1|뉴스)|뉴시스】|AP/뉴시스】|뉴시스\\]|AP/뉴시스\\]|뉴시스DB\\)|newiss.com|© News1.*", "") %>%
           str_replace_all("naver.com|nate.com", "") %>%
           str_replace_all("연합뉴스(\\)|\\]| 자료사진\\])", "") %>%
           str_replace_all("zenism|sowon|photo", "") %>%
           str_replace_all("전문보기: [:/a-zA-Z0-9]{5,}", "")) -> data_prep_ver2
data_prep_ver2 %>% write_excel_csv("data_prep_ver2.csv") # 데이터 중간 저장

# read_csv("data_prep_ver1.csv") -> data_prep_ver1
# # 3. 기사 제목을 기준으로 필터링 ----------------------------------
# 
# data_prep_ver1 %>% 
#   filter(!title %>% 
#            str_detect("톱뉴스|헤드라인|<표>|주요 신문 사설|PRNewswire|\\게시판[\\]|사진톡톡") %>% # 연합뉴스 전처리 
#            str_detect("오늘의 주요일정|\\[표\\]") %>%
#            str_detect("트럼프|충남소식")
#          ) -> data_prep_ver2
#   
