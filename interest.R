# 0. 패키지 로드 -------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  tidytext,
  KoNLP, 
  lubridate, scales, gridExtra,
  tidylo, rvest,
  tm, furrr, topicmodels,
  ggpmisc # 시계열 시각화
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
           str_replace_all("\\..*|\n| |(.*| )ⓒ", ""),
         ref = paste0("(", press, ", ", date, ")"))%>% # . 뒷부분 모두 제거 & \n 태그 제거 & ⓒ 앞부분 모두 제거 
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

data_prep_ver2 %>% 
  filter(!title %>% str_detect("북한|중국|해외|북핵|외교|국제")) %>% # 1,742 건
  filter(!title %>% str_detect("수상|회고록|In&Out|왜냐면")) %>% # 1,731 건
  filter(title %>% str_detect("김영란")) %>% 
  write_excel_csv("data_prep_kim.csv")

# 2.3. 데이터 정제: 기사제목 기준 필터링 (내용분석) ---------------------

data_prep_ver3 %>% # 584 건 중 내용분석을 통해 걸러내기
  filter(!title %>% str_detect("특임검사|파워우먼|공직열전|보관신탁|휴직|인재개발|분쟁조정 심의|파워인터뷰")) -> data_prep_ver4
data_prep_ver4 # 576 건
data_prep_ver4 %>% write_excel_csv("data_prep_ver4.csv")

# 2.4. 데이터 정제: 본문 태그 필터링 ------------------------------

data_prep_ver4 %>% 
  mutate(
    content = content %>% 
      str_replace_all("☞.*", "") %>%
      str_replace_all("▶.*", "") %>%
      str_replace_all("★.*", "") %>%
      str_replace_all("이 시각 인기뉴스.*", "") %>% 
      str_replace_all("\\<.*\\>", "") %>%
      str_replace_all("◆.*", "") %>% 
      str_replace_all("※.*", "") %>% 
      str_replace_all("(Copyright|ⓒ).*", "") %>% # 저작권 표기 제거
      str_replace_all("김태희 몰디브.*", "") %>% # 광고 태그
      str_replace_all("■ 장준하.*", "") %>% 
      str_replace_all("\\[(한겨레|내일신문)\\]", "") %>% 
      str_replace_all("경향신문|세계일보|공식 SNS|게티이미지뱅크|연합뉴스", "") %>% 
      str_replace_all("오늘의 핫뉴스|모바일 경향.*|GoodNews paper.*", "") %>%
      str_replace_all("맛있는 정보! 신선한 뉴스!.*", "") %>% 
      str_replace_all("한국일보는 투기 제보를 기다리고 있습니다.*|■ MB, ‘노무현 4주기’에 1박2일 골프…논란 확산.*", "") %>% 
      str_replace_all("\\[.*\\]", "") %>% 
      str_replace_all("[a-zA-Z0-9]{1,}@.*", "") %>% 
      str_replace_all("[·가-힣]{2,10}(| )(기자|특파원|차장|논설위원|정치부장|디지털부문장|대기자)", "") %>% 
      str_replace_all("김규원|남도영|이석우|손영옥|김재중|송호진|서강대 법학전문대학원 석좌교수|엄기영|성한용|김연수|고성호|곽정수|최무영·서울 강서구 화곡3동|이정우|조영달|장택동|서울|김경욱|우경임|국기연|이준협 현대경제연구원 연구위원|윤태범 한국방송통신대 행정학과 교수ㆍ참여연대 행정감시센터 실행위원.*|김상겸|대담=김용출 정치부장.*|서영지|김명국|김원철|박용현|논설위원|정형근 경희대 법학전문대학원 교수ㆍ변호사|정환봉|이준한 인천대.*|박찬구|김봉규|이종선|김명국|우향화·서울 서대문구|김경호|허정호|박민우|노지원|김지현|세종 박찬구|박석배 한국농수산식품유통공사 상임감사|세종=|세종=남건우|양민철|심우삼|박은정", "")
  ) -> data_prep_ver5

data_prep_ver5 %>% # 내용이 적은 기사 제외 (포토 등)
  filter(!content %>% str_length() <= 200) -> data_prep_ver5

data_prep_ver5 %>% write_excel_csv("data_prep_ver5.csv")

# 3. 탐색적 분석 -------------------------------------------

par(family = "AppleGothic") # 시각화를 위한 설정 1: 기본 글꼴 지정
theme_set(theme_gray(base_family = 'AppleGothic')) # 시각화를 위한 설정 2: plot용 글꼴 지정

# 3.1. 신문사별 기사 수 --------------------------------------

data_prep_ver5 %>% # 544 건
  count(press) %>% 
  arrange(desc(n))
# # A tibble: 11 x 2
#    press        n
#    <chr>    <int>
#  1 서울신문    97
#  2 세계일보    92
#  3 한겨레      62
#  4 문화일보    60
#  5 동아일보    57
#  6 한국일보    55
#  7 경향신문    53
#  8 국민일보    52
#  9 조선일보     7
# 10 내일신문     5
# 11 중앙일보     4


# 3.2. 날짜별 기사 수: 월별 ---------------------------------------
data_prep_ver5 %>%
  mutate(
    year = year(date),
    month = month(date),
    quarter = lubridate::quarter(date),
    yq = paste0(year, ": ", quarter, "Q"),
    quarterly = lubridate::yq(yq))  -> data_prep_ver6

data_prep_ver6 %>% 
  count(quarterly) %>% 
  filter(n >= 30)

data_prep_ver6 %>%
  count(quarterly) %>%
  ggplot(aes(x = quarterly, y = n)) +
  geom_rect( 
            aes(xmin = as.Date("2011-01-01", "%Y-%m-%d"), 
                xmax = as.Date("2015-07-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "lightgray", 
            alpha = 0.3) +
    geom_rect( 
            aes(xmin = as.Date("2015-07-01", "%Y-%m-%d"), 
                xmax = as.Date("2021-07-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.3) +
  geom_line(alpha = 0.8, color = "darkgreen") +
  ylim(0, 150) +
  theme_minimal() +
  ggplot2::annotate("text", label = "2015-1 (n = 38)", x = as.Date("2015-01-01"), y = 48) +
  ggplot2::annotate("text", label = "2016-3 (n = 39)", x = as.Date("2016-07-01"), y = 49) +
  ggplot2::annotate("text", label = "2019-1 (n = 32)", x = as.Date("2019-01-01"), y = 40) +
  ggplot2::annotate("text", label = "2021-1 (n = 109)", x = as.Date("2021-01-01"), y = 120)


# 4. 토크나이징 --------------------------------------------

# 4.1. 토크나이징 전 전처리 ------------------------------------
data_prep_ver6 %>% 
  filter(content %>% str_detect("토지주택공사"))


data_prep_ver6 %>% 
  mutate(
    content = content %>% 
      str_replace_all("\\‘.*(이해충돌 방지법|이해충돌방지법|이해충돌 방지법안| 이해충돌방지법안).*\\’", "이해충돌방지법") %>% 
      str_replace_all("공직자윤리법|공직자 윤리법", "공직자윤리법") %>% 
      str_replace_all("국민권익위|권익위|국민권익위원회", "국민권익위원회") %>% 
      str_replace_all("국민권익위원회장|국민권익위원장|권익위장|권익위원장|국민권익위장", "국민권익위원장") %>% 
      str_replace_all("이해 충돌", "이해충돌") %>% 
      str_replace_all("이해 충돌 방지", "이해충돌방지") %>% 
      str_replace_all("지방자치단체|자치단체", "지방자치단체") %>% 
      str_replace_all("금품수수|금품 수수", "금품수수") %>% 
      str_replace_all("부정 청탁|부정청탁|부정한 청탁", "부정청탁") %>% 
      str_replace_all("국민 신문고|국민신문고", "국민신문고") %>% 
      str_replace_all("입법 취지|입법취지", "입법취지") %>% 
      str_replace_all("미공개 정보|미공개정보", "미공개정보") %>% 
      str_replace_all("퇴직 공무원|퇴직공무원", "퇴직공무원") %>% 
      str_replace_all("제3자|제삼자", "제삼자") %>% 
      str_replace_all("인사 청문회|인사청문회", "인사청문회") %>% 
      str_replace_all("사익 추구|사익추구", "사익추구") %>% 
      str_replace_all("한국토지주택공사\\(lh\\)|한국토지주택공사\\(LH\\)|한국토지주택공사|LH|LH공사|LH 공사|토지주택공사|엘에이치", "한국토지주택공사") %>% 
      str_replace_all("직무 관련성|직무관련성", "직무관련성") %>% 
      str_replace_all("[^가-힣 ]", " ") # 한글만 빼고 다 지웠다
      
  ) -> data_prep_ver6

# 4.2. 사용자 사전 탑재 --------------------------------------

user_dic <- data.frame(
  term = c('이해충돌방지법안','이해충돌방지법', "김영란법", "공직자윤리법", 
           "김영란", "국민권익위원장", "권익위", "권익위원장", "귄익위장", "행정심판위원회", "참여연대", "법제처", "부패영향평가", "국제투명성기구",
           "하태경", "이명박", "박근혜", "문재인", "이성보", "손혜원", "전현희", "이상민", "정홍원", "윤상현", "김한길", "직무관련성",
           "감사원", "청와대", "행정안전부", "법무부", "국무총리", "국무위원",
           '부정부패','부패방지', "명문화", "직권남용", "부패인식지수",
           '국회의원', "중앙부처", "뇌물죄", "금품수수", "부정청탁", "미공개정보", "퇴직공무원", "퇴직자", "대가성", "축의금", "경조사비", "인사청문회", "포퓰리즘",
           '국민의힘', '자유한국당', '민주당', "더불어민주당", "정의당", "국민의당",
           "떡값", "스폰서", "사익추구", "규정",
           "추상적", "포괄적", "이해당사자", "이해관계자",
           "지방자치단체장", 
           '코로나19'), 
  tag ='ncn')
buildDictionary(ext_dic = 'NIAdic', user_dic = user_dic)

data_prep_ver6 %>% 
  unnest_tokens(input = content,
                output = words,
                token = extractNoun,
                drop = F) %>% 
  select(-content) -> data_word

data_word %>% 
  mutate(words = words %>% 
           str_replace_all("땐", "") %>% 
           str_replace_all("하는", "") %>% 
           str_replace_all("으로부터|부터|들로부터|로부터|없어|높아|박은정|하지|하다시피|하고|우리도|하면|하기로|하러|하거나|하며|에서도|에서|에서도|에서는|하에서|에서만|에서였다|였던|였|였다는|였지만|했지만|했다|했다던가|했|했느냐가|했다지만|했는지를|했는지|했다고|했고|자는|하라고|하더라도|하라는|이라는|를|된다지만|않기로", "") %>% 
           str_replace_all("규정하", "규정") %>%
           str_replace_all("처벌조항", "처벌조항")) %>% 
  
  filter(words %>% str_length() >= 2) -> data_word_prep1

data_word_prep1 %>% 
  count(words) %>% 
  arrange(desc(n)) %>% 
  as.data.frame() %>% 
  filter(words %>% str_detect("처벌"))
