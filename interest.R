# 0. 패키지 로드 -------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  tidytext, tidyr,
  KoNLP, 
  lubridate, scales, 
  tidylo, rvest,
  tm, furrr, topicmodels,
  ggpmisc, # 시계열 시각화
  ggraph, # 네트워크 분석
  widyr, # 단어쌍 
  tidygraph
)
citation("rvest")

RStudio.Version() # R Studio 버전 확인
useNIADic() # 사전 불러오기: NIAdic 사용

# 1. 데이터 불러오기 --------------------------------------------
read_csv("news.csv") -> data # 데이터 불러오기: 전체 언론기사 중 지면기사 수집

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
data_prep_ver2 %>% write_excel_csv("data_prep_ver2.csv")

# 2.2. 데이터 정제: 기사제목 기준 필터링 (키워드) ----------------------------

data_prep_ver2 %>% 
  filter(!title %>% str_detect("북한|중국|해외|북핵|외교|국제")) %>% # 1,742 건
  filter(!title %>% str_detect("수상|회고록|In&Out|왜냐면")) %>% # 1,731 건
  filter(title %>% str_detect("이해충돌|이해관계|공무원|청탁|공직|뇌물|부패|비리|공정|LH|특권|관피아")) -> data_prep_ver3 # 584 건
data_prep_ver3 %>% write_excel_csv("data_prep_ver3.csv") # 중간 저장

# 2.3. 데이터 정제: 기사제목 기준 필터링 (내용분석) ---------------------

data_prep_ver3 %>% # 584 건 중 내용분석을 통해 걸러내기
  filter(!title %>% str_detect("특임검사|파워우먼|공직열전|보관신탁|휴직|인재개발|분쟁조정 심의|파워인터뷰")) -> data_prep_ver4
data_prep_ver4 # 573 건
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
  filter(!content %>% str_length() <= 200) -> data_prep_ver5 # 포토기사 등 제거 544 건 

data_prep_ver5 %>% write_excel_csv("data_prep_ver5.csv")

# 3. 탐색적 분석 -------------------------------------------

read_csv("data_prep_ver5.csv") -> data_prep_ver5

par(family = "AppleGothic") # 시각화를 위한 설정 1: 기본 글꼴 지정
theme_set(theme_gray(base_family = 'AppleGothic')) # 시각화를 위한 설정 2: plot용 글꼴 지정

# 3.1. 신문사별 기사 수 --------------------------------------

data_prep_ver5 %>% # 544 건 -> 최종 데이터셋
  count(press) %>% 
  arrange(desc(n)) -> data_prep_신문사별
data_prep_신문사별

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
                xmax = as.Date("2015-04-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "lightgray", 
            alpha = 0.3) +
    geom_rect( 
            aes(xmin = as.Date("2015-04-01", "%Y-%m-%d"), 
                xmax = as.Date("2021-07-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.3) +
  geom_line(alpha = 0.8, color = "darkgreen") +
  ylim(0, 150) +
  theme_minimal(base_family = 'AppleGothic') +
  ggplot2::annotate("text", label = "김영란법 시기", x = as.Date("2013-04-01"), y = 140, family = 'AppleGothic') +
ggplot2::annotate("text", label = "이해충돌방지법 시기", x = as.Date("2018-07-01"), y = 140, family = 'AppleGothic') +
  ggplot2::annotate("text", label = "2015-1 (n = 38)", x = as.Date("2015-01-01"), y = 45) +
  ggplot2::annotate("text", label = "2016-3 (n = 39)", x = as.Date("2016-07-01"), y = 52) +
  ggplot2::annotate("text", label = "2019-1 (n = 32)", x = as.Date("2019-01-01"), y = 40) +
  ggplot2::annotate("text", label = "2021-1 (n = 109)", x = as.Date("2020-10-01"), y = 120) +
  xlab("분기") +
  ylab("기사 빈도수")  -> data_prep_분기별빈도
data_prep_분기별빈도

# 4. 토크나이징 --------------------------------------------
# 4.1. 토크나이징 전 전처리 ------------------------------------

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
      str_replace_all("공공 분야|공공분야", "공공분야") %>% 
      str_replace_all("전수 조사|전수조사", "전수조사") %>% 
      str_replace_all("소급 적용|소급적용", "소급적용") %>% 
      str_replace_all("미공개 정보|미공개정보", "미공개정보") %>% 
      str_replace_all("퇴직 공무원|퇴직공무원", "퇴직공무원") %>% 
      str_replace_all("법사위", "법제사법위원회") %>% 
      str_replace_all("법안소위|법안심사 소위|법안 심사 소위|법안심사소위", "법안심사소위") %>% 
      str_replace_all("정무 위원회|정무위|정무위원회", "정무위원회") %>% 
      str_replace_all("국토교통위|국토교통위원회", "국토교통위원회") %>% 
      str_replace_all("제3자|제삼자", "제삼자") %>% 
      str_replace_all("인사 청문회|인사청문회", "인사청문회") %>% 
      str_replace_all("사익 추구|사익추구", "사익추구") %>% 
      str_replace_all("한국토지주택공사\\(lh\\)|한국토지주택공사\\(LH\\)|한국토지주택공사|LH|LH공사|LH 공사|토지주택공사|엘에이치", "한국토지주택공사") %>% 
      str_replace_all("직무 관련성|직무관련성", "직무관련성") %>% 
      str_replace_all("[^가-힣 ]", " ") # 한글만 빼고 다 지웠다
  ) -> data_prep_ver6

# 4.2. 사용자 사전 --------------------------------------
# 4.2.1. 빅카인즈 참고 --------------------------------------

readxl::read_excel("bigkinds.xlsx") -> bigkinds
bigkinds %>% 
  select(`인물`, `기관`) %>% 
  rename(person = `인물`,
         inst = `기관`) -> bigkinds_prep 

bigkinds_prep %>% 
  select(person) %>% 
  unnest_tokens(word, person, token = stringr::str_split, pattern = ",") %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  filter(str_length(word) >= 3) %>% 
  filter(n >= 30) %>% 
  as.data.frame()

bigkinds_prep %>% 
  select(inst) %>% 
  unnest_tokens(word, inst, token = stringr::str_split, pattern = ",") %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  filter(str_length(word) >= 3) %>% 
  filter(n >= 30) %>% 
  as.data.frame()



# 4.2.2. 사용자 사전 탑재 ------------------------------------

user_dic <- data.frame(
  term = c('이해충돌방지법안','이해충돌방지법', "김영란법", "공직자윤리법", "이해충돌", "전수조사", "공공분야", "소급적용", "경제부총리", "사회부총리",
           "김영란", "국민권익위원회", "국민권익위원장", "행정심판위원회", "참여연대", "법제처", "부패영향평가", "국제투명성기구",
           "하태경", "이명박", "박근혜", "문재인", "이성보", "손혜원", "전현희", "이상민", "정홍원", "윤상현", "김한길", "직무관련성", "김태년", "이낙연", "박근혜", "박영선", "안철수", "주호영", "오세훈", "정세균", "추미애", "김병욱", "박덕흠", "성일종", "김기식", "김상조", "박병석", "심상정", "최인호", "이명박", "이완구", "나경원", "박원순", "홍남기", "김용태", "이상민", "박지원", "신동근", 
           "감사원", "청와대", "행정안전부", "법무부", "국무총리", "국무위원", "검찰총장",
           '부정부패','부패방지', "명문화", "직권남용", "부패인식지수",
           '국회의원', "중앙부처", "뇌물죄", "금품수수", "부정청탁", "미공개정보", "퇴직공무원", "퇴직자", "대가성", "축의금", "경조사비", "인사청문회", "포퓰리즘",
           '국민의힘', '자유한국당', '민주당', "더불어민주당", "정의당", "국민의당", "원내대표", "정무위원회", "국회법", "여의도", "임직원", "법무부", "상임위", "정무위", "정무위원회", "사립학교", "법안소위", "새정치민주연합", "반부패정책협의회", "바른미래당", "국정조사", "부동산거래분석원", "새정치연합", "헌법재판소", "국토교통위", "국토교통위원회", "법제사법위원회", "법제사법위", "국토교통부",
           "떡값", "스폰서", "사익추구", "규정",
           "추상적", "포괄적", "이해당사자", "이해관계자",
           "지방자치단체장", 
           '코로나19'), 
  tag ='ncn')
buildDictionary(ext_dic = 'NIAdic', user_dic = user_dic)

# 4.3. 토크나이징 ------------------------------------------

data_prep_ver6 %>% 
  unnest_tokens(input = content,
                output = words,
                token = extractNoun,
                drop = F) %>% 
  select(-content) -> data_word
data_word %>%  write_excel_csv("data_word.csv") # 중간저장.. 개빡친다!


# 4.4. 사후 전처리 -----------------------------------------
read_csv("data_word.csv") -> data_word
data_word %>% # 106,365 단어 추출
  mutate(words = words %>% 
           str_replace_all("땐|하는|으로부터|부터|들로부터|로부터|없어|높아|박은정|하지|하다시피|하고|우리도|하면|하기로|하러|하거나|하며|에서도|에서|에서도|에서는|하에서|에서만|에서였다|였던|였|였다는|였지만|했지만|했다|했다던가|했|했느냐가|했다지만|했는지를|했는지|했다고|했고|자는|하라고|하더라도|하라는|이라는|를|된다지만|않기로|안하다|하기|하자|되기|되길|만큼|만여|내놨다|않았다|됐다|으로|스스로|는|에|경우|들이|만원|제외|이후|생각|당시|제외|가지|가운데|하나|개월|지난달|산관|한국일보|일보", "") %>% # 불용어 제거
           str_replace_all("규정하", "규정") %>% 
           str_replace_all("등처벌조항|처벌조항도", "처벌조항") %>% 
           str_replace_all("보장하", "보장") %>% 
           str_replace_all("인식하", "인식") %>% 
           str_replace_all("투자하", "투자") %>% 
           str_replace_all("한목소리로", "한목소리") %>% 
           str_replace_all("한국토지주택공사(와|사태)", "한국토지주택공사") %>% 
           str_replace_all("대통령의", "대통령") 
         ) -> data_word_prep1
data_word_prep1 %>% 
  anti_join(data_word_prep1 %>% 
              count(words) %>% 
              filter(!n >= 5) %>% # 빈도수 5 이상의 단어만 추출
              select(words),
            by = "words") %>% # 96,580 단어
  filter(words %>% str_length() >= 2) %>% 
  mutate(
    words = ifelse(words %in% c("자유한국당", "한국당", "한국당은"), "자유한국당", words),
    words = ifelse(words %in% c("민주당", "더불어민주당"), "더불어민주당", words),
    words = ifelse(words %in% c("새누리당은", "새누리당"), "새누리당", words),
    words = ifelse(words %in% c("한국은", "한국의", "한국"), "한국", words),
    words = ifelse(words %in% c("새정치민주연합", "새정치연합"), "새정치민주연합", words),
    words = ifelse(words == "법안심사소위", "법안심사소위원회", words)
    ) -> data_word_prep2 # 최종 69,864 단어

data_word_prep2 %>% 
  count(words) %>% 
  filter(words %>% str_detect("권익"))

data_word_prep2 %>% write_excel_csv("data_final.csv")


# 5. 분석 -----------------------------------------------

par(family = "AppleGothic") # 시각화를 위한 설정 1: 기본 글꼴 지정
theme_set(theme_gray(base_family = 'AppleGothic')) # 시각화를 위한 설정 2: plot용 글꼴 지정

# 5.0.1. 데이터 불러오기 -------------------------------------

read_csv("data_final.csv") -> data_final

# 5.0.2. 태깅: 김영란법(first)과 이해충돌방지법(second) -------------

data_final %>% 
  mutate(
    period = ifelse(id < 1228, "first", "second")
  ) -> data_final
data_final %>% count(period) -> data_final_시기별단어빈도
data_final_시기별단어빈도
# # A tibble: 2 x 2
#   period     n
#   <chr>  <int>
# 1 first  17164
# 2 second 52700

# 5.0.3. 단어의 빈도수 추이 살펴보기 ------------------------------
data_final %>% 
  count(quarterly) %>% 
  filter(n >= 5000)

data_final %>% 
  count(quarterly) %>%
  ggplot(aes(x = quarterly, y = n)) +
  geom_line(alpha = 0.8, color = "darkgreen") +
  geom_rect( 
            aes(xmin = as.Date("2011-01-01", "%Y-%m-%d"), 
                xmax = as.Date("2015-04-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "lightgray", 
            alpha = 0.3) +
    geom_rect( 
            aes(xmin = as.Date("2015-04-01", "%Y-%m-%d"), 
                xmax = as.Date("2021-07-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.3) +
  geom_line(alpha = 0.8, color = "darkgreen") +
  ylim(0, 20000) +
  theme_minimal(base_family = 'AppleGothic') +
  ggplot2::annotate("text", label = "김영란법 시기", x = as.Date("2013-04-01"), y = 18000, family = 'AppleGothic') +
ggplot2::annotate("text", label = "이해충돌방지법 시기", x = as.Date("2018-07-01"), y = 18000, family = 'AppleGothic') +
  ggplot2::annotate("text", label = "2015-1 (n = 5,646)", x = as.Date("2015-01-01"), y = 6200) +
  ggplot2::annotate("text", label = "2016-3 (n = 5,696)", x = as.Date("2016-07-01"), y = 7200) +
  ggplot2::annotate("text", label = "2019-1 (n = 5,081)", x = as.Date("2019-01-01"), y = 5800) +
  ggplot2::annotate("text", label = "2021-1 (n = 16,035)", x = as.Date("2020-09-01"), y = 17000) +
  xlab("분기") +
  ylab("단어 빈도수") -> data_final_단어추이 # 단어 추이도 나쁘지 않다!
data_final_단어추이

# 5.1. 법안 통과 관련 빈도분석 ----------------------------------

data_final %>% # 법안 통과 관련
  filter(words %in% c("입법", "법안", "통과")) %>% 
  count(quarterly) %>% 
  filter(n >= 150)

data_final %>% # 법안 통과 관련
  filter(words %in% c("입법", "법안", "통과")) %>%
  count(quarterly) %>%
  ggplot(aes(x = quarterly, y = n)) +
  geom_rect( 
            aes(xmin = as.Date("2011-01-01", "%Y-%m-%d"), 
                xmax = as.Date("2015-04-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "lightgray", 
            alpha = 0.3) +
    geom_rect( 
            aes(xmin = as.Date("2015-04-01", "%Y-%m-%d"), 
                xmax = as.Date("2021-07-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.3) +
  geom_line(alpha = 0.8, color = "darkgreen") +
  ylim(0, 250) +
  theme_minimal(base_family = 'AppleGothic') +
  ggplot2::annotate("text", label = "김영란법 시기", x = as.Date("2013-04-01"), y = 225, family = 'AppleGothic') +
ggplot2::annotate("text", label = "이해충돌방지법 시기", x = as.Date("2018-07-01"), y = 225, family = 'AppleGothic') +
  ggplot2::annotate("text", label = "2015-1 (n = 165)", x = as.Date("2015-01-01"), y = 180) +
  ggplot2::annotate("text", label = "2021-1 (n = 185)", x = as.Date("2020-09-01"), y = 200) +
  xlab("분기") +
  ylab("단어 빈도수") -> data_final_법안통과
data_final_법안통과

# 5.2. 빈도 및 가중로그승산비 ----------------------------------------

# 5.2.1. tf-idf 계산 -------------------------------------------

data_final %>% 
  count(period, words, sort = T) %>% 
  bind_tf_idf(words, period, n) -> data_tf_idf

# 5.2.2. 가중로그승산비 계산 -----------------------------------

data_tf_idf %>% 
  bind_log_odds(set = period, 
                feature = words, 
                n = n) %>%
  rename(log_odds = "log_odds_weighted") -> data_tidylo


# 5.2.3. 단어 중심으로 데이터 통합: 김영란법 시기 -------------------------------

data_tidylo %>% 
  filter(period == "first") %>% 
  group_by(words) %>%
  summarise(
    n = sum(n, na.rm = TRUE),
    tf_idf = sum(tf_idf, na.rm = TRUE),
    log_odds = sum(log_odds, na.rm = TRUE)
  ) %>%
  arrange(desc(n)) %>%
  ungroup() -> data_final_first


# 5.2.4. 단어 중심으로 데이터 통합: 이해충돌방지법 시기 ------------------

data_tidylo %>% 
  filter(period == "second") %>% 
  group_by(words) %>%
  summarise(
    n = sum(n, na.rm = TRUE),
    tf_idf = sum(tf_idf, na.rm = TRUE),
    log_odds = sum(log_odds, na.rm = TRUE)
  ) %>%
  arrange(desc(n)) %>%
  ungroup() -> data_final_second


# 5.3. 빈도분석 시각화 ------------------------------------

# 5.3.1. 빈도분석 시각화: 김영란법 시기 -------------------

data_final_first %>% 
  mutate(words = reorder(words, n)) %>%
  slice_max(n, n = 30,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(words, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = 1.5) +
  xlab("") + ylab("") -> data_final_first_빈도분석
data_final_first_빈도분석

# 5.3.2. 빈도분석 시각화:  이해충돌방지법 시기 ------------

data_final_second %>% 
  mutate(words = reorder(words, n)) %>%
  slice_max(n, n = 30,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(words, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = 1.5) +
  xlab("") + ylab("") -> data_final_second_빈도분석
data_final_second_빈도분석


# 5.4.1. 가중로그승산비 시각화: 김영란법 시기 -------------

data_final_first %>% 
  slice_max(log_odds, n = 30, with_ties = F) %>%
  ggplot(mapping = aes(x=log_odds, 
                       y=fct_reorder(words, log_odds))) +
  geom_col() +
  geom_text(aes(label = round(log_odds, digits = 4)), hjust = 1.5) +
  xlab("") + ylab("") -> data_final_first_가중로그승산비
data_final_first_가중로그승산비

# 5.4.2. 가중로그승산비 시각화: 이해충돌방지법 시기 -------

data_final_second %>% 
  slice_max(log_odds, n = 30, with_ties = F) %>%
  ggplot(mapping = aes(x=log_odds, 
                       y=fct_reorder(words, log_odds))) +
  geom_col() +
  geom_text(aes(label = round(log_odds, digits = 4)), hjust = 1.5) +
  xlab("") + ylab("") -> data_final_second_가중로그승산비
data_final_second_가중로그승산비

# 5.5. 빈도분석 및 가중로그승산비 테이블 -----------------------------

data_final_first %>%
  mutate(log_odds = round(log_odds, digits = 4)) %>%
  slice_max(n, n = 30, with_ties = F) %>%
  select(words, n) %>%
  rename(단어_1기_빈도  = words,  빈도_1기  = n) %>%
  bind_cols(
    data_final_first %>%
      mutate(log_odds = round(log_odds, digits = 4)) %>%
      slice_max(log_odds, n = 30, with_ties = F) %>%
      select(words, log_odds) %>%
      rename(단어_1기_가중로그승산비  = words,  가중로그승산비_1기  = log_odds)
  ) %>%
  bind_cols(
    data_final_second %>%
      mutate(log_odds = round(log_odds, digits = 4)) %>%
      slice_max(n, n = 30, with_ties = F) %>%
      select(words, n) %>%
      rename(단어_2기_빈도  = words,  빈도_2기  = n),
    data_final_second %>%
      mutate(log_odds = round(log_odds, digits = 4)) %>%
      slice_max(log_odds, n = 30, with_ties = F) %>%
      select(words, log_odds) %>%
      rename(단어_2기_가중로그승산비  = words,  가중로그승산비_2기  = log_odds)
  ) %>%
  write_excel_csv("table.csv")


# 5.6. 토픽모델링 ------------------------------------------


# 5.6.1. DTM 생성 -----------------------------------------

data_final %>% 
  mutate(id = paste0(id, "_", date)) %>% 
  count(id, words, sort = T) %>% 
  bind_tf_idf(words, id, n) %>% 
  cast_dtm(document = id,
           term = words,
           value = n) -> data_dtm
str(data_dtm)
topics <- c(2:15)
topics %>% 
  future_map(
    LDA, x = data_dtm, control = list(seed = 486)
    ) -> data_lda

# 5.6.2. 엘보우 생성 ---------------------------------------

tibble(
  k = topics,
  perplex = map_dbl(data_lda, perplexity)
) -> data_lda_prep

data_lda_prep %>% 
  ggplot(mapping = aes(x = k, 
                       y = perplex)) +
  geom_point() +
  geom_line() +
  ggplot2::geom_vline(xintercept = 9, size = 1, color = 'red', alpha = 0.7, linetype = 2) -> data_lda_엘보우
data_lda_엘보우

# 5.6.3. 토픽모델링: 토픽 수 9개 ------------------------------

data_lda <- LDA(data_dtm, k=9, control=list(seed=486))
data_lda %>% 
  tidy(matrix = "beta") -> data_topics
data_topics %>% write_excel_csv("data_topics.csv")
read_csv("data_topics.csv") -> data_topics
data_topics %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 30) %>%
  ungroup() %>%
  arrange(topic, -beta) -> data_topic_terms
data_topic_terms %>% write_excel_csv("topic.csv")

data_topic_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>%
  # mutate(topic = ifelse(topic == 1, "Topic1: 문제의 흐름: 공직비리",
  #                       ifelse(topic == 2, "Topic2: 김영란법",
  #                              ifelse(topic == 3, "Topic3: 정치의 흐름(국회)",
  #                                     ifelse(topic == 4, "Topic4: 정책의 흐름","Topic5: 정책의 흐름"
  #                                            # ifelse(topic == 5, "Topic5: 정책의 흐름",
  #                                            #        ifelse(topic == 6, "Topic6: 정책의 흐름","Topic7: 정책선도가(권익위)"))
  #                                            ))))) %>% 
  ggplot(mapping = aes(x = beta, 
                       y = term, 
                       fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  ylab("단어") +
  xlab("베타값(beta)") -> data_topic_토픽모델링
data_topic_토픽모델링

# 5.6.4. 시기별 누적 토픽수 -----------------------------------

data_gamma <- tidy(data_lda, matrix = "gamma")
data_gamma %>%
  separate(document, c("document", "date"), sep = "_", convert = T) %>% 
  mutate(
    year = year(date),
    month = month(date),
    quarter = lubridate::quarter(date),
    yq = paste0(year, ": ", quarter, "Q"),
    quarterly = lubridate::yq(yq),
    topic = as.factor(topic)
  ) %>%
  group_by(topic, year) %>%
  summarise(sum = sum(gamma)) %>%
  ungroup() -> data_gamma_visualization
data_gamma_visualization %>% 
  ggplot(aes(year, sum, fill = topic, label = sum)) +
  geom_bar(position = "stack", stat = "identity") -> data_gamma_시기별토픽
data_gamma_시기별토픽

# 5.7. 네트워크 분석 ----------------------------------------
data_prep_ver5 %>%
  mutate(
    year = year(date),
    month = month(date),
    quarter = lubridate::quarter(date),
    yq = paste0(year, ": ", quarter, "Q"),
    quarterly = lubridate::yq(yq))  -> data_prep_ver6

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
      str_replace_all("공공 분야|공공분야", "공공분야") %>% 
      str_replace_all("전수 조사|전수조사", "전수조사") %>% 
      str_replace_all("소급 적용|소급적용", "소급적용") %>% 
      str_replace_all("미공개 정보|미공개정보", "미공개정보") %>% 
      str_replace_all("퇴직 공무원|퇴직공무원", "퇴직공무원") %>% 
      str_replace_all("법사위", "법제사법위원회") %>% 
      str_replace_all("법안소위|법안심사 소위|법안 심사 소위|법안심사소위", "법안심사소위") %>% 
      str_replace_all("정무 위원회|정무위|정무위원회", "정무위원회") %>% 
      str_replace_all("국토교통위|국토교통위원회", "국토교통위원회") %>% 
      str_replace_all("제3자|제삼자", "제삼자") %>% 
      str_replace_all("인사 청문회|인사청문회", "인사청문회") %>% 
      str_replace_all("사익 추구|사익추구", "사익추구") %>% 
      str_replace_all("한국토지주택공사\\(lh\\)|한국토지주택공사\\(LH\\)|한국토지주택공사|LH|LH공사|LH 공사|토지주택공사|엘에이치", "한국토지주택공사") %>% 
      str_replace_all("직무 관련성|직무관련성", "직무관련성")
  ) -> data_prep_ver6

data_prep_ver6 %>% 
  mutate(period = ifelse(id < 1228, "first", "second")) %>% 
  unnest_tokens(input = content,
                output = sentences,
                token = "regex", 
                pattern = "\\.",
                drop = F) %>% 
  select(-content) -> data_sentence

data_sentence %>% 
  mutate(sentences = sentences %>% 
           str_replace_all("[^가-힣 ]", " "),
         id = 1:length(sentences)) -> data_sentence_prep1
data_sentence_prep1

user_dic <- data.frame(
  term = c('이해충돌방지법안','이해충돌방지법', "김영란법", "공직자윤리법", "이해충돌", "전수조사", "공공분야", "소급적용", "경제부총리", "사회부총리",
           "김영란", "국민권익위원회", "국민권익위원장", "행정심판위원회", "참여연대", "법제처", "부패영향평가", "국제투명성기구",
           "하태경", "이명박", "박근혜", "문재인", "이성보", "손혜원", "전현희", "이상민", "정홍원", "윤상현", "김한길", "직무관련성", "김태년", "이낙연", "박근혜", "박영선", "안철수", "주호영", "오세훈", "정세균", "추미애", "김병욱", "박덕흠", "성일종", "김기식", "김상조", "박병석", "심상정", "최인호", "이명박", "이완구", "나경원", "박원순", "홍남기", "김용태", "이상민", "박지원", "신동근", 
           "감사원", "청와대", "행정안전부", "법무부", "국무총리", "국무위원", "검찰총장",
           '부정부패','부패방지', "명문화", "직권남용", "부패인식지수",
           '국회의원', "중앙부처", "뇌물죄", "금품수수", "부정청탁", "미공개정보", "퇴직공무원", "퇴직자", "대가성", "축의금", "경조사비", "인사청문회", "포퓰리즘",
           '국민의힘', '자유한국당', '민주당', "더불어민주당", "정의당", "국민의당", "원내대표", "정무위원회", "국회법", "여의도", "임직원", "법무부", "상임위", "정무위", "정무위원회", "사립학교", "법안소위", "새정치민주연합", "반부패정책협의회", "바른미래당", "국정조사", "부동산거래분석원", "새정치연합", "헌법재판소", "국토교통위", "국토교통위원회", "법제사법위원회", "법제사법위", "국토교통부",
           "떡값", "스폰서", "사익추구", "규정",
           "추상적", "포괄적", "이해당사자", "이해관계자",
           "지방자치단체장", 
           '코로나19'), 
  tag ='ncn')
buildDictionary(ext_dic = 'NIAdic', user_dic = user_dic)

data_sentence_prep1 %>% 
  unnest_tokens(input = sentences,
                output = words,
                token = extractNoun,
                drop = F) %>% 
  select(-sentences) -> data_word_network
data_word_network %>% # 109,591 단어 추출
  mutate(words = words %>% 
           str_replace_all("땐|하는|으로부터|부터|들로부터|로부터|없어|높아|박은정|하지|하다시피|하고|우리도|하면|하기로|하러|하거나|하며|에서도|에서|에서도|에서는|하에서|에서만|에서였다|였던|였|였다는|였지만|했지만|했다|했다던가|했|했느냐가|했다지만|했는지를|했는지|했다고|했고|자는|하라고|하더라도|하라는|이라는|를|된다지만|않기로|안하다|하기|하자|되기|되길|만큼|만여|내놨다|않았다|됐다|으로|스스로|는|에|경우|들이|만원|제외|이후|생각|당시|제외|가지|가운데|하나|개월|지난달|산관|한국일보|일보", "") %>% # 불용어 제거
           str_replace_all("규정하", "규정") %>% 
           str_replace_all("등처벌조항|처벌조항도", "처벌조항") %>% 
           str_replace_all("보장하", "보장") %>% 
           str_replace_all("인식하", "인식") %>% 
           str_replace_all("투자하", "투자") %>% 
           str_replace_all("한목소리로", "한목소리") %>% 
           str_replace_all("한국토지주택공사(와|사태)", "한국토지주택공사") %>% 
           str_replace_all("대통령의", "대통령") 
         ) -> data_network_word_prep1
data_network_word_prep1 %>% 
  anti_join(data_network_word_prep1 %>% 
              count(words) %>% 
              filter(!n >= 5) %>% # 빈도수 5 이상의 단어만 추출
              select(words),
            by = "words") %>% # 96,580 단어
  filter(words %>% str_length() >= 2) %>% 
  mutate(
    words = ifelse(words %in% c("자유한국당", "한국당", "한국당은"), "자유한국당", words),
    words = ifelse(words %in% c("민주당", "더불어민주당"), "더불어민주당", words),
    words = ifelse(words %in% c("새누리당은", "새누리당"), "새누리당", words),
    words = ifelse(words %in% c("한국은", "한국의", "한국"), "한국", words),
    words = ifelse(words %in% c("새정치민주연합", "새정치연합"), "새정치민주연합", words),
    words = ifelse(words == "법안심사소위", "법안심사소위원회", words)
    ) -> data_network_word_prep2

# 5.7.0. 네트워크 행위자 식별 ----------------------------------

tibble(
  words = c("경제부총리", "사회부총리","김영란", "국민권익위원회", "국민권익위원장", "행정심판위원회", "감사원", "청와대", "행정안전부", "법무부", "검찰총장",
            "하태경", "이명박", "박근혜", "문재인", "이성보", "손혜원", "전현희", "이상민", "정홍원", "윤상현", "김한길", "김태년", "이낙연", "박근혜", "박영선", "안철수", "주호영", "오세훈", "정세균", "추미애", "김병욱", "박덕흠", "성일종", "김기식", "김상조", "박병석", "심상정", "최인호", "이명박", "이완구", "나경원", "박원순", "홍남기", "김용태", "이상민", "박지원", "신동근", 
            "참여연대", "법제처", "부패영향평가", "국제투명성기구", "법안심사소위원회",
           '국민의힘', '자유한국당', '민주당', "더불어민주당", "정의당",  "여당",  "야당", "국민의당", "정무위원회", "정무위원회", "새정치민주연합", "반부패정책협의회", "바른미래당", "부동산거래분석원", "새정치연합", "헌법재판소", "국토교통위", "국토교통위원회", "법제사법위원회", "국토교통부"
)) -> actor

data_network_word_prep2 %>% 
  inner_join(actor, by = "words") %>% 
  rename(actor = words) %>% 
  select(id, date, actor, period) -> data_network


# 5.7.1. 김영란법 시기 행위자 네트워크 -----------------------------


data_network %>% 
  filter(period == "first") %>% 
  widyr::pairwise_count(
    item = actor,
    feature = id,
    sort = T
  ) %>% 
  filter(n >= 3) %>%
  as_tbl_graph(directed = F) %>% 
  mutate(cent_dgr = centrality_degree(),
         cent_btw = centrality_betweenness(),
         cent_cls = centrality_closeness(),
         cent_egn = centrality_eigen(),
         cent_wgt = centrality_pagerank(weights = n),
         ) -> data_pairs_graph_first
data_pairs_graph_first %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n,
                     edge_width = n),
                 edge_color = "lightgray",
                 show.legend = F) +
  geom_node_point(aes(size = cent_wgt)) +
  geom_node_text(aes(label = name), 
                 repel = T,
                 point.padding = unit(0.2, "lines"), family = 'AppleGothic') +
  theme_void() -> data_network_first_시각화
data_network_first_시각화

# 5.7.2. 이해충돌방지법 시기 행위자 네트워크 --------------------------

data_network %>% 
  filter(period == "second") %>% 
  widyr::pairwise_count(
    item = actor,
    feature = id,
    sort = T
  ) %>% 
  filter(n >= 10) %>%
  as_tbl_graph(directed = F) %>% 
  mutate(cent_dgr = centrality_degree(),
         cent_btw = centrality_betweenness(),
         cent_cls = centrality_closeness(),
         cent_egn = centrality_eigen(),
         cent_wgt = centrality_pagerank(weights = n)
         ) -> data_pairs_graph_second
data_pairs_graph_second %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n,
                     edge_width = n),
                 edge_color = "lightgray",
                 show.legend = F) +
  geom_node_point(aes(size = cent_wgt)) +
  geom_node_text(aes(label = name), 
                 repel = T,
                 point.padding = unit(0.2, "lines"), family = 'AppleGothic') +
  theme_void() -> data_network_second_시각화
data_network_second_시각화

data_pairs_graph_first %>% 
  as.data.frame() %>% 
  write_excel_csv("network_first.csv")
data_pairs_graph_second %>% 
  as.data.frame() %>% 
  write_excel_csv("network_second.csv")


# 6. 최종 시각화 정리 ----------------------------------------

data_prep_신문사별
data_prep_분기별빈도

data_final_시기별단어빈도

data_final_first_빈도분석
data_final_first_가중로그승산비
data_final_second_빈도분석
data_final_second_가중로그승산비
read_csv("table.csv")

data_final_단어추이
data_final_법안통과

data_lda_엘보우
data_topic_토픽모델링
data_gamma_시기별토픽
read_csv("topic.csv")

data_network_first_시각화
data_network_second_시각화
read_csv("network_first.csv")
read_csv("network_second.csv")
