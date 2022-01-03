# Title
공직자의 이해충돌 방지법 정책결정과정 분석: 텍스트 마이닝을 활용한 다중흐름모형의 적용  
A Study of the policy-making process of the Public Servant Conflict-of-Interest Prevention Act: The Application of the Multiple Streams Framework Using Text Mining  
  
## Abstract
This study analyzes the policy-making process of the Public Servant Conflict-of-Interest Prevention Act. This study combines the Multiple Stream Framework that has strong explanatory power with text mining for validation. This combination allows the enhancement of the validity of the qualitative analysis and analyzes the dynamics between actors that appear in the policy-making process using the network analysis method. Newspaper articles for this study were collected by web crawling. Then, word frequency analysis, weighted log odds ratio analysis, actor network analysis, and topic modeling analysis based on the latent Dirichlet allocation were performed with the statistical language R. The results identify the streams that appeared by each period, the major actors, events and issues that influence the policy-making process. The analysis is as follows. First, considering the national mood created by series of focusing events, the indicator as a sub-factor of the problem stream shows a significant gap with public perception. Second, the role of policy entrepreneurs in the process of establishing the government agendas is clear. However, the impact on the process of adopting agendas for the legislature turns out to be insignificant. Third, although certain actors are identified in the policy stream, they barely exert any influence in the problem stream or the political stream. Since there are only a few cases that utilize the quantitative data in the Multiple Stream Framework, this study contributes to the reproducibility of research and validity of analysis by applying text mining analysis to the Multiple Stream Framework with the statistical language R.
  
## Keywords 
Multiple Stream Framework, Conflict-of-Interest Prevention Act, text mining, topic modeling, R.  
  
## 1. Author
김주상 (Kim, Jusang) 

## 2. Requirements
본 분석은 `MacOS Big Sur(11.5.1 version)`에서 `R(4.1.1 version)` 및 `Rstudio(1.4.1717 version)`으로 진행되었음을 알립니다. `Windows` 환경의 경우 font 오류 등의 문제로 코드상 문제가 발생할 수 있습니다. `MacOS` 환경에서 구동해주시면 감사하겠습니다. 분석결과에 대한 자세한 내용은 논문 원본을 참조해주세요.

### 2.1. 웹크롤링 코드
크롤링 코드 실행을 위해 우선적으로 `conflict.of.interest.Rproj`를 실행한 후 `crawling_all.R` 을 실행해주세요.  
  
본 연구에서는 정파적 편향성을 보정하고 다양한 관점의 데이터를 확보하기 위해 경향신문, 국민일보, 내일신문, 동아일보, 문화일보, 서울신문, 세계일보, 조선일보, 중앙일보, 한겨레, 한국일보 등 `11`개 신문사를 대상으로 네이버뉴스(www.news.naver.com)에서 R 패키지인 `rvest(Wickham, 2021)`을 활용한 웹 크롤링(web-crawling)으로 지면기사 총 `1,784` 건을 수집하였습니다.  

## 3. 본 분석
본 분석 코드를 살펴보기 위해서 `conflict.of.interest.Rproj`를 실행한 후 `interest.R` 을 실행해주세요.  

### 3.1. 패키지 로드
```{r}
if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  tidytext, tidyr,
  KoNLP, 
  lubridate, scales, 
  tidylo, rvest,
  tm, furrr, topicmodels,
  ggpmisc, # 시계열 시각화
  ggraph, widyr, tidygraph
)
```

### 3.2. 사용자 사전
```{r}
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
```
### 3.3. 데이터 불러오기
```{r}
read_csv("news.csv") -> data
```
### 3.4. 데이터 전처리
```{r}
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
      
      ...
      
      ) -> data_prep_ver5
```
수집한 데이터를 기사 제목을 기준으로 1차 정제과정을 거쳐 `584` 건의 신문기사를 확보하였습니다. 이후 2차 정제과정에서는 해당 기사의 본문을 내용분석하여 이해충돌 방지법안의 제정과정과 무관한 기사를 제외하였습니다. 이후 포토기사 등 본문 내 텍스트의 비중이 타 기사와 비교하여 현저히 적은 기사 등을 글자수 기준(한글 200자 이상)으로 여과하여 제외하였습니다.  
  
### 3.5. 형태소 분석
```{r}
data_prep_ver6 %>% 
  unnest_tokens(input = content,
                output = words,
                token = extractNoun,
                drop = F) %>% 
  select(-content) -> data_word
data_word %>%  write_excel_csv("data_word.csv")
```
이해충돌 방지법의 정책결정과정을 분석하기 위해 국내 신문기사를 대상으로 텍스트 마이닝 기법을 통한 텍스트의 최소 단위인 단어를 추출하였습니다. 이를 위해 `KoNLP 패키지(Jeon & Kim, 2016)`를 활용한 한글 토큰화(tokenizing) 작업과 형태소 분석 및 전처리를 수행하였습니다. 데이터 셋 `544`건의 기사를 형태소 분석하여 명사만을 추출해 총 `106,365` 건의 단어 데이터 셋을 구성하였습니다. 숫자 및 불용어를 제거하고 빈도수를 기준(빈도수 5 이하 단어 제거)으로 전처리를 수행하여 해당 데이터를 `96,580` 개의 단어로 전환할 수 있었습니다. 이후 한 글자 단어를 제외하고 유사 단어군을 통합하는 과정을 거치는 2차 정제과정을 통해 `69,864` 개의 단어로 이루어진 최종 데이터 셋을 구성하였습니다.
  
### 3.6. 전처리 과정 도식화
전처리과정을 도식화한 결과는 다음과 같습니다.  

**데이터 정제 과정**
![_flow](https://user-images.githubusercontent.com/75797388/147895880-8e54d83d-3687-4543-8d5b-1ec5d66964fa.jpg)
  
## 4. 분석
### 4.0. 빈도분석 및 가중로그승산비 분석
```{r}
data_tf_idf %>% 
  bind_log_odds(set = period, 
                feature = words, 
                n = n) %>%
  rename(log_odds = "log_odds_weighted") -> data_tidylo
```
가중로그승산비를 `tidylo::bind_log_odds`를 통해 계산하였습니다.

```{r}
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
```
청탁금지법 시기와 이해충돌방지법 시기에서 나타난 단어들의 빈도와 가중로그승산비를 구하였습니다. 시각화 결과는 다음과 같습니다.   
**청탁금지법 시기 빈도분석 및 가중로그승산비분석 결과**  
![4 김영란법_빈도분석](https://user-images.githubusercontent.com/75797388/147895719-3359ab41-3fdd-49b4-8015-a204961ea34f.jpeg)
![6 가중로그승산비분석_김영란법](https://user-images.githubusercontent.com/75797388/147895722-ea815911-b626-4803-a868-eabe22447ab3.jpeg)
    
**이해충돌 방지법 시기 빈도분석 및 가중로그승산비분석 결과**  
![5 이해충돌방지법_빈도분석](https://user-images.githubusercontent.com/75797388/147895720-b9a72397-df0c-476c-9720-3717fa63af52.jpeg)
![7 가중로그승산비분석_이해충돌](https://user-images.githubusercontent.com/75797388/147895729-3c682a19-15f6-42ee-9a26-46790f3fd9f9.jpeg)
    
### 4.1.1. LDA 분석(토픽모델링)
```{r}
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
```
```{r}
List of 6
 $ i       : int [1:45385] 1 6 17 24 38 43 45 53 83 91 ...
 $ j       : int [1:45385] 1 1 1 1 1 1 1 1 1 1 ...
 $ v       : num [1:45385] 32 1 1 1 7 1 2 1 1 3 ...
 $ nrow    : int 544
 $ ncol    : int 2201
 $ dimnames:List of 2
  ..$ Docs : chr [1:544] "2310_2020-05-07" "1425_2016-05-31" "2074_2019-03-08" "2192_2019-08-22" ...
  ..$ Terms: chr [1:2201] "특권" "관리" "공동주택" "채용" ...
 - attr(*, "class")= chr [1:2] "DocumentTermMatrix" "simple_triplet_matrix"
 - attr(*, "weighting")= chr [1:2] "term frequency" "tf"
  
A LDA_VEM topic model with 9 topics.  
  
A tibble: 270 x 3
   topic term       beta
   <int> <chr>     <dbl>
 1     1 공직자   0.0358
 2     1 직무     0.0226
 3     1 이해충돌 0.0216
 4     1 공무원   0.0186
 5     1 업무     0.0146
 6     1 관련     0.0144
 7     1 금지     0.0123
 8     1 규정     0.0120
 9     1 주식     0.0116
10     1 심사     0.0111
… with 260 more rows
```

토픽모델링 분석을 통해 9개의 토픽을 추출하였습니다.  
  
### 4.1.2. 시기별 누적 토픽수 계산
```{r}
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
```
시기별 누적 토픽수를 연도별로 계산하였습니다. 시각화한 결과는 아래와 같습니다.
**토픽모델링 결과**   
![10 토픽모델링](https://user-images.githubusercontent.com/75797388/147895695-bde893d9-a463-4228-a74e-d63bf88d4d88.jpeg)   
**시기별 토픽별 적재 확률**   
![9 시기별토픽](https://user-images.githubusercontent.com/75797388/147895700-2c14ab81-06cd-44d2-9a13-69b379fd085a.jpeg)   
**시기별 토픽별 비중**   
![ratio](https://user-images.githubusercontent.com/75797388/147895703-70020068-ed64-4a83-a89c-08599584230d.jpeg)   



### 4.2. 네트워크 분석
```{r}
# 5.7.0. 네트워크 행위자 식별 ----------------------------------

tibble(
  words = c("경제부총리", "사회부총리","김영란", "국민권익위원회", "국민권익위원장", "행정심판위원회", "감사원", "청와대", "행정안전부", "법무부", "검찰총장",
            "하태경", "이명박", "박근혜", "문재인", "이성보", "손혜원", "전현희", "이상민", "정홍원", "윤상현", "김한길", "김태년", "이낙연", "박근혜", "박영선", "안철수", "주호영", "오세훈", "정세균", "추미애", "김병욱", "박덕흠", "성일종", "김기식", "김상조", "박병석", "심상정", "최인호", "이명박", "이완구", "나경원", "박원순", "홍남기", "김용태", "이상민", "박지원", "신동근", 
            "참여연대", "법제처", "부패영향평가", "국제투명성기구", "법안심사소위원회",
           '국민의힘', '자유한국당', '민주당', "더불어민주당", "정의당",  "국민의당", "정무위원회", "정무위원회", "새정치민주연합", "반부패정책협의회", "바른미래당", "부동산거래분석원", "새정치연합", "헌법재판소", "국토교통위", "국토교통위원회", "법제사법위원회", "국토교통부"
)) -> actor

data_network_word_prep2 %>% 
  inner_join(actor, by = "words") %>% 
  rename(actor = words) %>% 
  select(id, date, actor, period) -> data_network
data_network %>% 
  write_excel_csv("data_network.csv")


# 5.7.1. 청탁금지법 시기 행위자 네트워크 -----------------------------


data_network %>% 
  filter(period == "first") %>% 
  widyr::pairwise_count(
    item = actor,
    feature = id,
    sort = T
  ) %>% 
  filter(n >= 5) %>%
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
```
행위자 네트워크 분석을 위해 네트워크 행위자를 `actor` 변수로 구분하여 네트워크 분석을 위한 별도의 데이터 셋을 구성하였습니다.  네트워크 분석을 위해 `ggragh` 등의 패키지를 활용하였습니다. 이를 시각화한 결과는 다음과 같습니다.  
**청탁금지법 시기**   
![first_network](https://user-images.githubusercontent.com/75797388/147895668-9fc0f577-ee13-4cdd-a693-a35f4985497f.jpeg)   
**이해충돌방지법 시기**   
![second_network](https://user-images.githubusercontent.com/75797388/147895663-301ca65b-d14f-4813-8a8e-18b1449688ca.jpeg)   

### 5. 분석결과 해석
논문 참조

### 6. 감사의 말

내가 지금껏 배워온 행정학이란 어떻게 하면 좋은 나라를 만들 수 있을지 치열하게 고민하는 학문이었다. 아시타비(我是他非)와 묘서동처(猫鼠同處)가 올해의 사자성어로 선정되는 세상에서 살아가는 행정학도로서, 나란 존재가 사회를 위해 어떤 역할을 할 수 있을지 고민하지 않을 수 없었다. 그리고 그 역할에 대한 나의 대답과 이를 향한 첫 발을 이해충돌 방지법의 정책결정과정을 들여다보는 연구로 한껏 내딛어보려 한다.  
지금 이 시간 감사해야할 분들이 너무나도 많다. 먼저 관심과 애정으로 지도해주신 장현주 교수님께 감사드린다. 교수님께서는 행정학에 대해 아무것도 모르던 학부생이 행정학의 길을 걸을 수 있도록 걸음마부터 가르쳐주셨다. 항상 온 마음을 다하여 감사드린다.   
대학원 수학 과정에서도 과분한 은혜를 받았다. 대학원 생활 2년 동안 행정학의 이론적 기반을 닦을 수 있도록 아낌없이 가르쳐주신 황성돈 교수님, 논문의 ‘ㄴ’자도 모르던 학생을 옆에 두고 하나하나 가르쳐주신 장지호 교수님, 첫 학기 학술 논문을 작성하는 과정에서 함께해주시며 지도해주신 김성수 교수님, 양적 연구의 기초를 다져주신 권태형 교수님, 행정학자의 길을 걷고자 하는 부족한 학생에게 항상 애정 어린 조언을 해주신 견진만 교수님, 보직으로 바쁘신 와중에도 꼼꼼히 지도해주시고 격려해주신 곽선주 교수님께도 온 마음을 다해 감사의 마음을 올린다.  
가까이에서 격려해주시고 학업에 대한 고민에 공감해주시며 이끌어주신 이창율 박사님께 감사드린다. 더불어 자칫 외로울 수 있었던 행정학 석사과정에서 함께 고군분투 해주고 방법론 공부를 같이 해온 김초원, 문현정에게 한없는 감사를 전한다. 이들 덕분에 공부가 외롭지 않았다. 대학원 생활 동안 동병상련의 마음으로 아낌없이 격려해준 서울대 사회학과 이예진, 예일대 신학대학원 이한나 목사가 또한 큰 힘이 됐다. 정말 감사하다.   
논문 초안을 검수해준 김슬기・이동근 부부, 텍스트 마이닝 전처리 과정과 토픽모델링 분석에 있어 통찰력 있는 조언을 해주신 SK이노베이션 정병기 박사님, 영문 초록을 검수해준 학부 동기이자 내 인생 최고의 영어 선생인 박도형 통역사에게도 심심한 감사를 표한다.   
끝으로 든든한 나의 아군인 동생과 누나, 부족한 아들을 위해 아낌없이 뒷바라지 해주신 부모님, 우리집 귀염둥이 쏘미, 그리고 무엇보다 가장 나를 옆에서 지탱해준 사랑하는 주찬양에게 이 논문을 바친다.  




