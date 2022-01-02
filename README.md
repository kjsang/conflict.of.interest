# Title
공직자의 이해충돌 방지법 정책결정과정 분석: 텍스트 마이닝을 활용한 다중흐름모형의 적용  
A Study of the policy-making process of the Public Servant Conflict-of-Interest Prevention Act: The Application of the Multiple Streams Framework Using Text Mining  
  
## Abstract
This study analyzes the policy-making process of the Public Servant Conflict-of-Interest Prevention Act. This study combines the Multiple Stream Framework that has strong explanatory power with text mining for validation. This combination allows the enhancement of the validity of the qualitative analysis and analyzes the dynamics between actors that appear in the policy-making process using the network analysis method. Newspaper articles for this study were collected by web crawling. Then, word frequency analysis, weighted log odds ratio analysis, actor network analysis, and topic modeling analysis based on the latent Dirichlet allocation were performed with the statistical language R. The results identify the streams that appeared by each period, the major actors, events and issues that influence the policy-making process. The analysis is as follows. First, considering the national mood created by series of focusing events, the indicator as a sub-factor of the problem stream shows a significant gap with public perception. Second, the role of policy entrepreneurs in the process of establishing the government agendas is clear. However, the impact on the process of adopting agendas for the legislature turns out to be insignificant. Third, although certain actors are identified in the policy stream, they barely exert any influence in the problem stream or the political stream. Since there are only a few cases that utilize the quantitative data in the Multiple Stream Framework, this study contributes to the reproducibility of research and validity of analysis by applying text mining analysis to the Multiple Stream Framework with the statistical language R.
  
## Keywords 
Multiple Stream Framework, Conflict-of-Interest Prevention Act, text mining, topic modeling, R.  
  
## Author
김주상 (Kim, Jusang) 

## Requirements
본 분석은 `MacOS Big Sur(11.5.1 version)`에서 `R(4.1.1 version)` 및 `Rstudio(1.4.1717 version)`으로 진행되었음을 알립니다. `Windows` 환경의 경우 font 오류 등의 문제로 코드상 문제가 발생할 수 있습니다. `MacOS` 환경에서 구동해주시면 감사하겠습니다. 분석결과에 대한 자세한 내용은 논문 원본을 참조해주세요.

### 웹크롤링 코드


### 본 분석


### 패키지 로드

### 사용자 사전

### 데이터 불러오기

### 데이터 전처리

### 형태소 분석

### 전처리 과정 도식화

### 분석

### LDA 분석(토픽모델링)

### 네트워크 분석

### 분석결과 해석

### 감사의 말

내가 지금껏 배워온 행정학이란 어떻게 하면 좋은 나라를 만들 수 있을지 치열하게 고민하는 학문이었다. 아시타비(我是他非)와 묘서동처(猫鼠同處)가 올해의 사자성어로 선정되는 세상에서 살아가는 행정학도로서, 나란 존재가 사회를 위해 어떤 역할을 할 수 있을지 고민하지 않을 수 없었다. 그리고 그 역할에 대한 나의 대답과 이를 향한 첫 발을 이해충돌 방지법의 정책결정과정을 들여다보는 연구로 한껏 내딛어보려 한다.  
지금 이 시간 감사해야할 분들이 너무나도 많다. 먼저 관심과 애정으로 지도해주신 장현주 교수님께 감사드린다. 교수님께서는 행정학에 대해 아무것도 모르던 학부생이 행정학의 길을 걸을 수 있도록 걸음마부터 가르쳐주셨다. 항상 온 마음을 다하여 감사드린다.   
대학원 수학 과정에서도 과분한 은혜를 받았다. 대학원 생활 2년 동안 행정학의 이론적 기반을 닦을 수 있도록 아낌없이 가르쳐주신 황성돈 교수님, 논문의 ‘ㄴ’자도 모르던 학생을 옆에 두고 하나하나 가르쳐주신 장지호 교수님, 첫 학기 학술 논문을 작성하는 과정에서 함께해주시며 지도해주신 김성수 교수님, 양적 연구의 기초를 다져주신 권태형 교수님, 행정학자의 길을 걷고자 하는 부족한 학생에게 항상 애정 어린 조언을 해주신 견진만 교수님, 보직으로 바쁘신 와중에도 꼼꼼히 지도해주시고 격려해주신 곽선주 교수님께도 온 마음을 다해 감사의 마음을 올린다.  
가까이에서 격려해주시고 학업에 대한 고민에 공감해주시며 이끌어주신 이창율 박사님께 감사드린다. 더불어 자칫 외로울 수 있었던 행정학 석사과정에서 함께 고군분투 해주고 방법론 공부를 같이 해온 김초원, 문현정에게 한없는 감사를 전한다. 이들 덕분에 공부가 외롭지 않았다. 대학원 생활 동안 동병상련의 마음으로 아낌없이 격려해준 서울대 사회학과 이예진, 예일대 신학대학원 이한나 목사가 또한 큰 힘이 됐다. 정말 감사하다.   
논문 초안을 검수해준 김슬기・이동근 부부, 텍스트 마이닝 전처리 과정과 토픽모델링 분석에 있어 통찰력 있는 조언을 해주신 SK이노베이션 정병기 박사님, 영문 초록을 검수해준 학부 동기이자 내 인생 최고의 영어 선생인 박도형 통역사에게도 심심한 감사를 표한다.   
끝으로 든든한 나의 아군인 동생과 누나, 부족한 아들을 위해 아낌없이 뒷바라지 해주신 부모님, 우리집 귀염둥이 쏘미, 그리고 무엇보다 가장 나를 옆에서 지탱해준 사랑하는 주찬양에게 이 논문을 바친다.  
   
2021년 12월 30일  
이문동 국정관리연구소에서  
김 주 상   



