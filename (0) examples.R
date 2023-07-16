# 필요한 패키지 load
library(readr)      # read_file(), read_csv(), ...
library(stringr)    # str_replace_all(), str_squish()
library(dplyr)      # %>%, select(), as_tibble(), mutate(), count(), filter(), arrange(), ...   
library(tidytext)   # unnest_tokens(), cast_dtm()   
library(tm)         # removeNumbers(), removePunctuation(), stripWhitespace(), removeWords()

library(KoNLP)      # useSejongDic(), extractNoun(), useNIADic()  
library(RcppMeCab)  # pos()

library(wordcloud)  # wordcloud()

library(ggplot2)
library(ggwordcloud)  # geom_text_wordcloud()  # ggplot()에서 워드 클라우드 만들도록 도와주는 패키지  


################################################################################
# 예제 0-1 : 텍스트 데이터에 어떤 단어들이 사용되었는가?
################################################################################
# Working directory 지정
setwd("D:/TM-COSS")


# (step 1) 원 데이터 read 
raw_arirang = read_file("arirang.txt")


# (step 2) 원 데이터를 사전처리하고 티블구조 data set으로 만드는 함수
make_DataSet <- function(raw_data) {
  data <- raw_data %>%
    str_replace_all("[^가-힣 & ^.]", " ") %>%  # 한글, . 만 남기기
    str_squish() %>%                           # 연속된 공백 제거
    as_tibble()                                # tibble로 변환
  
  return (data)
}

arirang = make_DataSet(raw_arirang)


# (step 3) 단어 단위 토큰화, 빈도수로 정렬, 두 글자 이상만 남기기
# tidytext 패키지 unnest_tokens() 함수를 이용한 토큰화 (단어, 문장 등 단위로)
to_Tokenize = function(data) {
  word_space <- data %>%
    unnest_tokens(input = value,
                  output = word,
                  token = "words")
  
  # 빈도수로 정렬, 두 글자 이상만 남기기
  word_space <- word_space %>%
    count(word, sort = T) %>%
    filter(str_count(word) > 1)
  
  return (word_space)
}

word_space = to_Tokenize(arirang)


# (step 4) 출현 빈도가 높은 단어를 추출하는 함수
get_TopFreqWord = function(word_space) {
  top20 <- word_space %>%
    head(20)               # 빈도가 높은 상위 20개 단어
  
  return (top20)
}

word_top20 = get_TopFreqWord(word_space)


# (step 5) 단어 빈도 그래프 표현
word_top20 = arrange(word_top20, n)  # 빈도 오름차순 정렬

# 막대그래프 : barplot() 이용
barplot(sort(word_top20$n, decreasing=FALSE), horiz = TRUE, names.arg = word_top20$word, cex.names=0.6, las=1)

# 막대그래프 : ggplot() 이용
make_BarChart = function(top20) {
  ggplot(top20, aes(x = reorder(word, n), y = n)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = n), hjust = -0.3) +            # 막대 밖 빈도 표시
    labs(title = "단어 빈도",                # 그래프 제목
         x = NULL, y = NULL) +                           # 축 이름 삭제
    theme(title = element_text(size = 12))               # 제목 크기
}

make_BarChart(word_top20)


# (step 6) 워드 클라우드 : ggplot() 이용
make_WordCloud = function(word_space) {
  ggplot(word_space, 
         aes(label = word, 
             size = n, 
             col = n)) +                      # 빈도에 따라 색깔 표현
    geom_text_wordcloud(seed = 1234) +  
    scale_radius(limits = c(1, NA),           # 최소, 최대 단어 빈도
                 range = c(3, 30)) +          # 최소, 최대 글자 크기
    scale_color_gradient(low = "#66aaf2",     # 최소 빈도 색깔
                         high = "#004EA1") +  # 최고 빈도 색깔
    theme_minimal()                           # 배경 없는 테마 적용
}

make_WordCloud(word_space) 



################################################################################
# 예제 0-2 : 두 문서의 단어 비교
################################################################################
# Working directory 지정
setwd("D:/TM-COSS/KOR")


# (step 1) 원 데이터 read 
raw_moon = read_file("19_jiMoon(2017).txt")
raw_yoon = read_file("20_syYoon(2022).txt")


# (step 2) 원 데이터를 사전처리하고 티블구조 data set으로 만듬 
make_DataSet <- function(raw_data, pname) {
  data <- raw_data %>%
    str_replace_all("[^가-힣 & ^.]", " ") %>%  # 한글, . 만 남기기
    str_replace_all("[\r\t\n]", "") %>%        # \r\t\n 제거
    str_squish() %>%                          # 연속된 공백 제거
    as_tibble() %>%                            # tibble로 변환
    mutate(president=pname)
  
  return (data)
}

moon = make_DataSet(raw_moon, "moon")
yoon = make_DataSet(raw_yoon, "yoon")


# (step 3) 데이터 합치고 단어 단위 토큰화, 빈도수 계산
to_Tokenize = function(data1, data2) {
  bindingData <- bind_rows(data1, data2) %>%
    select(president, value)
    
  word_space <- bindingData %>%
    unnest_tokens(input = value,
                  output = word,
                  token = "words")
    
  # 빈도수로 정렬, 두 글자 이상만 남기기
  word_space <- word_space %>%
    count(president, word, sort = T) %>%  # 대통령별 단어별 빈도
    filter(str_count(word) > 1)           # 두 글자 이상 추출

  return (word_space)
}

word_space = to_Tokenize(moon, yoon)


# (step 4) 출현 빈도가 높은 10개 단어만 추출
get_TopFreqWord = function(word_space) {
  top10 <- word_space %>%
    group_by(president) %>%             # word_space를 president별로 분리
    slice_max(n, n=10, with_ties = F)   # 빈도가 높은 상위 10개 단어 선택(빈도 동점 단어 제외)
  
  return (top10)
}

word_top10 = get_TopFreqWord(word_space)


# (step 5) 단어 빈도 막대그래프 표현
make_BarChart = function(top10) {
  ggplot(top10, aes(x = reorder(word, n),
                    y = n,
                    fill = president)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ president,      # 대통령별 그래프 생성
               scales="free_y")  # y축 동일하지 않음. 즉, 대통령별로 y축 생성
}

make_BarChart(word_top10)



################################################################################
# 예제 0-3 : 문서들간의 상관계수
################################################################################
# Working directory 지정
setwd("D:/TM-COSS")


# (step 1) 원 데이터(여러 명의 대통령 연설문) read
raw_speeches = read_csv("speeches.csv") # tibble 구조로 read


# (step 2) 원 데이터 사전처리 
make_DataSet <- function(raw_speeches) {
  
  # 불필요한 문자(특수문자, 한자, 공백 등) 및 공백 하나만 남기고 제거
  speeches <- raw_speeches %>%
    mutate(speech = str_replace_all(speech, "[^가-힣 & ^.]", " "),
           speech = str_squish(speech))
  
  return (speeches)
}

speeches = make_DataSet(raw_speeches)


# (step 3) 명사 단위 토큰화(KoNLP extractNoun() 함수 이용), 빈도수로 정렬, 두 글자 이상만 남기기
to_Tokenize = function(data) {
  word_space <- data %>%
    unnest_tokens(input = speech,
                  output = word,
                  token = extractNoun)
  
  # 빈도수로 정렬, 두 글자 이상만 남기기
  word_space <- word_space %>%
    count(no, president, word, sort = T) %>%
    filter(str_count(word) > 1)
  
  return (word_space)
}

word_space = to_Tokenize(speeches)


# (step 4) DTM 만들기 
make_DTM = function(word_space){
  dtm <- word_space %>%
    cast_dtm(document = president, term = word, value = n)
  
  dtm = as.matrix(dtm)
  
  return(dtm)
}

dtm = make_DTM(word_space)
nrow(dtm)
ncol(dtm)

# (step 5) 문서들의 상관계수 행렬 계산 
calc_Corr = function(dtm) {
  doc.corr <- matrix(NA, nrow=nrow(dtm), ncol=nrow(dtm))
  
  for (i in 1:nrow(dtm)) {
    for (j in 1:nrow(dtm)) {
      doc.corr[i,j] <- cor(dtm[i, ], dtm[j, ])
    }
  }
  
  rownames(doc.corr) <- colnames(doc.corr) <- rownames(dtm)
  
  return (doc.corr)
}

doc.corr = calc_Corr(dtm)
round(doc.corr, 3) #상관계수 행렬 출력
