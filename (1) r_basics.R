################################################################################
# 예제 1-1 : 데이터 입력하여 data set 생성 
################################################################################

# create variables and input data
id = c(1, 2, 3, 4, 5, 6, 7) # id = 1:7
region = c('서울', '부산', '대구', '인천', '광주', '대전', '울산')
pop = c(9.7, 3.4, 2.4, 2.9, 1.5, 1.5, 1.1)
pop

# create data set
pop_2020 = data.frame(id, region, pop)


################################################################################
# 예제 1-2 : csv 파일 읽어 데이터 셋 생성
################################################################################

# Working directory 지정 : setwd("d:/TM-COSS") 또는 Session 메뉴 이용
# setwd("d:/TM-COSS")

# read file and create data set
SiDo_Pop = read.csv("SiDo_Pop.csv", header=T)

head(SiDo_Pop)


################################################################################
# 예제 1-3 : 텍스트 데이터 입력하여 data set 생성 
################################################################################

# create variables and input data
text<- "텍스트마이닝은 자연어 (Natural Language)로 구성된 비정형데이터 (unstructured data)에서 패턴 또는 관계를 추출하여 의미있는 정보를 찾아내는 기법으로, 컴퓨터가 사람들이 말하는 언어를 이해할 수 있는 자연어 처리 (Natural Language Processing)에 기반으로 둔 기술이다. 트위터, 페이스북과 같은 소셜 미디어에서 생산되는 데이터는 비정형데이터이기 때문에 텍스트마이닝을 이용하여 분석할 수 있다. 
텍스트마이닝은 말 그대로 텍스트 형태의 비정형데이터에 마이닝 기법을 적용한 것이다. 즉, 텍스트에 나타나는 단어를 분해, 정제하고, 특정 단어의 출현빈도 등을 파악하여 단어들 간의 관계를 조사하는 기법이다."

text
class(text)
nchar(text)


################################################################################
# 예제 1-4 : 파일 읽어 텍스트 데이터 셋 생성
################################################################################

# (1) 1개의 txt 파일 : line 별 read 
raw_arirang1 = readLines("arirang.txt")

raw_arirang1
class(raw_arirang1)
nchar(raw_arirang1)

# (2) 1개의 txt 파일 : line 별 read 
install.packages("readr")
library(readr)      # read_file(), read_csv(), ...

raw_arirang2 = read_file("arirang.txt")

raw_arirang2
class(raw_arirang2)
nchar(raw_arirang2)

# (3) 여러 개의 텍스트 데이터를 하나의 파일에 저장해 놓은 경우 
raw_speeches1 = read_csv("speeches.csv") # tibble 구조로 read

raw_speeches1
class(raw_speeches1)

# (4) 여러 개의 텍스트 파일을 하나의 폴더에 저장해 놓은 경우 
data.location <- "D:/TM-COSS/KOR" 

library(tm)         # VCorpus(), DirSource()
raw_speeches  <- VCorpus(DirSource(data.location))

# 사전처리
speeches <- c(rep(NA),10)
for (i in 1:10) {
  speeches[i] <- paste(raw_speeches[[i]][1], collapse="")
  speeches[i] <- gsub("[\"]", "", speeches[i])
  speeches[i] <- gsub("[\r\t\n]", "", speeches[i])
  speeches[i] <- gsub("[,]", "", speeches[i])
  speeches[i] <- gsub("c", "", speeches[i])
  speeches[i] <- gsub("\\(", " ", speeches[i])
  speeches[i] <- gsub("\\)", " ", speeches[i])
}

no = 1:10
president = c("박정희", "전두환", "노태우", "김영삼", "김대중", "노무현", "이명박", "박근혜", "문재인", "윤석열")

library(dplyr)
raw_speeches2 <- data_frame(no=no, president=president, speech=speeches)

raw_speeches2
class(raw_speeches2)


################################################################################
# 예제 1-5 : Data set 다루기 (특정한 행/열 이용, 새로운 data set 생성 등)
################################################################################

# read file and create data set
SiDo_Pop = read.csv("SiDo_Pop.csv", header=T)
head(SiDo_Pop)

# 데이터 셋의 행/열의 수
nr = nrow(SiDo_Pop)
nc = ncol(SiDo_Pop)

# 데이터(변수) 요약 정보
summary(SiDo_Pop)

### data set_name[행, 열]
# 특정한 변수 이용
SiDo_Pop[, 5]  # total은 열의 5번째에 위치한 변수
SiDo_Pop$total # $ 기호 활용하면 편리
summary(SiDo_Pop$total)	#summary(SiDo_Pop[, 5])

# chain/pipe operator : 어떤 작업에서 여러 단계의 절차를 필요로 할 때, 
# 중간 결과를 생성한 후 그 결과를 후속 절차에서 받아서 사용하는 경우에 유용
library(dplyr)   # to use %>%, select(), as_tibble(), mutate(), count(), filter(), ...    
                 # package for Data Manipulation
m = SiDo_Pop %>% summary()
m

# 특정한 관측치 이용
SiDo_Pop[SiDo_Pop$year==2000, ]	# == 는 같음을 나타내는 비교연산자
SiDo_Pop[SiDo_Pop$year!=2000, ]


###################################################################
# data frame/set에서 일부 데이터로 구성된 새로운 data set 생성 방법

# year, region, total 변수로 구성된 새로운 data set
new_data = SiDo_Pop[, c(1, 2, 5)]
new_data = subset(SiDo_Pop, select=c(year, region, total))
new_data = SiDo_Pop %>% select(year, region, total)

# year, region, total 변수를 제거한 data set
new_data = SiDo_Pop[, -c(1, 2, 5)]
new_data = subset(SiDo_Pop, select=-c(year, region, total))
new_data = SiDo_Pop %>% select(-c(year, region, total))

# 2010년 이후의 데이터로 구성된 새로운 data set
new_data = SiDo_Pop[SiDo_Pop$year>=2010, ]
new_data = subset(SiDo_Pop, year>=2010)

# 2010 이후 데이터, (year, region, total) 변수로 구성된 새로운 data set
new_data = subset(SiDo_Pop, year>=2010, select=c(year, region, total))
new_data = SiDo_Pop %>% filter(year>=2010) %>% select(year, region, total)

# 2010년 이전, area_tag=='M'인 데이터에서 male, female 변수로 구성된 data set
new_data = subset(SiDo_Pop, year<=2010 & area_tag=='M', select=c(male, female))
new_data = SiDo_Pop %>% filter(year<=2010 & area_tag=='M') %>% select(male, female)


####################################################
# 예제1-6 : 기본 plot
####################################################

# 데이터 scale 조정
SiDo_Pop$total = round(SiDo_Pop$total/1000000, 1)
SiDo_Pop$male = round(SiDo_Pop$male/1000000, 1)
SiDo_Pop$female = round(SiDo_Pop$female/1000000, 1)

# 2020년 데이터
Pop_2020 = SiDo_Pop %>% filter(year==2020) 

# 가장 기본적인 함수: plot(x, y)
plot(Pop_2020$male, Pop_2020$female) 

# 자주 쓰는 함수
hist(Pop_2020$total)
barplot(table(Pop_2020$area_tag))


### plot 옵션 설정 및 내용 추가
# 그래프 옵션 설정
SiDo_Pop$Colour="red"
SiDo_Pop$Colour[SiDo_Pop$year==2010]="green3"
SiDo_Pop$Colour[SiDo_Pop$year==2020]="blue"

plot(SiDo_Pop$male, SiDo_Pop$female, main="Scatter plot", xlab="Male", ylab="Female",
     xlim=c(0, 10), ylim=c(0, 10), type="p", pch=20, cex=1, col=SiDo_Pop$Colour, font=3)

# 그래프에 text 추가
text(7, 8, "r = 0.95", font=3)

# 그래프에 화살표 및 text 추가

arrows(6.8, 2, 6.8, 6.5, length=0.2, angle=20)
text(6.8, 1.5, "경기도", col="red")

# 그래프에 범례(legend) 추가
legend(8, 9, legend=c("2000", "2010", "2020"), col=c("red", "green3", "blue"), pch=20)


####################################################
# 예제1-7 : ggplot 함수 활용
####################################################

### Graph 1: Basic graph function
hist(SiDo_Pop$total, breaks=7, col="red")

### Graph 2: ggplot2 package
library(ggplot2)  # to use ggplot(), geom_histogram(), annotate(), ...

# histogram
ggplot(SiDo_Pop) +
  geom_histogram(aes(x=total), bins=7, fill="red") +
  annotate("text", x=13, y=3, col = "blue", label = "경기도?")

# scatter plot
SiDo_Pop$year = as.factor(SiDo_Pop$year)

ggplot(SiDo_Pop, aes(x=male, y=female, fill=year)) +
  geom_point(na.rm=TRUE, aes(colour=year)) +
  annotate("text", x=7.5, y=7.2, label="r = 0.95") 

