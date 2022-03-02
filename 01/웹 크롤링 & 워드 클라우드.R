library(RSQLite)
library(KoNLP)
library(tm)
library(wordcloud)
library(httr)
library(rvest)
library(XML)

# URL 요청
Hankook <- "https://www.hankookilbo.com/" ; web3 <- GET(Hankook)
Joongang <- "https://joongang.joins.com/" ; web4 <- GET(Joongang)
Chosun <- "https://www.chosun.com/" ; web5 <- GET(Chosun)
Donga <- "https://www.donga.com/" ; web6 <- GET(Donga)
Hani <- "http://www.hani.co.kr/" ; web7 <- GET(Hani)
Khan <- "http://www.khan.co.kr/" ; web8 <- GET(Khan)

# HTML 파싱
html3 <- htmlTreeParse(web3, useInternalNodes = T, 
                      trim = T, encoding = "utf-8") ; rootNode3 <- xmlRoot(html3) 
html4 <- htmlTreeParse(web4, useInternalNodes = T, 
                       trim = T, encoding = "utf-8") ; rootNode4 <- xmlRoot(html4)
html5 <- htmlTreeParse(web5, useInternalNodes = T, 
                       trim = T, encoding = "utf-8") ; rootNode5 <- xmlRoot(html5)
html6 <- htmlTreeParse(web6, useInternalNodes = T, 
                        trim = T, encoding = "utf-8") ; rootNode6 <- xmlRoot(html6)
html7 <- htmlTreeParse(web7, useInternalNodes = T, 
                        trim = T, encoding = "utf-8") ; rootNode7 <- xmlRoot(html7)
html8 <- htmlTreeParse(web8, useInternalNodes = T, 
                        trim = T, encoding = "utf-8") ; rootNode8 <- xmlRoot(html8)

# <div class = "main_content_inner _conetent_inner">

khan <- paste(xpathSApply(rootNode8, "//div[@class='spotMain']", xmlValue),
              xpathSApply(rootNode8, "//div[@class='section_mediastory']", xmlValue),
              xpathSApply(rootNode8, "//div[@class='section_wrap']", xmlValue))
hani <- xpathSApply(rootNode7, "//div[@class='type1']", xmlValue)
dong <- xpathSApply(rootNode6, "//div[@class='headline_type2']", xmlValue)
chos <- xpathSApply(rootNode5, "//div[@class='grid grid__container grid__container-centered']", xmlValue)
joong <- paste(xpathSApply(rootNode4, "//div[@class='float_left']", xmlValue),
               xpathSApply(rootNode4, "//div[@class='list_vertical']", xmlValue))
hank <- xpathSApply(rootNode3, "//div[@class='container main']", xmlValue)[1]

# 수집한 자료 전처리
khan_pre <- gsub('[\r\n\t]', ' ', khan) ; khan_pre <- gsub('[[:punct:]]', ' ', khan_pre) # 특수문자 제거
khan_pre <- gsub('[a-z]+', ' ', khan_pre) ; khan_pre <- gsub('[A-Z]+', ' ', khan_pre) # 영문 대,소문자 제거
khan_pre <- gsub('[[:cntrl:]]', ' ', khan_pre) ; khan_pre <- gsub('\\d+', ' ', khan_pre) # 문장부호, 숫자 제거
khan_pre <- gsub('\\s+', ' ', khan_pre) ; khan_pre

hani_pre <- gsub('[\r\n\t]', ' ', hani) ; hani_pre <- gsub('[[:punct:]]', ' ', hani_pre) # 특수문자 제거
hani_pre <- gsub('[a-z]+', ' ', hani_pre) ; hani_pre <- gsub('[A-Z]+', ' ', hani_pre) # 영문 대,소문자 제거
hani_pre <- gsub('[[:cntrl:]]', ' ', hani_pre) ; hani_pre <- gsub('\\d+', ' ', hani_pre) # 문장부호, 숫자 제거
hani_pre <- gsub('\\s+', ' ', hani_pre) ; hani_pre

dong_pre <- gsub('[\r\n\t]', ' ', dong) ; dong_pre <- gsub('[[:punct:]]', ' ', dong_pre) # 특수문자 제거
dong_pre <- gsub('[a-z]+', ' ', dong_pre) ; dong_pre <- gsub('[A-Z]+', ' ', dong_pre) # 영문 대,소문자 제거
dong_pre <- gsub('[[:cntrl:]]', ' ', dong_pre) ; dong_pre <- gsub('\\d+', ' ', dong_pre) # 문장부호, 숫자 제거
dong_pre <- gsub('\\s+', ' ', dong_pre) ; dong_pre

chos_pre <- gsub('[\r\n\t]', ' ', chos) ; chos_pre <- gsub('[[:punct:]]', ' ', chos_pre) # 특수문자 제거
chos_pre <- gsub('[a-z]+', ' ', chos_pre) ; chos_pre <- gsub('[A-Z]+', ' ', chos_pre) # 영문 대,소문자 제거
chos_pre <- gsub('[[:cntrl:]]', ' ', chos_pre) ; chos_pre <- gsub('\\d+', ' ', chos_pre) # 문장부호, 숫자 제거
chos_pre <- gsub('\\s+', ' ', chos_pre) ; chos_pre

joong_pre <- gsub('[\r\n\t]', ' ', joong) ; joong_pre <- gsub('[[:punct:]]', ' ', joong_pre) # 특수문자 제거
joong_pre <- gsub('[a-z]+', ' ', joong_pre) ; joong_pre <- gsub('[A-Z]+', ' ', joong_pre) # 영문 대,소문자 제거
joong_pre <- gsub('[[:cntrl:]]', ' ', joong_pre) ; joong_pre <- gsub('\\d+', ' ', joong_pre) # 문장부호, 숫자 제거
joong_pre <- gsub('\\s+', ' ', joong_pre) ; joong_pre

hank_pre <- gsub('[\r\n\t]', ' ', hank) ; hank_pre <- gsub('[[:punct:]]', ' ', hank_pre) # 특수문자 제거
hank_pre <- gsub('[a-z]+', ' ', hank_pre) ; hank_pre <- gsub('[A-Z]+', ' ', hank_pre) # 영문 대,소문자 제거
hank_pre <- gsub('[[:cntrl:]]', ' ', hank_pre) ; hank_pre <- gsub('\\d+', ' ', hank_pre) # 문장부호, 숫자 제거
hank_pre <- gsub('\\s+', ' ', hank_pre) ; hank_pre

# 한국
news_noun3 <- extractNoun(hank_pre) ; newsCorpus3 <- Corpus(VectorSource(news_noun3))
TDM3 <- TermDocumentMatrix(newsCorpus3, control = list(wordLengths = c(4,16)))
tdm.df3 <- as.data.frame(as.matrix(TDM3)) ; dim(tdm.df3)
wordResult3 <- sort(rowSums(tdm.df3), decreasing = TRUE) ; wordResult3 <- wordResult3[wordResult3 > 1] ; head(wordResult3,50)
wordResult3 <- wordResult3[-3] ; head(wordResult3,50)
wordResult3 <- head(wordResult3, 50)
myNames3 <- names(wordResult3)
df3 <- data.frame(word=myNames3, freq=wordResult3)
pal <- brewer.pal(8, "Paired")
wordcloud(df3$word, df3$freq, min.freq=2, random.order=F, scale =c(5, 0.3),
          rot.per=0.1, colors=pal, family="malgun")

# 조선
news_noun5 <- extractNoun(chos_pre) ; newsCorpus5 <- Corpus(VectorSource(news_noun5))
TDM5 <- TermDocumentMatrix(newsCorpus5, control = list(wordLengths = c(4,16)))
tdm.df5 <- as.data.frame(as.matrix(TDM5)) ; dim(tdm.df5)
wordResult5 <- sort(rowSums(tdm.df5), decreasing = TRUE) ; wordResult5 <- wordResult5[wordResult5 > 1] ; head(wordResult5)
wordResult5<- wordResult5[-c(26,30,33,36,46)]
wordResult5 <- wordResult5/3 ; wordResult5 <- wordResult5[1:50]
myNames5 <- names(wordResult5)
df5 <- data.frame(word=myNames5, freq=wordResult5)
wordcloud(df5$word, df5$freq, min.freq=2, random.order=F, scale =c(5, 0.3),
          rot.per=0.1, colors=pal, family="malgun")

# 중앙
news_noun4 <- extractNoun(joong_pre)
newsCorpus4 <- Corpus(VectorSource(news_noun4))
TDM4 <- TermDocumentMatrix(newsCorpus4, control = list(wordLengths = c(4,16)))
tdm.df4 <- as.data.frame(as.matrix(TDM4)) ; dim(tdm.df4)
wordResult4 <- sort(rowSums(tdm.df4), decreasing = TRUE) ; wordResult4 <- wordResult4[wordResult4 > 1] ; head(wordResult4,50)
wordResult4 <- wordResult4[-c(1,2,3)] ;wordResult4 <- head(wordResult4, 50)
myNames4 <- names(wordResult4) 
df4 <- data.frame(word=myNames4, freq=wordResult4)
wordcloud(df4$word, df4$freq, min.freq=2, random.order=F, scale =c(5, 0.3),
          rot.per=0.1, colors=pal, family="malgun")

# 동아
news_noun6 <- extractNoun(dong_pre)
newsCorpus6 <- Corpus(VectorSource(news_noun6))
TDM6 <- TermDocumentMatrix(newsCorpus6, control = list(wordLengths = c(4,16)))
tdm.df6 <- as.data.frame(as.matrix(TDM6)); dim(tdm.df6)
wordResult6 <- sort(rowSums(tdm.df6), decreasing = TRUE) ; wordResult6 <- wordResult6[wordResult6 > 1] ; head(wordResult6, 50) 
wordResult6 <- head(wordResult6, 50)
myNames6 <- names(wordResult6)
df6 <- data.frame(word=myNames6, freq=wordResult6)
wordcloud(df6$word, df6$freq, min.freq=2, random.order=F, scale =c(5, 0.3),
          rot.per=0.1, colors=pal, family="malgun")

# 한겨례
news_noun7 <- extractNoun(hani_pre)
newsCorpus7 <- Corpus(VectorSource(news_noun7))
TDM7 <- TermDocumentMatrix(newsCorpus7, control = list(wordLengths = c(4,16)))
tdm.df7 <- as.data.frame(as.matrix(TDM7)) ; dim(tdm.df7)
wordResult7 <- sort(rowSums(tdm.df7), decreasing = TRUE) ; wordResult7 <- wordResult7[wordResult7 > 1] ; head(wordResult7, 50)
wordResult7 <- wordResult7[-33] ; wordResult7 <- head(wordResult7, 50) 
myNames7 <- names(wordResult7)
df7 <- data.frame(word=myNames7, freq=wordResult7)
wordcloud(df7$word, df7$freq, min.freq=2, random.order=F, scale =c(5, 0.3),
          rot.per=0.1, colors=pal, family="malgun")

# 경향 
news_noun8 <- extractNoun(khan_pre)
newsCorpus8 <- Corpus(VectorSource(news_noun8))
TDM8 <- TermDocumentMatrix(newsCorpus8, control = list(wordLengths = c(4,16)))
tdm.df8 <- as.data.frame(as.matrix(TDM8)) ; dim(tdm.df8)
wordResult8 <- sort(rowSums(tdm.df8), decreasing = TRUE) ; wordResult8 <- wordResult8[wordResult8 > 1] ; head(wordResult8, 50)
wordResult8 <- wordResult8[-c(1,10:12)] ; head(wordResult8, 50) 
names(wordResult8)[2] = "코스피"
wordResult8 <- head(wordResult8, 50)
myNames8 <- names(wordResult8)
df8 <- data.frame(word=myNames8, freq=wordResult8)
wordcloud(df8$word, df8$freq, min.freq=2, random.order=F, scale =c(5, 0.3),
          rot.per=0.1, colors=pal, family="malgun")

