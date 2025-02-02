#데이터 불러오기 및 전처리
movie<-read.csv("C:/IMDb_All_Genres_etf_clean1.csv")
library(tidyverse)
movie$Total_Gross
movie <- movie[!(movie$Total_Gross)=='Gross Unkown',]
str_detect(movie$Total_Gross, 'M')
movie$Total_Gross=gsub('\\D',"",movie$Total_Gross)
movie$Total_Gross <- as.numeric(movie$Total_Gross)
movie <- movie[movie$Total_Gross != 0, ] 




#변수 분석
summary(movie$Rating) #평점 요약통계량

movie %>% ggplot(aes(x=Rating)) + geom_histogram() #평점 시각화

summary(movie$Total_Gross) #수익 요약통계량
movie %>% ggplot(aes(x=Total_Gross)) + geom_histogram() #수익 시각화
movie %>% ggplot(aes(x=log10(Total_Gross))) + geom_histogram() #로그변환 수익 시각화

summary(movie$Runtime.Mins.) #러닝타임 요약통계량
movie %>% ggplot(aes(x=Runtime.Mins.)) + geom_histogram() #러닝타임 시각화
movie %>% ggplot(aes(x=log10(Runtime.Mins.))) + geom_histogram() #로그변환 수익 시각화

# 탐구 1-평점과 수익의 상관관계 분석
cor(movie$Rating,log10(movie$Total_Gross)) #평점과 로그변환 수익의 상관계수
movie %>% ggplot(aes(x=movie$Rating, y=log10(movie$Total_Gross))) + geom_point() +geom_smooth() #평점과 로그변환 수익의 시각화

rating_gross<-lm(movie$Rating~log10(movie$Total_Gross))
summary(rating_gross) #평점과 로그변환 수익 회귀적합 
plot(rating_gross) #잔차진단


cor(movie$Rating, log10(movie$Runtime.Mins.)) #평점과 러닝타임 상관계수
movie %>% ggplot(aes(x=movie$Runtime.Mins., y=log10(movie$Rating))) + geom_point() +geom_smooth() #평점과 러닝타임 시각화 그래프

rating_run <- lm(Rating~log10(Runtime.Mins.), data=movie)
summary(rating_run)
plot(rating_run) #평점과 러닝타임 회귀적합과 잔차진단

movie %>% ggplot(aes(Year, Rating))+geom_point()+geom_line() #년도에 따른 평점

genre_rating <- movie %>%
  group_by(main_genre) %>%
  summarize(mean_rating = mean(Rating)) #장르별 평점 평균표

ggplot(data = genre_rating, aes(x = main_genre, y = mean_rating)) + geom_col() #장르에 따른 평점 평균

movie %>% mutate(main_genre=reorder(main_genre, Rating, median)) %>% ggplot(aes(main_genre, Rating)) + geom_jitter(col='gray') + geom_boxplot(alpha=.5) #장르에 따른 평점 병렬 상자 그림

rating_genre <- lm(Rating~main_genre, data=movie)
summary(rating_genre)
plot(rating_genre) #장르와 평점 회귀적합과 잔차 진단
new_movie <- subset(movie, main_genre != "Comedy")
new_model <- lm(Rating ~ main_genre, data = new_movie) #Comedy 변수를 제거한 회귀적합
summary(new_model)


top5 <- genre_rating %>%
  arrange(desc(mean_rating)) %>%
  head(5)
top <- ggplot(data = top5, aes(x = reorder(main_genre, mean_rating), y = mean_rating)) + geom_col() +  coord_flip() #상위 5개 장르의 평점

bottom5 <- genre_rating %>%
  arrange(mean_rating) %>%
  head(5)
bottom <- ggplot(data = bottom5, aes(x = reorder(main_genre, -mean_rating), y = mean_rating)) + geom_col() + coord_flip() + ylim(0, 10) #하위 5개 장르의 평점

movie %>% mutate(Director=reorder(Director, Rating, median)) %>% ggplot(aes(Director, Rating)) + geom_jitter(col='gray') + geom_boxplot(alpha=.5)+scale_x_discrete(limits=c("Woody Allen","Clint Eastwood","Steven Spielberg","Ron Howard","Martin Scorsese","Ridley Scott","Steven Soderbergh","Tim Burton","Robert Zemeckis","Oliver Stone"))+ggtitle("Rating of top10 Directors") #상위 10명 감독의 평점 

c<-lm(Rating~log10(Total_Gross)+log10(Runtime.Mins.)+main_genre,data=movie)
summary(c)
new_movie_c <- subset(movie, main_genre != "Fantasy")
new_model_c <- lm(Rating ~ log10(Total_Gross)+log10(Runtime.Mins.)+main_genre,data=new_movie_c) #Fantasy 변수를 제거한 회귀적합
summary(new_model_c)

#탐구1-수익 분석
cor(movie$Total_Gross, movie$Runtime.Mins.)
#수익과 러닝타임 상관계수
movie %>% ggplot(aes(x=log10(movie$Runtime.Mins.), movie$Total_Gross)) + geom_point() + geom_smooth() #러닝타임에 따른 수익

run_gross <- lm(Total_Gross~Runtime.Mins., data=movie)
summary(run_gross)
plot(run_gross) #러닝타임과 수익익 회귀적합과 잔차진단

movie %>% ggplot(aes(Year, Total_Gross))+geom_point()+geom_line()+scale_y_log10() #년도에 따른 수익

genre_gross <- movie %>%
  group_by(main_genre) %>%
  summarize(mean_gross = mean(Total_Gross)) #장르별 수익 평균표

movie %>% mutate(main_genre=reorder(main_genre, Total_Gross, median)) %>% 
  ggplot(aes(main_genre,Total_Gross))+ geom_jitter(col='gray') + geom_boxplot(alpha=.5) #장르에 따른 수익

gross_genre <- lm(log10(Total_Gross)~main_genre, data=movie)
summary(gross_genre)
plot(gross_genre) #선형모형 적합과 잔차진단

top5 <- genre_gross %>%
  arrange(desc(mean_gross)) %>%
  head(5)
ggplot(data = top5, aes(x = reorder(main_genre, mean_gross), y = mean_gross)) +
  geom_col() +
  coord_flip() #상위 5개 장르의 수익

bottom5 <- genre_gross %>%
  arrange(mean_gross) %>%
  head(5)
ggplot(data = bottom5, aes(x = reorder(main_genre, -mean_gross),
                           y = mean_gross)) +
  geom_col() +
  coord_flip() +
  ylim(0, 6000) #하위 5개 장르의 수익

movie %>% mutate(Director=reorder(Director, Total_Gross, median)) %>% ggplot(aes(Director, Total_Gross)) + geom_jitter(col='gray') + geom_boxplot(alpha=.5)+scale_x_discrete(limits=c("Woody Allen","Clint Eastwood","Steven Spielberg","Ron Howard","Martin Scorsese","Ridley Scott","Steven Soderbergh","Tim Burton","Robert Zemeckis","Oliver Stone"))+ggtitle("Total_Gross of top10 Directors") #상위 10명 감독에 따른 수익

탐구2-평점과 수익

head(sort(table(movie$main_genre),decreasing = TRUE),10) #가장 인기있는 장르 추출
movie %>% filter(main_genre=="Action") %>% ggplot(aes(Year, Rating, col=Total_Gross))+geom_point()+geom_line() #가장 인기있는 장르의 시간에 따른 수익과 평점의 변화 
 
op10.director <- head(sort(table(movie$Director),decreasing = TRUE),10) #상위 10명의 감독
ggplot(movie, aes(x=main_genre, y=Director, fill=Rating))+geom_tile()+scale_y_discrete(limits=c("Woody Allen","Clint Eastwood","Steven Spielberg","Ron Howard","Martin Scorsese","Ridley Scott","Steven Soderbergh","Tim Burton","Robert Zemeckis","Oliver Stone"))+scale_fill_gradient(low="yellow",high="red")+ggtitle("Heatmap of Rating by main_genre & top 10 directors") #상위 10명 감독에 따른 장르와 감독별 평점의 heatmap

ggplot(movie, aes(x=main_genre, y=Director, fill=log10(Total_Gross)))+geom_tile()+scale_y_discrete(limits=c("Woody Allen","Clint Eastwood","Steven Spielberg","Ron Howard","Martin Scorsese","Ridley Scott","Steven Soderbergh","Tim Burton","Robert Zemeckis","Oliver Stone"))+scale_fill_gradient(low="yellow",high="red")+ggtitle("Heatmap of Total_Gross by main_genre & top 10 directors") #상위 10명 감독에 따른 장르와 감독별 수익의 heatmap

ggplot(movie, aes(x=Rating, y=Total_Gross, col=main_genre))+geom_point(alpha=0.5)+scale_y_log10() #장르에 따른 평점과 수익

movie %>% filter(Year>2015) %>% ggplot(aes(x=Rating,y=Runtime.Mins.,col=main_genre, size=Total_Gross))+geom_point(alpha=.5)+facet_wrap(~Year)+scale_y_log10() # 여러 장르의 평점과 상영시간 및 수익과 최근 2016년부터 2021년까지의 변화

