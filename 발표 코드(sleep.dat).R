sleep <- read.csv('C:/Sleep_Efficiency.csv')
head(sleep,2)

#결측치 drop
sleep <- na.omit(sleep)

#Bedtime, Wakeuptime drop
sleep <- subset(sleep, select = -Bedtime)
sleep <- subset(sleep, select= -Wakeup.time)
head(sleep)

str(sleep)

#smoking 가변수화: yes가 1
sleep <- transform(sleep, Smoking.status = ifelse(Smoking.status == "Yes", 1, 0))

#카페인, 알콜 관련 변수 제거 데이터 생성
no <- subset(sleep, select = -c(Caffeine.consumption, Alcohol.consumption, Smoking.status, ID, Gender, Exercise.frequency, Sleep.duration, Age))

no<-scale(no)
no
head(no)

#clustering
single_linkage <- hclust(dist(no), method = "single")
complete_linkage <- hclust(dist(no), method = "complete")
group_average <- hclust(dist(no), method = "average")

table(cutree(single_linkage, k=2))
table(cutree(complete_linkage, k=2))
table(cutree(group_average, k=2))

plot(single_linkage, main = "Single Linkage", xlab = "", sub = "")
plot(complete_linkage, main = "Complete Linkage", xlab = "", sub = "")
plot(group_average, main = "Group Average", xlab = "", sub = "")

plot(single_linkage, labels=cutree(single_linkage, k=4))
plot(no, col=cutree(complete_linkage, k=2), pch=16)
plot(no, col=cutree(group_average, k=2), pch=16)

#크게 2그룹, 작게 4~5그룹으로 나누는 것이 적절해보인다.

cutree_labels <- cutree(hc, k = 3)
plot(hc, labels = cutree_labels)


hc_complete=hclust(dist(no),method="complete")

cutree(hc_complete, k=2)
plot(no, col=cutree(hc_complete, k=2), pch=16)


#k-means적용
kmeans_2 <- kmeans(no, centers = 2); kmeans_2
kmeans_3 <- kmeans(no, centers = 3); kmeans_3
kmeans_4 <- kmeans(no, centers = 4); kmeans_4
kmeans_5 <- kmeans(no, centers = 5); kmeans_5

#kmeans를 적용했을 때 


#그룹별 평균 게산

cluster_data <- data.frame(no, Cluster = kmeans_2$cluster)

mean_alcohol <- aggregate(sleep$Alcohol.consumption ~ Cluster, data = cluster_data, FUN = mean) #알코올(0~5)
mean_caffeine <- aggregate(sleep$Caffeine.consumption ~ Cluster, data = cluster_data, FUN = mean) #카페인(0~200)
mean_smoking <- aggregate(sleep$Smoking.status  ~ Cluster, data = cluster_data, FUN = mean) #흡연(0,1)
mean_exercise <- aggregate(sleep$Exercise.frequency ~ Cluster, data = cluster_data, FUN = mean) #운동빈도(0~5)
mean_alcohol
mean_caffeine
mean_smoking
mean_exercise

#그룹 1이 수면 효율성이 높고, 딥슬립 비율이 높으며며, 라이트슬립 비율이 낮게, 중간에 자다 깨는 것이 낮게 나타난다. 
알코올 섭취량은 적고, 카페인 섭취량은 조금 더 많고(약간의 카페인 섭취량이 수면에 도움이 되는 것으로 보임), 흡연은 덜 하며, 운동 빈도가 높다.


# 그룹별 성별 분포 확인
cluster_data <- data.frame(no, Cluster = kmeans_2$cluster, Gender = sleep$Gender)
gender_counts <- table(cluster_data$Cluster, cluster_data$Gender)
table(cluster_data$Cluster, cluster_data$Gender)


#성별에 따른 수면의 질 차이는 크게 나타나지 않는 것으로 보인다. 

##############
plot(no, col = kmeans_2$cluster, pch = 16)

####
age, duration은 큰 영향이 없어(클러스터 간 차이가 별로 없음) 제외하엿음


# 그룹별 Bedtime 분류
sleep$Cluster <- kmeans_2$cluster

# Bedtime 분류 함수 정의
classify_bedtime <- function(bedtime) {
  if (as.POSIXlt(bedtime, format = "%Y-%m-%d %H:%M:%S")$hour < 1) {
    return("before midnight")
  } else {
    return("after midnight")
  }
}


# 각 클러스터에서 Bedtime 분류
sleep$Bedtime_Class <- sapply(sleep$Bedtime, classify_bedtime)

# 그룹 1과 2에서 12시 이전에 잔 사람 수
group_1_count <- sum(sleep$Cluster == 1 & sleep$Bedtime_Class == "12시 이전")
group_2_count <- sum(sleep$Cluster == 2 & sleep$Bedtime_Class == "12시 이전")

# 결과 출력
print(paste("그룹 1에서 12시 이전에 잔 사람 수:", group_1_count))
print(paste("그룹 2에서 12시 이전에 잔 사람 수:", group_2_count))
위의 코드를 실행하면, 그







# 그룹별 비율 테이블 생성
ratio_table <- table(sleep$Cluster, sleep$Bedtime_Class)
prop.table(ratio_table, margin = 1)

# 결과 출력
print(prop_table)

group_1_count <- sum(sleep$Cluster == 1 & sleep$Bedtime_Class == "before midnight")
group_2_count <- sum(sleep$Cluster == 2 & sleep$Bedtime_Class == "after midnight")

group_1_count; group_2_count


#12시 이전에 잔 사람 수가 그룹 1에서 더 많이 나타나는 것을 보아 수면시간의 질을 높이기 위해서는 12시 전에 자는 것이 좋음을 알 수 있다.




data$Cluster <- kmeans_clusters$cluster

# Bedtime 분류 함수 정의
classify_bedtime <- function(bedtime) {
  if (as.POSIXlt(bedtime, format = "%Y-%m-%d %H:%M:%S")$hour < 12) {
    return("12시 이전")
  } else {
    return("12시 이후")
  }
}

# 각 클러스터에서 Bedtime 분류
data$Bedtime_Class <- sapply(data$Bedtime, classify_bedtime)

# 그룹 1과 2에서 12시 이전에 잔 사람 수
group_1_count <- sum(no$Cluster == 1 & no$Bedtime_Class == "12시 이전")
group_2_count <- sum(no$Cluster == 2 & no$Bedtime_Class == "12시 이전")

# 결과 출력
print(paste("그룹 1에서 12시 이전에 잔 사람 수:", group_1_count))
print(paste("그룹 2에서 12시 이전에 잔 사람 수:", group_2_count))
위의 코드를 실행하면, 그








