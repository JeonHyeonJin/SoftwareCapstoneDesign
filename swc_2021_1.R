#install packages if they are not installed
if(!require(dplyr)) {install.packages("dplyr");library(dplyr)}
if(!require(RRF)) {install.packages("RRF");library(RRF)}
if(!require(inTrees)) {install.packages("RRF");library(inTrees)}
if(!require(ROCR)){install.packages("ROCR");library(ROCR)}
if(!require(ggplot2)){install.packages("ggplot2");library(ggplot2)}
if(!require(pROC)){install.packages("pROC");library(pROC)}
if(!require(corrplot)){install.packages("corrplot");library(corrplot)}

#read csv
d1<-read.csv('hn_18_all_ANSI.csv')
d2<-read.csv('hn_16_all_ANSI.csv')


#attribute selection

#mh_PHQ_S - PHQ-9항목 총점
#DF2_dg - 의사 우울증 진단 여부 1:있음 0: 없음
#DF2_pr - 현재 우울증 유병 여부 1: 있음 0: 없음
#DF2_pt - 우울증 치료 1:있음 0:없음 # actual_class
#DF2_ag - 우울증 진단 나이
# 설명 변수 - phq-9 9개항목, target - 현재 우울증 유병 여부
df1 <- select(d1, sex, age, BP_PHQ_1,BP_PHQ_2,BP_PHQ_3,BP_PHQ_4,
              BP_PHQ_5,BP_PHQ_6,BP_PHQ_7,BP_PHQ_8,BP_PHQ_9, mh_PHQ_S, DF2_pr)

df2 <- select(d2, sex, age,BP_PHQ_1,BP_PHQ_2,BP_PHQ_3,BP_PHQ_4,
              BP_PHQ_5,BP_PHQ_6,BP_PHQ_7,BP_PHQ_8,BP_PHQ_9, mh_PHQ_S, DF2_pr)


#16 and 18 data merge 
data<-rbind(df1, df2)
str(data)

sum(is.na(data))

#delete NA
clean_d <- na.omit(data)
str(clean_d)

# 현재 우울증 여부에서 8(미해당, 모름)지우기
clean_d <- subset(clean_d, (DF2_pr == 0 | DF2_pr == 1))
str(clean_d)

clean_d_for_cor = clean_d[,3:13]
clean_d_cor = cor(clean_d_for_cor)
corrplot(clean_d_cor,method="number")
# dataset seperating by sex, age
m_oldest <-subset(clean_d, sex==1 & (age>=65))
f_oldest <-subset(clean_d, sex==2 & (age>=65))
m<-subset(clean_d, sex==1 & (age>=19 & age<65))
f<-subset(clean_d, sex==2 & (age>=19 & age<65))

#실제 우울증인 사람 확인
m_old_D = subset(m_oldest, DF2_pr == 1)
f_old_D = subset(f_oldest, DF2_pr == 1)
m_D = subset(m, DF2_pr == 1)
f_D = subset(f, DF2_pr == 1)


# 불면증인 사람 확인 #ism : insomnia
m_old_ism  = subset(m_oldest, BP_PHQ_3 >=2)
f_old_ism  = subset(f_oldest, BP_PHQ_3 >=2)
m_ism  = subset(m, BP_PHQ_3 >=2)
f_ism  = subset(f, BP_PHQ_3 >=2)

# 노인의 cutoff는 5점, 성인은 10점을 기준으로 한다. 
# Pred_D : phq-9 점수합의 cutoff 기준으로 우울증(1), 정상(0) binary class로 나눈 attribute

#일반 성인
f$Pred_D = ifelse(f$mh_PHQ_S>=10, 1, 0) # 10점 이상을 우울증(1), 미만을 (0)으로 변경
m$Pred_D= ifelse(m$mh_PHQ_S>=10, 1, 0)

#노인
f_oldest$Pred_D = ifelse(f_oldest$mh_PHQ_S>=5, 1, 0) # 5점 이상을 우울증(1), 미만을 (0)으로 변경
m_oldest$Pred_D = ifelse(m_oldest$mh_PHQ_S>=5, 1, 0)


actual_m_old = m_oldest[['DF2_pr']]
actual_f_old = f_oldest[['DF2_pr']]
actual_m = m[['DF2_pr']]
actual_f= f[['DF2_pr']]

# 변수 타입 확인
class(actual_f) # integer
class(f$Pred_D) # numeric
# actual_값을 numeric으로 형변환해줌
as.numeric(actual_f)
as.numeric(actual_m)
as.numeric(actual_m_old)
as.numeric(actual_f_old)


# roc curve 그리기 
#true_label = actual_f
#predictions = f$Pred_D
#p = prediction(predictions, true_label)
#roc = performance(p, measure = 'tpr', x.measure = 'fpr')
#plot(roc)
#auc = performance(p, measure = 'auc')
#auc@y.values

# 그룹별로 plot 함수 만들기
#plot_roc = function(trueLabel, toPred){
#  true_label = trueLabel
#  predictions = toPred$Pred_D
#  p = prediction(predictions, true_label)
#  roc = performance(p, measure = 'tpr', x.measure = 'fpr')
#  #plot(roc, col = 'red', lwd = 2, bg = 'skyblue', print.auc=TRUE, print.auc.col='blue')
#  plot.roc(roc, col = 'blue', print.auc = TRUE, print.auc.col = 'red')
#  auc = performance(p, measure = 'auc')
#  auc@y.values
#}

# pROC로 그려보기
plot_pROC = function(trueLabel, toPred){
  true_label = trueLabel
  predictions = toPred$Pred_D
  my_roc = roc(true_label, predictions)
  plot.roc(my_roc,  
           col="red",
           print.auc=TRUE,   # auc 값출력
           max.auc.polygon=TRUE,   #auc 최대 면적 출력
           print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",# 기준치(cut-off value), point, col 설정
           auc.polygon=TRUE, auc.polygon.col="#CDECFA")   
}





plot_pROC(actual_m_old, m_oldest) #0.48 --> 분별력이 없음으로 나옴.
plot_pROC(actual_f_old, f_oldest) # 0.60
plot_pROC(actual_m, m) # 0.65
plot_pROC(actual_f, f) # 0.70

# 전체 남녀 50-80세 중,노년층
#old<-rbind(m_oldest, f_oldest, f, m)
#nrow(old)

#res<-mean(old$mh_PHQ_S)
#res
# 조건에 맞는 행 개수 출력
#length(which(old$DF2_pr == 1))

m_oldest = m_oldest[,-c(13)]
f_oldest = f_oldest[,-c(13)]
m = m[,-c(13)]
f = f[,-c(13)]

# PHQ9개, mh_PHQ_s, Pred_D만 남기기.
m_oldest<-m_oldest[,3:13]
f_oldest<-f_oldest[,3:13]
m<-m[,3:13]
f<-f[,3:13]

# mh_PHQ_S 제거 --> PHQ9개, Pred_D만 남기기.

f<-f[,-c(10)]
m<-m[,-c(10)]
f_oldest<-f_oldest[,-c(10)]
m_oldest<-m_oldest[,-c(10)]


#length(which(old$DF2_dg==1 & old$DF2_pr ==1 & old$DF2_pt==0 ))
#length(which(old$DF2_dg==1 & old$DF2_pr ==0))

# column위치를 target에 맞게 순서 변경
#m_oldest<-m_oldest[,c(1,3,2)]
#f_oldest<-f_oldest[,c(1,3,2)]
#m_old<-m_old[,c(1,3,2)]
#f_old<-f_old[,c(1,3,2)]



print_ruleM<-function(df){
  X<-df[,1:(ncol(df)-1)]
  #iris[,1:(ncol(iris)-1)]
  target <- df[,"Pred_D"] 
  rf <- RRF(X,as.factor(target),ntree=100) # build an ordinary RF 
  treeList <- RF2List(rf)
  ruleExec <- extractRules(treeList,X) # transform to R-executable rules
  ruleExec <- unique(ruleExec)
  ruleMetric <- getRuleMetric(ruleExec,X,target) # measure rules
  # }
  return(ruleMetric)
} 


print_learner<-function(df){
  X<-df[,1:(ncol(df)-1)]

  target <- df[,"Pred_D"] 
  rf <- RRF(X,as.factor(target),ntree=100) # build an ordinary RF 
  treeList <- RF2List(rf)
  ruleExec <- extractRules(treeList,X) # transform to R-executable rules
  ruleExec <- unique(ruleExec)
  ruleMetric <- getRuleMetric(ruleExec,X,target) # measure rules
  
  learner <- buildLearner(ruleMetric,X,target) #build the simplified tree ensemble learner
  readableLearner <- presentRules(learner,colnames(X))
  freqPattern <- getFreqPattern(readableLearner)
  freqPattern <- freqPattern[which(as.numeric(freqPattern[,"len"])>=2),][1:10,]
  return(freqPattern)
}

print_learner(m_oldest)
print_learner(f_oldest)
print_learner(m)
print_learner(f)






