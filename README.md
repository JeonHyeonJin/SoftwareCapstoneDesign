# 앙상블 기법을 이용한 우울증 척도 PHQ-9의 성능 개선 연구
### 2021-1 Software Capstone Design in Kyung-Hee University.
------------------
+ ### 연구 배경
  + COVID-19로 인한 우울증 문제 대두
  + 셧다운, 사회적 거리두기, 격리 및 고립으로 인한 우울 문제가 대두됨.
  + 그와 더불어 노인 우울증은 증가하는 노인 인구와 함께 사회적 문제로 자리매김함.
    + 의료 기술 발달로 인한 전 세계적 평균 수명 증가
    + 한국 고령화율 : 15.7 % (세계에서 가장 빠른 속도)
    + 평균 수명 : 82.7세 (OECD 상위권)
  + 우울증 판별 도구 PHQ-9(Patient Health Questionnaire-9)
    + 9개의 항목으로 구성, 항목당 0-3점 부과
    + PHQ-9 점수 총합이 10이상일 경우 우울증으로 판단할때 민감도(88%), 특이도(88%)
    + 환자가 자체적으로 시행할 수 있는 설문지
   #### 즉, 우울증 문제가 조명되는 시점에서, 우울증 판별 도구 PHQ-9에서 일반적으로 사용하는 cut-off value인 10점을 과연 노인에게도 동일하게 적용 가능한가에 대한 의문에서 비롯하여, 노인과 일반 성인에게 다른 우울증 cut-off value를 사용하여 각 그룹별로 적용해보았다.
  
---------------------------------
+ ### 연구 목적
1. 노인과 일반 성인에게 다른 우울증 임계값 설정 → 새로운 PHQ-9 INDEX 생성
2. 앙상블 기법을 적용한 우울증 예측 모델 수립
3. 성별, 나이 그룹 별 유의미한 PHQ-9항목 및 특징 비교

---------------------------------
+ ### 연구 진행 과정
1. R – inTrees를 이용한 앙상블 기법 적용
2. 노인과 일반 성인의 PHQ-9 threshold 조정
3. 총 4개의 그룹별 우울증 패턴 분석
4. PHQ-9 설문 항목 중 그룹별 유의미한 항목 분석
----------------------------------
+ ### 실행 파일 이름 : swc_2021_1.R
  + #### intrees package를 이용한 rule 추출
  ``` R
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
  ```
  + #### buildLearner, frequent pattern을 사용하여 상위 n개의 rule을 가독성 높게 출력
  ``` R
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
  ```
  ----------
  
  
