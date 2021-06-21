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
    + 2017-2040 한국 65세이상 노인 인구 구성 비율 --> 우상향 하는 형태를 보임.
    + <img src = "https://user-images.githubusercontent.com/50453570/122723243-b010d380-d2ad-11eb-8f76-112cfd086fe8.png" width = 1000>
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
  return(freqPattern)}

----------

### 연구 결과
  + 다음과 같이 그룹별로 얻어진 rule들을 확인할 수 있다.
  +   <img src = "https://user-images.githubusercontent.com/50453570/122724092-92903980-d2ae-11eb-8ab1-da819e1881b5.png" width = 500>
  
  + 다음 사진은 현재 우울증 유병 여부와 새롭게 지정한 cut-off value를 이용한 우울증 prediction값을 비교하여 ROC curve를 그린것이다.
    + 나이는 한국 노인 기준인 65세를 기준으로 하였다. 성별은 남녀로 구분하였다. 이를 통해 구분한 총 4개의 그룹은 다음과 같다.
    + f_oldest : 65세 이상 여성
    + m_oldest : 65세 이상 남성
    + f : 65세 미만 여성
    + m : 65세 미만 남성
    + f_oldest ROC and AUC
    <img src = "https://user-images.githubusercontent.com/50453570/122723207-a4bda800-d2ad-11eb-92e9-a95b6eeabd28.png" width = 400>
    
    + m_oldest ROC and AUC
    <img src = "https://user-images.githubusercontent.com/50453570/122724787-3e398980-d2af-11eb-9310-81fedf9d6edf.png" width = 400>
    
    + f ROC and AUC
    <img src = "https://user-images.githubusercontent.com/50453570/122724864-54474a00-d2af-11eb-951c-0162d8a4ae76.png" width = 400>
    
    + m ROC and AUC
    <img src = "https://user-images.githubusercontent.com/50453570/122724721-2f52d700-d2af-11eb-9f98-1025d10352f5.png" width = 400>
-----

### 결론 및 제언
+ 우울증 판정 도구로 사용하는 PHQ-9의 성능 개선을 목표로 하여 성별, 나이별 집단 총 4개를 구성하여 각 집단이 가지는 우울증 비율, 우울증 판별 패턴, 중요 우울증 인자 등을 식별하였다. 성별은 남/여로 구분하였으며, 나이는 65세를 기준으로 하였다. 
+ 65세는 한국에서 고령자(노인)의 기준 나이이다. 이를 통해 남,여 각각의 노인, 성인별 집단을 구성하였다. 앙상블 기법의 R 라이브러리 intress의 RRF를 사용하여 각 그룹별 상위 10개의 phq-9항목별 우울증 여부 판정 기능의 패턴을 생성해내었으며, 65세이상 남성, 65세미만 남성 모두 phq-4, phq-5번과 많은 연관이 있으며, 여성은 65세 이상은 불면증, 65세 미만은 식욕, 희망과 관련되었다. 
+ 연구 결과 여성이 남성에 비해 우울증에 취약한 것으로 나타났다. 우울증을 경험하는 전체 여성의 수는 260명, 남성은 70명으로 여성이 남성의 3배 이상을 나타냄을 확인되었다. 
+ 따라서 추후 데이터셋을 추가로 확보할 수 있다면, 성별별로 우울증의 차이를 더욱 세밀하게 식별하고, 여성의 우울증 인자에 대한 검증을 통해 예방 및 치료할 수 있는 기반을 마련할 수 있을것으로 기대한다. 

-----
#### 참고 자료
[inTrees package](https://cran.r-project.org/web/packages/inTrees/index.html)

[R-pROC](http://blog.naver.com/PostView.nhn?blogId=nife0719&logNo=220993392408)

  

  
  
