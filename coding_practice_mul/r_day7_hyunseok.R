# 1. mtcars데이터 weight열 추가, 무게가 중위수보다 큰 자동차는 heavy, 그렇지 않은 자동차는 light를 저장
# - 각 종류별 데이터 건수 출력, 비율



# 2. ggplot2 패키지에는 미국 동북중부 437개 지역의 인구통계 정보를 담은 midwest라는 데이터가 포함되어 있음. 
# midwest 데이터를 사용하여,
# 불러오기 : midwest<-as.data.frame(ggplot2::midwest)
# - ggplot2 의 midwest 데이터를 데이터 프레임 형태로 불러와서 데이터의 특성을 파악
# - poptotal(전체 인구)을 total 로, popasian(아시아 인구)을 asian 으로 변수명을 수정
# - total, asian 변수를 이용해 '전체 인구 대비 아시아 인구 백분율' 파생변수 생성
# - 아시아 인구 백분율 전체 평균을 구하고, 평균을 초과하면 "large", 그 외에는 "small"을 부여하는 파생변수 생성
# - "large"와 "small"에 해당하는 지역이 얼마나 되는지 빈도수를 출력



# 3. 타이타닉 데이터 분석
# - 타이타닉 데이터 불러오기
# - 생존자 수, 사망자 수 출력
# - pclass, embarked 별 승객수 출력(비율)
# - Name에서 호칭 종류 출력, 호칭 종류 별 승객수 출력
# - 호칭을 아래와 같이 변경하여 name2열에 추가
# * "Mlle", "Ms", "Lady", "Dona" 는 "Miss"로 변경
# * "Mme"는  "Mrs"로 변경
# * "Capt", "Col", "Major", "Dr", "Rev", "Don",  "Sir", "the Countess", "Jonkheer"는 "Officer"로 변경
# * "Mr", "Mrs", "Miss"는 그대로
# * 나머지 호칭은 "Others"
# -name2 열을 factor로(5가지 범주) 변환
# -name2열의 호칭별 인원수 출력
# -호칭 정보를 바탕으로 나이(Age) 결측값 대체(호칭 별 나이의 평균값)
# -age열의 구간별 인원수 출력
# 10대 미만, 10대, 20대, 30대, 40대, 50대 이상
# -cabin 컬럼의 1번째 글자 출력(NA는 제외)
# - fare열 값에 대해 최대/최소/평균/표준편차 출력
# - sibsp + parch를 더하여 새롭게 family열에 저장





