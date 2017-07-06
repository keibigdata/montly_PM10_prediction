# 미세먼지 발생 패턴 분석

본 연구는 에어코리아(www.airkorea.or.kr)에서 제공하는 대기오염물질 확정 데이터에 의사결정나무(Decision Tree) 기법을 적용하여 미세먼지의 발생 패턴에 대해 분석하는 연구입니다.

## Code 설명
본 repository에서 제공하는 code에 대한 설명은 다음과 같습니다.

* _00-0managingAirPollutionMonitoringStation.txt: 대기오염물질 측정망의 주소 정보를 활용하여 측정소의 위경도를 알아내는 과정에 대한 설명
* _00-1geocoding.R: geocoding code
* _00-3matching: 대기오염물질 측정망과 기상기후 측정망을 대상 시군구에 매칭시키는 과정에 대한 설명
* _01meanOfAirPollutionOfMonitoring.R: 대기오염물질 측정소별 월별 평균 구하는 code
* _02changeColumn.txt: _01meanOfAirPollutionOfMonitoring.R의 결과에 시군구 code를 매칭시키는 방법에 대한 설명
* _03-0meanOfAirPollutionOfSigungu.R: 대기오염물질의 시군구별 평균 구하는 code
* _03-1descriptiveStatistics.R: 기술통계 작성 code(연평균, 월평균, 시군구별평균)
_04merge.R: 대기오염물질 데이터와 기상기후 데이터를 매칭하는 code
