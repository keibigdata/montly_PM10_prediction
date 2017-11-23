# 미세먼지 발생 패턴 분석

본 연구는 [에어코리아](http://www.airkorea.or.kr) 에서 제공하는 대기오염물질 확정 데이터에 의사결정나무(Decision Tree) 기법을 적용하여 미세먼지의 발생 패턴에 대해 분석하는 연구입니다.

## Code 설명
본 repository에서 제공하는 code에 대한 설명은 다음과 같습니다.

> pm10

>> _00data

>>> _2016airPollution01-13: 2001 ~ 2013년 대기오염물질 농도 데이터

>>> _2017airPollution14-16: 2014 ~ 2016년09월 대기오염물질 농도 데이터

>>> airQualityChina: 베이징, 상해 PM2.5 데이터 / 베이징 AQI

>>> emissionData: 1999~2013년 대기오염물질 배출량 데이터

>>> 인구.zip: 인구 데이터, 본 실험에서는 "인구밀도_2000-2016_ver2_2016기준.xlsx" 파일 활용

>>> 황사.zip: 황사관측일수

>> _01processing

>>> _00input

>>> _01code

>>>> _00-0monitoring_staion_processing: 대기오염물질측정소 주소 정리 및 geocoding 결과 반영(위도, 경도)

>>>> _00-1geocoding.R: 대기오염물질측정소 주소를 기반으로 위경도 좌표 찾기

>>>> _00-2weather_monitoring_station_processing: 기상기후측정소 정리

>>>> _00-3matchingProgress: 시군구(행정구역), 대기오염물질측정소, 기상기후측정소 전처리 파일 / 결과는 00-4 참조

>>>> _00-4matching: 대기오염물질측정소에 시군구를 조인함, 시군구에 기상기후측정소를 조인함

>>>> _00-5missing_sigungu: 변경된 시군구 처리

>>>> _00-6preprocessingEmissionData.R: 실험대상 시군구를 기준으로 대기오염물질 배출량 처리

>>>> _00-7distanceToChina: 베이징, 상해로부터 실험대상 시군구까지의 거리 측정

>>>> _00-8preprocessingChina.R: 월별 데이터로 처리

>>>> _00-9populationDensity: 인구밀도 데이터 

>>>> _00-9populationDensitySigCd: 인구밀도 데이터에 시군구 코드 매칭

>>>> _00-10mergePopulationDensity.R: 인구밀도 데이터를 조인시키기 위한 전처리

>>>> _00-11yellowDust: [황사관측일수](http://www.kma.go.kr/weather/asiandust/observday.jsp) 처리 과정

>>>> _00-11yellowDust1: 사용 안함

>>>> _00-12mergeYellowDust.R: 황사관측일수 데이터를 조인시키기 위한 전처리

>>>> _01meanOfAirPollutionOfMonitoring.R: 

>>>> _02changeColumn

>>>> _03-0meanOfAirPollutionOfSigungu.R

>>>> _03-1descriptiveStatistics.R: 연평균, 월평균, 시군구별 평균 그래프

>>>> _04merge.R

>>>> _05variableImportance.R

>>>> _06-1average

>>>> _06counterFactual.R

>>> _02output