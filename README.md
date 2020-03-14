# patterns-of-PIM
Title : Patterns of potentially inappropriate medication use according to gender in Korean older adults
- post-doc을 시작하며 CDM기반의 첫 번째 논문을 작성하기 위한 GIT 입니다.
- 양영모 박사님과 협업하여 논문을 완성할 수 있기를 희망합니다.

기본 데이터 전처리는 DB쿼리를 기반으로 서버 사이드에서 수행

(처방된 의료 건수와 그에 대한 약물의 처방 패턴을 분석하기 위한 접근 으로 인해 데이터 수가 기본적으로 높음)

따라서 로컬 환경 보다는 서버의 고성능을 활용하여 데이터 전처리를 수행

* 전처리 과정에서 데이터 수 변화
  * (약물 처방건 5000만 건 정도)
  * (진료 처방 700만건 정도)
  * (가비지 데이터를 삭제하고 매칭이 안되는 데이터(오류값 포함)를 제외)
  * (비어스 매뉴얼 조건 적용)
  * (최종 300만건 으로 데이터 정리)

* 전처리 후 과정
  * beers criteria 를 기반으로 노인 (65세) 이상의 노인에 대한 약물 처방에 대한 권고 사항을 체크하는 기본 접근
  * 해당 병원 내의 약물 처방 현황에 대한 시계열 측면의 통계 접근
  * 약물 과 만성질환 간의 관계를 분석 ( Logistic Regression )
  * 분석 후 가시화 계획 ( 정의 중 )

* 사용 기술
  * SQL
  * R
  * visualization method ( sankey Diagram 예상 )

* 코드 구성
  * statc_main.R : 결과를 호출
  * function_set.R : 데이터 처리 함수 모음
  * properties_sql.R : 데이터 전처리 과정에서 수행한 쿼리모음
  * (숨김코드) propertiesParameters.R : 각종 ID/PW, 접근 정보, 변수 설정

* 파일 구성
  * conditon_inform.xlsx : 만성질환 정보 구분
  * drug_inform.xlsx : beers 기준 필터링 약물 구분
  