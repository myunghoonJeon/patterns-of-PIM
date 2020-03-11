# condition 에서 외래환자면서 65세 노인인 경우
comment="
SELECT condition_occurrence_id,ce.person_id, p.year_of_birth,cast(year(ce.condition_start_date) as int)-cast(p.year_of_birth as int) as age, ce.condition_start_date as condition_date,
vo.visit_concept_id as visit_concept
FROM [DB].dbo.condition_occurrence ce
left join [DB].dbo.person p
on ce.person_id = p.person_id and cast(year(ce.condition_start_date) as int)-cast(p.year_of_birth as int) >64
left join '[DB].dbo.visit_occurrence vo
on ce.visit_occurrence_id = vo.visit_occurrence_id
where p.year_of_birth is not null and year(condition_start_date)>1994 and vo.visit_concept_id=9202
"
#condition concept id로변환 and concept id 0 제거
comment="
SELECT ce.person_id, p.year_of_birth,cast(year(ce.condition_start_date) as int)-cast(p.year_of_birth as int) as age, ce.condition_start_date as condition_date,
vo.visit_concept_id as visit_concept,condition_concept_id
FROM [DB].dbo.condition_occurrence ce
left join [DB].dbo.person p
on ce.person_id = p.person_id and cast(year(ce.condition_start_date) as int)-cast(p.year_of_birth as int) >64
left join [DB].dbo.visit_occurrence vo
on ce.visit_occurrence_id = vo.visit_occurrence_id
where p.year_of_birth is not null and year(condition_start_date)>1994 and vo.visit_concept_id=9202 and condition_concept_id != 0
"

# drug 에서 65세 이상 ( year_of_birth 가 null 인거 제외 )
comment="
SELECT drug_exposure_id,drug_concept_id,drug_source_value,de.person_id, p.year_of_birth,cast(year(de.drug_exposure_start_date) as int)-cast(p.year_of_birth as int) as age, year(de.drug_exposure_start_date) as drug_year,drug_exposure_start_date
  FROM drug_exposure de
  left join person p
  on de.person_id = p.person_id and cast(year(de.drug_exposure_start_date) as int)-cast(p.year_of_birth as int) >64 
  where p.year_of_birth is not null and year(drug_exposure_start_date)>1994 and drug_exposure_start_date is not null
"

#복사할 drug테이블

comment="SELECT 	de.person_id,
				cast(year(de.drug_exposure_start_date) as int)-cast(p.year_of_birth as int) as age,
				drug_exposure_start_date,
				drug_concept_id
		FROM [DB].dbo.drug_exposure de
		left join [DB].dbo.person p
		on de.person_id = p.person_id and cast(year(de.drug_exposure_start_date) as int)-cast(p.year_of_birth as int) >64 
		where p.year_of_birth is not null and year(drug_exposure_start_date)>1994 and drug_exposure_start_date is not null"

#drug 튜플 합체 쿼리

comment="select distinct person_id,age,drug_exposure_start_date,
	stuff((
		select','+cast(drug_concept_id as nvarchar)
		from JMH_Paper.dbo.drug_copy 
		where person_id = dc.person_id and drug_exposure_start_date = dc.drug_exposure_start_date
		for XML PATH('')
	),1,1,'') AS drug_concept_id
from JMH_Paper.dbo.drug_copy dc
"

#최종 전처리 데이터 테이블에 저장
comment="insert into JMH_Paper.dbo.final_condition_drug_description
select * from
(select ced.person_id, 
		ced.age, 
		condition_concept_id,
		condition_start_date,
		ded.drug_concept_id ,
		p.gender_source_value as gender
 from JMH_Paper.dbo.condition_exposure_distinct ced
left join JMH_Paper.dbo.drug_exposure_distinct ded
on ced.person_id = ded.person_id and ced.condition_start_date = ded.drug_exposure_start_date
left join cdmpv531.dbo.person p
on ced.person_id = p.person_id) temp"


