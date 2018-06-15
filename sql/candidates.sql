with fec_zips as (
  select 
    fec_id, zip_code
  from rdata.fec_stage_processed
  where length(zip_code) = 9
),

cads_zips as (
  select 
    entity_id,
    regexp_replace(zipcode, '[^0-9]', '') as zipcode
  from cdw.d_bio_address_mv
  where entity_id in (
    select entity_id from cdw.d_entity_mv
    where person_or_org = 'P' and record_status_code = 'A'
  )
  and contact_type_desc = 'ADDRESS'
  and country_desc is null
  and addr_type_code in ('H', 'B')  
),

zip_candidates as (
  select 
    fec_zips.fec_id,
    cads_zips.entity_id
  from fec_zips inner join cads_zips on fec_zips.zip_code = cads_zips.zipcode
),

fec_names as (
  select distinct
    fec_id, first, last
  from rdata.fec_stage_processed
),

cads_names as (
  select distinct
    entity_id,    
    regexp_replace(upper(first_name), '[^A-Z]', '') as first_name,
    upper(substr(middle_name, 1, 1)) as middle_initial,
    regexp_replace(upper(last_name), '[^A-Z]', '') as last_name
  from cdw.d_bio_name_mv
  where 
    entity_id in (
      select entity_id from cdw.d_entity_mv 
      where 
        person_or_org = 'P' 
        and record_status_code = 'A')
),

name_candidates as (
  select fec_names.fec_id, cads_names.entity_id
  from fec_names inner join cads_names on
    fec_names.first = cads_names.first_name
    and fec_names.last = cads_names.last_name
),

candidates as (
  select * from zip_candidates
  union
  select * from name_candidates
),

cads_addresses as (
  select distinct
    entity_id,
    upper(city) as city, 
    state_code, 
    zipcode5
  from cdw.d_bio_address_mv
  where
    entity_id in (
      select entity_id from cdw.d_entity_mv 
      where 
        person_or_org = 'P' 
        and record_status_code = 'A')
    and contact_type_desc = 'ADDRESS'
    and addr_type_code in ('H', 'B')  
    and country_desc is null
),

fec_to_match as (
  select distinct
    fec.fec_id,
    fec.first as fec_first,
    fec.middle_init as fec_mi,
    fec.last as fec_last,
    fec.city as fec_city,
    fec.zip_code as fec_zip,
    fec.zip5 as fec_zip5,
    fec.employer as fec_employer,
    fec.occupation as fec_occupation
from fec_stage_processed fec
),

candidate_matrix as (
  select 
    fec_to_match.*,
    candidates.entity_id,
    cads_names.first_name as cads_first,
    cads_names.middle_initial as cads_mi,
    cads_names.last_name as cads_last,
    cads_addresses.city as cads_city,
    cads_addresses.zipcode5 as cads_zip5  
  from fec_to_match 
    inner join candidates on fec_to_match.fec_id = candidates.fec_id
    inner join cads_names on candidates.entity_id = cads_names.entity_id
    left join cads_addresses on candidates.entity_id = cads_addresses.entity_id
)

select
  fec_id,
  entity_id,
  fec_employer as fec_employer,
  fec_occupation as fec_occupation,
  max(case when fec_first = cads_first then 1 else 0 end) as first,
  max(case when fec_mi = cads_mi then 1 else 0 end) as mi,
  max(case when fec_last = cads_last then 1 else 0 end) as last,
  max(case when fec_city = cads_city then 1 when fec_zip5 = cads_zip5 then 1 else 0 end) as geo
from candidate_matrix
group by 
  fec_id,
  entity_id,
  fec_employer,
  fec_occupation
