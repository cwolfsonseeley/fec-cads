with cads_names as (
  select distinct
    entity_id,
    regexp_replace(upper(first_name), '[^A-Z]', '') as first_name,
    regexp_replace(upper(last_name), '[^A-Z]', '') as last_name
  from cdw.d_bio_name_mv
  where 
    entity_id in (
      select entity_id from cdw.d_entity_mv 
      where 
        person_or_org = 'P' 
        and record_status_code = 'A')
),

fec_frequency_first as (
  select
    first,
    fec_first,
    sum(fec_first) over() as n_fec_first
  from (
    select 
      first,
      count(distinct fec_id) as fec_first
    from rdata.fec_stage_processed
    group by first
  )
),

fec_frequency_last as (
  select 
    last, 
    fec_last, 
    sum(fec_last) over () as n_fec_last
  from (
    select 
      last,
      count(distinct fec_id) as fec_last
    from rdata.fec_stage_processed
    group by last
  )
),

cads_frequency_first as (
  select 
    first_name, 
    cads_first, 
    sum(cads_first) over() as n_cads_first
  from (
    select 
      first_name,
      count(distinct entity_id) as cads_first
    from cads_names
    group by first_name
  )
),

cads_frequency_last as (
  select 
    last_name, 
    cads_last, 
    sum(cads_last) over () as n_cads_last
  from(
    select 
      last_name,
      count(distinct entity_id) as cads_last
    from cads_names
    group by last_name
  )
),

first_wt as (
  select 
    first_name,
    log(2, m_first / u_first) as first_weight
  from (
    select 
      first_name,
      .98 * h / nab as m_first,
      .98 * (fec_first * cads_first - h) / (n_fec_first * n_cads_first - nab) as u_first
    from (
      select
        first as first_name,
        fec_first, cads_first, n_fec_first, n_cads_first,
        case when fec_first > 1 or cads_first > 1 then least(fec_first, cads_first) else .6 end as h,
        sum(case when fec_first > 1 or cads_first > 1 then least(fec_first, cads_first) else .6 end) over () as nab
      from fec_frequency_first fec 
        inner join cads_frequency_first cads 
        on fec.first = cads.first_name
      )
  )
),

last_wt as (
  select 
    last_name,
    log(2, m_last / u_last) as last_weight
  from (
    select 
      last_name,
      .98 * h / nab as m_last,
      .98 * (fec_last * cads_last - h) / (n_fec_last * n_cads_last - nab) as u_last
    from (
      select
        last as last_name,
        fec_last, cads_last, n_fec_last, n_cads_last,
        case when fec_last > 1 or cads_last > 1 then least(fec_last, cads_last) else .6 end as h,
        sum(case when fec_last > 1 or cads_last > 1 then least(fec_last, cads_last) else .6 end) over () as nab
      from fec_frequency_last fec 
        inner join cads_frequency_last cads 
        on fec.last = cads.last_name
      )
  )
)

select 
  'first' as type,
  first_wt.first_name as name, 
  first_wt.first_weight as weight
from first_wt
union
select
  'last' as type,
  last_wt.last_name as name,
  last_wt.last_weight as weight
from last_wt