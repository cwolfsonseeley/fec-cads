create table rdata.fec_stage_processed as (
  select 
    ora_hash(first || ' ' || last || ' ' || zip5, 4294967295, 20180613) ||
      ora_hash(first || ' ' || last || ' ' || zip5, 4294967295, 20180614) as fec_id,
    sub_id, city, state, zip_code, employer, occupation, zip5,
    first, middle_init, last
  from (
  select
    sub_id, city, state, zip_code, employer, occupation, zip5,
    regexp_replace(first, '[^A-Z]', '') as first,
    substr(case 
           when middle = first then null 
           when middle in ('MS', 'MR', 'MRS', 'DR', 'DRS') then null
           else middle end, 1, 1) as middle_init,
    regexp_replace(
      trim(regexp_replace(
           full_last, 
           '(\WI{2,}\W?)|(\WJR\W?)|(\WSR\W?)|(\WMD\W?)|(\WPHD\W?)|([0-9]+)|(\WCFP\W?)|(\WMBA\W?)|(\WCHFC\W?)', '')),
      '[^A-Z]', '') as last
  from (
    select
      sub_id, city, state, zip_code, employer, occupation, zip5,
      full_last, last,
      case when first is null then first_name else first end as first,
      replace(middle, '.', '') as middle
    from (
      select
        sub_id, city, state, zip_code, employer, occupation, 
        substr(zip_code, 1, 5) as zip5,
        full_last, last,
        case when
          substr(first_name, 1, instr(first_name, ' ') - 1) = '' then first_name
          else substr(first_name, 1, instr(first_name, ' ') - 1)
        end as first, 
        substr(first_name, instr(first_name, ' ') + 1) as middle,
        first_name 
        from (
          select 
            sub_id, city, state, 
            regexp_replace(zip_code, '[^0-9]', '') as zip_code,
            employer, occupation, 
            substr(name, 1, instr(name, ',') - 1) as full_last,
            substr(name, 1, instr(name, ' ') - 1) as last,
            trim(substr(name, instr(name, ',') + 1)) as first_name  
          from rdata.fec_stage
  ))))
)
