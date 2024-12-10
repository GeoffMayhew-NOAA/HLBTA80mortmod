#' [Author: Geoff Mayhew]
#' [Purpose: Pull of raw data]

library(odbc)         # for connectivity to NORPAC
library(data.table)   # for data wrangling

#======================================================================================================================#
# Pull covariate and haul data from NORPAC/OBSINT ####
#======================================================================================================================#

# Connect to NORPAC
channel <- dbConnect(
  odbc::odbc(),"AFSC",
  UID    = rstudioapi::askForPassword("Database user"),
  PWD    = rstudioapi::askForPassword("Database password"))

#' Pull all halibut covariate data - we will further subset this data using haul purpose code and gear type for this 
#' project specifically. 
hlbt_covar_pull <- setDT(dbGetQuery(channel, paste(
  "
  -- Pull covariate data, join with length, comp, and sample so that haul_seq is obtained for each record
  WITH X1 AS (
    SELECT 
      a.cruise, a.permit, a.length_seq, a.covariate_seq, a.number_of_animals, 
      -- Assessment time is in a format that R doesn't understand. Use TO_CHAR or EXTRACT
      EXTRACT(DAY FROM a.assessment_time) AS assess_day,
      EXTRACT(HOUR FROM a.assessment_time) AS assess_hour,
      EXTRACT(MINUTE FROM a.assessment_time) AS assess_min,
      EXTRACT(SECOND FROM a.assessment_time) AS assess_sec,
      a.release_code AS RELEASE_CODE_C, a.special_study_code AS SSC_C,
      b.species_composition_seq AS COMP_SEQ, b.haul_seq AS HAUL_SEQ_L, 
      b.length_size, b.viability, b.condition_code, b.frequency,
      c.sample_seq,
      d.haul_seq AS HAUL_SEQ_S
    FROM atl_halibut_condition_covar a 
      LEFT JOIN atl_length b
        ON a.cruise = b.cruise AND a.permit = b.permit AND a.length_seq = b.length_seq
      LEFT JOIN atl_species_composition c
        ON b.cruise = c.cruise AND b.permit = c.permit AND b.species_composition_seq = c.species_composition_seq
      LEFT JOIN atl_sample d
        ON c.cruise = d.cruise AND c.permit = d.permit AND c.sample_seq = d.sample_seq
  ),
  -- Consolidate haul_seq (from length and sample tables) into one column
  X2 AS (    
    SELECT 
      cruise, permit, haul_seq_l AS HAUL_SEQ, sample_seq, comp_seq, length_seq, covariate_seq, 
      frequency, length_size, viability, condition_code, number_of_animals,
      assess_day, assess_hour, assess_min, assess_sec, release_code_c, ssc_c
    FROM X1 
    WHERE haul_seq_l IS NOT NULL
    UNION
    SELECT 
      cruise, permit, haul_seq_s AS HAUL_SEQ, sample_seq, comp_seq, length_seq, covariate_seq, 
      frequency, length_size, viability, condition_code, number_of_animals, 
      assess_day, assess_hour, assess_min, assess_sec, release_code_c, ssc_c
    FROM X1 
    WHERE haul_seq_s IS NOT NULL
  )
  -- Merge with atl_haul and atl_halibut_assess_attribute, but some attribute records are apparently missing
  SELECT 
    e.*,
    f.haul_purpose_code, f.trip_seq, f.haul_number, f.obs_vessel_type AS VESSEL, f.obs_gear_code AS GEAR, 
    f.deployment_date AS DEPLOY, f.retrieval_date AS RETRV, f.reporting_area_code AS AREA, f.fishing_depth,
    f.bottom_depth, f.official_total_catch AS HAUL_MT, f.retrieval_latitude_dd AS LAT_D,  
    f.retrieval_longitude_dd AS LON_D, f.target_fishery_code AS HAUL_TARGET, f.trip_target_code AS TRIP_TARGET,
    g.attribute_seq, g.release_code AS RELEASE_CODE_A, g.air_temperature AS A_TEMP, g.water_temperature AS W_TEMP,
    g.time_net_landed_on_deck, g.sorting_begin_time, g.sorting_end_time, g.percent_of_hooks_not_primary, 
    g.is_time_net_landed_est, g.is_sorting_begin_time_est, g.is_sorting_end_time_est, g.special_study_code AS SSC_A,
    h.weight AS WEIGHT_KG,
    i.presorted_weight, i.presorted_number  -- These don't show for all non-trawl hauls
  FROM X2 e
    LEFT JOIN norpac_views.akr_obs_haul_mv f
      ON e.cruise = f.cruise AND e.permit = TO_CHAR(f.vessel_id) AND e.haul_seq = f.haul_seq
    LEFT JOIN atl_halibut_assess_attribute g
      ON e.cruise = g.cruise AND e.permit = g.permit AND e.haul_seq = g.haul_seq
    LEFT JOIN obsint.atl_lov_length_weight  h
      ON e.length_size = h.length
    LEFT JOIN obsint.atl_extrawl_presort_mv i
      ON e.cruise = i.cruise AND e.permit = i.permit AND e.haul_seq = i.haul_seq
    WHERE h.species_code = 101
  "
)))

#======================================================================================================================#
# Save raw data pull ####
#======================================================================================================================#

save(hlbt_covar_pull, file = "data/hlbt_covar_pull.rdata")
