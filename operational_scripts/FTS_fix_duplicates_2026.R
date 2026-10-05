#Climate database - fix duplicates

#found in several forestry FTS stations
#remove data with hours that have three digits only, ie. 0:00, 1:00, 2:00, as opposed to 00:00, 01:00, 02:00

#get tables ready for merge statements
#Climate PROD edit
connection <- odbc::dbConnect(drv = odbc::odbc(), dsn = "ECC_CLIMATE", uid = "CLIMATE_EDIT", pwd = Sys.getenv("ORACLE_CLIM_EDIT_PWD"))

bad_hours <- c("0:00", "1:00", "2:00", "3:00", "4:00",
               "5:00", "6:00", "7:00", "8:00", "9:00")

good_hours <- c("00:00", "01:00", "02:00", "03:00", "04:00",
                "05:00", "06:00", "07:00", "08:00", "09:00")

#data tables
Table0<-odbc::dbGetQuery(connection, "SELECT * FROM ECC_CLIMATE.CD_CLIMATE_DATA_0 WHERE CD_TIME IN ('0:00', '1:00', '2:00', '3:00', '4:00',
                                                                                                      '5:00', '6:00', '7:00', '8:00', '9:00')")
Table3<-odbc::dbGetQuery(connection, "SELECT * FROM ECC_CLIMATE.CD_CLIMATE_DATA_3 WHERE CD_TIME IN ('0:00', '1:00', '2:00', '3:00', '4:00',
                                                                                                      '5:00', '6:00', '7:00', '8:00', '9:00')")
Table1<-odbc::dbGetQuery(connection, "SELECT * FROM ECC_CLIMATE.CD_CLIMATE_DATA_1 WHERE CD_TIME IN ('0:00', '1:00', '2:00', '3:00', '4:00',
                                                                                                      '5:00', '6:00', '7:00', '8:00', '9:00')")

#Data types
cd_data_types<-odbc::dbGetQuery(connection, "SELECT * FROM ECC_CLIMATE.CD_DATA_TYPES_LUT")

#count how many rows would be deleted
qry_count <- "
SELECT COUNT(*) AS n_bad
FROM %s
WHERE LENGTH(CD_TIME)=4
  AND STATION_ID = 79 
  AND REGEXP_LIKE(CD_TIME, '^[0-9]:[0-5][0-9]$')
"

odbc::dbGetQuery(connection, sprintf(qry_count, "ECC_CLIMATE.CD_CLIMATE_DATA_0"))
odbc::dbGetQuery(connection, sprintf(qry_count, "ECC_CLIMATE.CD_CLIMATE_DATA_1"))
odbc::dbGetQuery(connection, sprintf(qry_count, "ECC_CLIMATE.CD_CLIMATE_DATA_3"))

#see which values would be deleted

qry_freq <- "
SELECT CD_TIME, COUNT(*) AS n
FROM %s
WHERE LENGTH(CD_TIME)=4
  AND STATION_ID = 79
  AND REGEXP_LIKE(CD_TIME, '^[0-9]:[0-5][0-9]$')
GROUP BY CD_TIME
ORDER BY CD_TIME
"

odbc::dbGetQuery(connection, sprintf(qry_freq, "ECC_CLIMATE.CD_CLIMATE_DATA_0"))

station_id <- 79

qry_bad_rows <- "
SELECT COUNT(*) AS n_bad_rows
FROM %s
WHERE STATION_ID = %d
  AND LENGTH(TRIM(CD_TIME)) = 4
  AND REGEXP_LIKE(TRIM(CD_TIME), '^[0-9]:[0-5][0-9]$')
"

odbc::dbGetQuery(connection, sprintf(qry_bad_rows, "ECC_CLIMATE.CD_CLIMATE_DATA_0", station_id))  # expect 11247
odbc::dbGetQuery(connection, sprintf(qry_bad_rows, "ECC_CLIMATE.CD_CLIMATE_DATA_1", station_id))  # expect 44988
odbc::dbGetQuery(connection, sprintf(qry_bad_rows, "ECC_CLIMATE.CD_CLIMATE_DATA_3", station_id))  # expect 0

#delete bad rows for station id 79 only

# station_id <- 79
# 
# delete_sql <- "
# DELETE FROM %s
# WHERE STATION_ID = %d
#   AND LENGTH(TRIM(CD_TIME)) = 4
#   AND REGEXP_LIKE(TRIM(CD_TIME), '^[0-9]:[0-5][0-9]$')
# "
# 
# tables <- c("ECC_CLIMATE.CD_CLIMATE_DATA_0",
#             "ECC_CLIMATE.CD_CLIMATE_DATA_1",
#             "ECC_CLIMATE.CD_CLIMATE_DATA_3")
# 
# DBI::dbBegin(connection)
# 
# for (tbl in tables) {
#   pre <- odbc::dbGetQuery(connection, sprintf(qry_bad_rows, tbl, station_id))$N_BAD_ROWS
#   
#   DBI::dbExecute(connection, sprintf(delete_sql, tbl, station_id))
#   
#   post <- odbc::dbGetQuery(connection, sprintf(qry_bad_rows, tbl, station_id))$N_BAD_ROWS
#   
#   if (post != 0) {
#     DBI::dbRollback(connection)
#     stop(sprintf("Rollback: %s still has %d bad rows (pre=%d).", tbl, post, pre))
#   }
#   
#   message(sprintf("%s: deleted %d bad rows.", tbl, pre))
# }
# 
# DBI::dbCommit(connection)

#delete bad rows for other stations

# bad_station_ids <- sort(c(
#   33,34,38,40,41,42,43,44,46,47,48,49,50,51,52,53,54,55,63,66,67,71,72,74,75,
#   78,80,81,85,86,87,88,89
# ))
# 
# tables_to_clean <- c(
#   "ECC_CLIMATE.CD_CLIMATE_DATA_0",
#   "ECC_CLIMATE.CD_CLIMATE_DATA_1",
#   "ECC_CLIMATE.CD_CLIMATE_DATA_3"
# )
# 
# get_scalar <- function(df) as.integer(df[[1]][1])
# 
# qry_count_bad_station <- "
# SELECT COUNT(*) AS n_bad
# FROM %s
# WHERE STATION_ID = %d
#   AND LENGTH(TRIM(CD_TIME)) = 4
#   AND REGEXP_LIKE(TRIM(CD_TIME), '^[0-9]:[0-5][0-9]$')
# "
# 
# qry_delete_bad_station <- "
# DELETE FROM %s
# WHERE STATION_ID = %d
#   AND LENGTH(TRIM(CD_TIME)) = 4
#   AND REGEXP_LIKE(TRIM(CD_TIME), '^[0-9]:[0-5][0-9]$')
# "
# 
# logs <- list()
# 
# for (sid in bad_station_ids) {
#   
#   message(sprintf("\n=== Station %d ===", sid))
#   DBI::dbBegin(connection)
#   
#   ok <- TRUE
#   station_log <- data.frame(
#     station_id = integer(0),
#     table_name = character(0),
#     pre_bad = integer(0),
#     deleted_reported = integer(0),
#     post_bad = integer(0),
#     stringsAsFactors = FALSE
#   )
#   
#   for (tbl in tables_to_clean) {
#     
#     pre <- get_scalar(odbc::dbGetQuery(connection, sprintf(qry_count_bad_station, tbl, sid)))
#     
#     deleted_reported <- tryCatch(
#       as.integer(DBI::dbExecute(connection, sprintf(qry_delete_bad_station, tbl, sid))),
#       error = function(e) { ok <<- FALSE; NA_integer_ }
#     )
#     
#     post <- get_scalar(odbc::dbGetQuery(connection, sprintf(qry_count_bad_station, tbl, sid)))
#     
#     if (!isTRUE(post == 0L)) ok <- FALSE
#     
#     station_log <- rbind(
#       station_log,
#       data.frame(
#         station_id = sid,
#         table_name = tbl,
#         pre_bad = pre,
#         deleted_reported = deleted_reported,
#         post_bad = post,
#         stringsAsFactors = FALSE
#       )
#     )
#   }
#   
#   if (ok) {
#     DBI::dbCommit(connection)
#     message(sprintf("Committed station %d", sid))
#   } else {
#     DBI::dbRollback(connection)
#     message(sprintf("Rolled back station %d (verification failed)", sid))
#   }
#   
#   logs[[as.character(sid)]] <- station_log
# }
# 
# delete_log <- do.call(rbind, logs)
# print(delete_log)
# 
# # Quick station-level totals
# aggregate(pre_bad ~ station_id, delete_log, sum)
# aggregate(post_bad ~ station_id, delete_log, sum)


#delete bad rows for other stations

# bad_station_ids <- sort(c(
#   32, 61, 65, 77
# ))
# 
# tables_to_clean <- c(
#   "ECC_CLIMATE.CD_CLIMATE_DATA_0",
#   "ECC_CLIMATE.CD_CLIMATE_DATA_1",
#   "ECC_CLIMATE.CD_CLIMATE_DATA_3"
# )
# 
# get_scalar <- function(df) as.integer(df[[1]][1])
# 
# qry_count_bad_station <- "
# SELECT COUNT(*) AS n_bad
# FROM %s
# WHERE STATION_ID = %d
#   AND LENGTH(TRIM(CD_TIME)) = 4
#   AND REGEXP_LIKE(TRIM(CD_TIME), '^[0-9]:[0-5][0-9]$')
# "
# 
# qry_delete_bad_station <- "
# DELETE FROM %s
# WHERE STATION_ID = %d
#   AND LENGTH(TRIM(CD_TIME)) = 4
#   AND REGEXP_LIKE(TRIM(CD_TIME), '^[0-9]:[0-5][0-9]$')
# "
# 
# logs <- list()
# 
# for (sid in bad_station_ids) {
# 
#   message(sprintf("\n=== Station %d ===", sid))
#   DBI::dbBegin(connection)
# 
#   ok <- TRUE
#   station_log <- data.frame(
#     station_id = integer(0),
#     table_name = character(0),
#     pre_bad = integer(0),
#     deleted_reported = integer(0),
#     post_bad = integer(0),
#     stringsAsFactors = FALSE
#   )
# 
#   for (tbl in tables_to_clean) {
# 
#     pre <- get_scalar(odbc::dbGetQuery(connection, sprintf(qry_count_bad_station, tbl, sid)))
# 
#     deleted_reported <- tryCatch(
#       as.integer(DBI::dbExecute(connection, sprintf(qry_delete_bad_station, tbl, sid))),
#       error = function(e) { ok <<- FALSE; NA_integer_ }
#     )
# 
#     post <- get_scalar(odbc::dbGetQuery(connection, sprintf(qry_count_bad_station, tbl, sid)))
# 
#     if (!isTRUE(post == 0L)) ok <- FALSE
# 
#     station_log <- rbind(
#       station_log,
#       data.frame(
#         station_id = sid,
#         table_name = tbl,
#         pre_bad = pre,
#         deleted_reported = deleted_reported,
#         post_bad = post,
#         stringsAsFactors = FALSE
#       )
#     )
#   }
# 
#   if (ok) {
#     DBI::dbCommit(connection)
#     message(sprintf("Committed station %d", sid))
#   } else {
#     DBI::dbRollback(connection)
#     message(sprintf("Rolled back station %d (verification failed)", sid))
#   }
# 
#   logs[[as.character(sid)]] <- station_log
# }
# 
# delete_log <- do.call(rbind, logs)
# print(delete_log)
# 
# # Quick station-level totals
# aggregate(pre_bad ~ station_id, delete_log, sum)
# aggregate(post_bad ~ station_id, delete_log, sum)
# 
# #fix remaining bad data rows (data gaps in correct data upload that didn't replace old bad data)

bad_station_ids <- sort(c(
  32, 33,34,38,40,41,42,43,44,46,47,48,49,50,51,52,53,54,55,
  63,66,67,71,72,74,75,
  78,80,81,85,86,87,88,89
))

bad_station_ids <- sort(c(61, 65, 77))

tables_to_clean <- c(
  "ECC_CLIMATE.CD_CLIMATE_DATA_0",
  "ECC_CLIMATE.CD_CLIMATE_DATA_1",
  "ECC_CLIMATE.CD_CLIMATE_DATA_3"
)

# Bad-load window: CREATED on Sept 9 or Sept 10 (calendar day)
# Keep only if UPDATED later than Sept 11  => must be >= Sept 12
created_days_sql <- "DATE '2025-09-09', DATE '2025-09-10'"
updated_cutoff_sql <- "DATE '2025-09-12'"  # UPDATED < 2025-09-12 is bad

# If you want to restrict to the bad user, keep this TRUE.
# If some leftovers were created by something like CLIMATE_EDIT, set FALSE.
require_created_by <- TRUE
created_by_norm <- "EMMA_RILEY"

get_scalar <- function(df) as.integer(df[[1]][1])

created_by_clause <- if (require_created_by) {
  sprintf("AND TRIM(UPPER(CREATED_BY)) = '%s'", toupper(created_by_norm))
} else {
  ""
}

qry_count <- "
SELECT COUNT(*) AS n_bad
FROM %s
WHERE STATION_ID = %d
  AND TRUNC(CREATED) IN (%s)
  %s
  AND (UPDATED IS NULL OR UPDATED < %s)
"

qry_preview <- "
SELECT *
FROM %s
WHERE STATION_ID = %d
  AND TRUNC(CREATED) IN (%s)
  %s
  AND (UPDATED IS NULL OR UPDATED < %s)
ORDER BY CD_YEAR, CD_MONTH, CD_DAY, DATA_TYPE_ID, CD_TIME
FETCH FIRST 50 ROWS ONLY
"

qry_delete <- "
DELETE FROM %s
WHERE STATION_ID = %d
  AND TRUNC(CREATED) IN (%s)
  %s
  AND (UPDATED IS NULL OR UPDATED < %s)
"

# -------------------
# 1) Pre-audit counts
# -------------------
pre_audit <- data.frame()
for (sid in bad_station_ids) {
  for (tbl in tables_to_clean) {
    n <- get_scalar(DBI::dbGetQuery(
      connection,
      sprintf(qry_count, tbl, sid, created_days_sql, created_by_clause, updated_cutoff_sql)
    ))
    pre_audit <- rbind(pre_audit, data.frame(
      station_id = sid, table_name = tbl, n_bad = n, stringsAsFactors = FALSE
    ))
  }
}
print(pre_audit[pre_audit$n_bad > 0, ], row.names = FALSE)

# ------------------------------------------------
# 2) Delete loop (transaction per station + verify)
# ------------------------------------------------
logs <- list()

for (sid in bad_station_ids) {
  
  message(sprintf("\n=== Station %d ===", sid))
  
  # avoid "Double begin"
  try(DBI::dbRollback(connection), silent = TRUE)
  
  DBI::dbBegin(connection)
  
  ok <- TRUE
  station_log <- data.frame(
    station_id = integer(0),
    table_name = character(0),
    pre_bad = integer(0),
    deleted_reported = integer(0),
    post_bad = integer(0),
    stringsAsFactors = FALSE
  )
  
  for (tbl in tables_to_clean) {
    
    pre <- get_scalar(DBI::dbGetQuery(
      connection,
      sprintf(qry_count, tbl, sid, created_days_sql, created_by_clause, updated_cutoff_sql)
    ))
    
    if (pre > 0) {
      message(sprintf("  %s: %d rows to delete (preview up to 50)", tbl, pre))
      preview <- DBI::dbGetQuery(
        connection,
        sprintf(qry_preview, tbl, sid, created_days_sql, created_by_clause, updated_cutoff_sql)
      )
      print(preview)
    } else {
      message(sprintf("  %s: 0 rows to delete", tbl))
    }
    
    deleted_reported <- tryCatch(
      as.integer(DBI::dbExecute(
        connection,
        sprintf(qry_delete, tbl, sid, created_days_sql, created_by_clause, updated_cutoff_sql)
      )),
      error = function(e) {
        ok <<- FALSE
        message(sprintf("  ERROR deleting from %s: %s", tbl, e$message))
        NA_integer_
      }
    )
    
    post <- get_scalar(DBI::dbGetQuery(
      connection,
      sprintf(qry_count, tbl, sid, created_days_sql, created_by_clause, updated_cutoff_sql)
    ))
    
    if (!isTRUE(post == 0L)) ok <- FALSE
    
    station_log <- rbind(
      station_log,
      data.frame(
        station_id = sid,
        table_name = tbl,
        pre_bad = pre,
        deleted_reported = deleted_reported,
        post_bad = post,
        stringsAsFactors = FALSE
      )
    )
  }
  
  if (ok) {
    DBI::dbCommit(connection)
    message(sprintf("Committed station %d", sid))
  } else {
    DBI::dbRollback(connection)
    message(sprintf("Rolled back station %d (verification failed)", sid))
  }
  
  logs[[as.character(sid)]] <- station_log
}

delete_log <- do.call(rbind, logs)
print(delete_log, row.names = FALSE)




