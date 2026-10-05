# Climate database - fix CD_TIME format inconsistency + duplicate records
#
# Problem:
#   Some rows have CD_TIME as "X:00" (e.g., "3:00"); others use "0X:00" ("03:00").
#   For certain (STATION_ID, DATA_TYPE_ID, CD_YEAR, CD_MONTH, CD_DAY, hour) keys,
#   BOTH formats exist with DIFFERENT DATA_VALUE (e.g., sign-flipped T_AIR_HIGH_C
#   at Snare rapids 2023-10-17). The newer CREATED load is the authoritative one.
#
# Scope (after comparing UAT vs PROD conflict_stats):
#   * PROD DATA_TYPE_ID 5 (rain/wind) is already clean -- previously fixed by
#     FTS_fix_duplicates_2026.R.
#   * PROD DATA_TYPE_ID 11 (air temperature) still has ~4,700 conflict groups
#     across stations 1, 2, 22, 28 in CD_CLIMATE_DATA_1. This script targets
#     exactly that scope.
#
# Plan:
#   0. Back up in-scope rows locally as .rds
#   1. Audit: count unpadded rows + duplicate groups in scope
#   2. Review conflicting duplicate groups before deletion
#   3. Delete older-CREATED duplicate record per normalized key (scoped)
#   4. Normalize surviving CD_TIME to "0X:00" via LPAD (scoped)
#   5. Verify
#   6. Add CHECK + UNIQUE constraints to prevent recurrence
#
# NOTE: these are DIFFERENT duplicates than FTS_fix_duplicates_2026.R. That
# script deleted unpadded rows outright because padded siblings were known-good.
# Here we may be deleting the PADDED row if it's the older one — the rule is
# "newest CREATED wins", not "padded wins".

# ------------------------------------------------------------------
# SCOPE -- target stations + variable + table
# ------------------------------------------------------------------
# Station 28 (Winter Lake FTS) excluded from newest-wins scope:
# its Sep 18 2025 reload shifted timestamps +1h (bad UTC->MT conversion).
# Dec 5 2024 load matches the raw sensor file -- so for station 28, OLDER
# wins. Handled separately at the bottom of this script.
target_stations     <- c(1, 2, 22)
target_data_type_id <- 11
target_table        <- "ECC_CLIMATE.CD_CLIMATE_DATA_1"

# SQL fragment used to restrict Step 3 DELETE and Step 4 UPDATE to scope.
scope_clause <- sprintf(
  "STATION_ID IN (%s) AND DATA_TYPE_ID = %d",
  paste(target_stations, collapse = ","), target_data_type_id
)

# ------------------------------------------------------------------
# Connection -- PROD edit
# ------------------------------------------------------------------
connection <- odbc::dbConnect(drv = odbc::odbc(), dsn = "ECC_CLIMATE",
                              uid = "CLIMATE_EDIT", pwd = Sys.getenv("ORACLE_CLIM_EDIT_PWD"))

get_scalar <- function(df) as.integer(df[[1]][1])

# ============================================================
# STEP 0 -- Local .rds backup of in-scope rows (RUN ONCE before Step 3/4)
# ============================================================
# Pulls every row in the target table matching the scope. Includes both
# padded and unpadded members of any duplicate pair, so the backup is
# sufficient to reconstruct anything Step 3 deletes.

# backup_dir <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")),
#                      "/Documents/R_Scripts/Packages/nwtclimate/data/db_backups/")
# if (!dir.exists(backup_dir)) dir.create(backup_dir, recursive = TRUE)
#
# backup_df <- DBI::dbGetQuery(connection,
#   sprintf("SELECT * FROM %s WHERE %s", target_table, scope_clause))
# backup_file <- file.path(backup_dir,
#   sprintf("CD_CLIMATE_DATA_1_scope_BAK_%s.rds", format(Sys.Date(), "%Y%m%d")))
# saveRDS(backup_df, backup_file)
# message(sprintf("Backed up %d rows to %s (%.1f MB)",
#                 nrow(backup_df), backup_file,
#                 file.info(backup_file)$size / 1e6))

# ============================================================
# STEP 1 -- Audit: unpadded rows + duplicate groups in scope
# ============================================================

# 1a. Total unpadded rows in scope
qry_unpadded_count <- sprintf("
SELECT COUNT(*) AS n_unpadded
FROM %s
WHERE LENGTH(CD_TIME) = 4 AND %s
", target_table, scope_clause)

n_unpadded <- get_scalar(DBI::dbGetQuery(connection, qry_unpadded_count))
message(sprintf("%s (scope): %d unpadded rows", target_table, n_unpadded))

# 1b. Unpadded rows per station
qry_unpadded_by_station <- sprintf("
SELECT STATION_ID, COUNT(*) AS n_unpadded
FROM %s
WHERE LENGTH(CD_TIME) = 4 AND %s
GROUP BY STATION_ID
ORDER BY n_unpadded DESC
", target_table, scope_clause)

unpadded_by_station <- DBI::dbGetQuery(connection, qry_unpadded_by_station)
unpadded_by_station

# 1c. Duplicate groups in scope (collide when we LPAD)
qry_dup_groups <- sprintf("
SELECT STATION_ID, CD_YEAR, CD_MONTH, CD_DAY,
       LPAD(CD_TIME, 5, '0') AS CD_TIME_NORM,
       COUNT(*) AS n_rows
FROM %s
WHERE %s
GROUP BY STATION_ID, CD_YEAR, CD_MONTH, CD_DAY, LPAD(CD_TIME, 5, '0')
HAVING COUNT(*) > 1
ORDER BY STATION_ID, CD_YEAR, CD_MONTH, CD_DAY, CD_TIME_NORM
", target_table, scope_clause)

dup_groups <- DBI::dbGetQuery(connection, qry_dup_groups)
message(sprintf("%s (scope): %d duplicate groups", target_table, nrow(dup_groups)))
head(dup_groups)

# ============================================================
# STEP 2 -- Quantify agreement vs. conflict per station
# ============================================================
# Shows how many groups actually have DIFFERENT values (true conflicts,
# where newest-wins matters) vs. identical values (harmless double-loads).

qry_conflict_stats <- sprintf("
WITH normalized AS (
  SELECT STATION_ID,
         CD_YEAR, CD_MONTH, CD_DAY,
         LPAD(CD_TIME, 5, '0') AS CD_TIME_NORM,
         DATA_VALUE
  FROM %s
  WHERE %s
)
SELECT STATION_ID,
       SUM(CASE WHEN n_distinct_vals > 1 THEN 1 ELSE 0 END) AS n_conflicts,
       SUM(CASE WHEN n_distinct_vals = 1 THEN 1 ELSE 0 END) AS n_agree,
       COUNT(*) AS n_groups
FROM (
  SELECT STATION_ID, CD_YEAR, CD_MONTH, CD_DAY, CD_TIME_NORM,
         COUNT(DISTINCT DATA_VALUE) AS n_distinct_vals
  FROM normalized
  GROUP BY STATION_ID, CD_YEAR, CD_MONTH, CD_DAY, CD_TIME_NORM
  HAVING COUNT(*) > 1
)
GROUP BY STATION_ID
ORDER BY n_conflicts DESC, n_groups DESC
", target_table, scope_clause)

conflict_stats <- DBI::dbGetQuery(connection, qry_conflict_stats)
print(conflict_stats)

# ---------- Drill-down: pull conflicting rows per target station ----------
# Use this to confirm newest-CREATED is the authoritative value.
# Station 22 (Snare rapids) T_AIR is known to be newest-wins from prior
# investigation. Check 1, 2, 28 for the same pattern before running Step 3.

qry_conflicts_only <- sprintf("
WITH normalized AS (
  SELECT CD_ID, STATION_ID, DATA_TYPE_ID,
         CD_YEAR, CD_MONTH, CD_DAY, CD_TIME,
         LPAD(CD_TIME, 5, '0') AS CD_TIME_NORM,
         DATA_VALUE, CREATED, CREATED_BY
  FROM %s
  WHERE %%s = STATION_ID AND %s
),
conflict_keys AS (
  SELECT STATION_ID, CD_YEAR, CD_MONTH, CD_DAY, CD_TIME_NORM
  FROM normalized
  GROUP BY STATION_ID, CD_YEAR, CD_MONTH, CD_DAY, CD_TIME_NORM
  HAVING COUNT(DISTINCT DATA_VALUE) > 1
)
SELECT n.*
FROM normalized n
JOIN conflict_keys c
  ON n.STATION_ID = c.STATION_ID
 AND n.CD_YEAR    = c.CD_YEAR
 AND n.CD_MONTH   = c.CD_MONTH
 AND n.CD_DAY     = c.CD_DAY
 AND n.CD_TIME_NORM = c.CD_TIME_NORM
ORDER BY n.STATION_ID, n.CD_YEAR, n.CD_MONTH, n.CD_DAY,
         n.CD_TIME_NORM, n.CREATED
", target_table, scope_clause)
# Template contains %%s (escaped) -- the inner sprintf fills in the station id.

conflict_samples <- list()
for (sid in target_stations) {
  message(sprintf("\n=== Station %d ===", sid))
  df <- DBI::dbGetQuery(connection, sprintf(qry_conflicts_only, sid))
  conflict_samples[[as.character(sid)]] <- df
  print(head(df, 20))
}

# ---> INSPECTED 2026-04-15: newest-wins confirmed for all four stations.
#   * Stations 1, 2, 22: older load had signs stripped (positives where
#     negatives were physically correct). 2025-09-15 reload restored the
#     negatives. Authoritative.
#   * Station 28: older load was off by one hour (each newer value matches
#     the older value of the timestamp one hour earlier). 2025-09-18 reload
#     is a timezone/DST correction. Authoritative.
# Proceed to Step 3.

# ============================================================
# STEP 3 -- Delete older-CREATED duplicate per normalized key (scoped)
# ============================================================
# ROW_NUMBER() partitions by the NORMALIZED key so rows differing only in
# CD_TIME padding are grouped together. Newest CREATED gets rn = 1 and
# survives; older siblings (rn > 1) are deleted.
# Scope applied BOTH in outer DELETE and inside ROW_NUMBER's source.

qry_dedupe_delete <- sprintf("
DELETE FROM %s
WHERE %s
  AND CD_ID IN (
    SELECT CD_ID FROM (
      SELECT CD_ID,
             ROW_NUMBER() OVER (
               PARTITION BY STATION_ID, DATA_TYPE_ID,
                            CD_YEAR, CD_MONTH, CD_DAY,
                            LPAD(CD_TIME, 5, '0')
               ORDER BY CREATED DESC, CD_ID DESC
             ) AS rn
      FROM %s
      WHERE %s
    )
    WHERE rn > 1
  )
", target_table, scope_clause, target_table, scope_clause)

qry_dup_count <- sprintf("
SELECT COUNT(*) AS n_dup_rows FROM (
  SELECT CD_ID,
         ROW_NUMBER() OVER (
           PARTITION BY STATION_ID, DATA_TYPE_ID,
                        CD_YEAR, CD_MONTH, CD_DAY,
                        LPAD(CD_TIME, 5, '0')
           ORDER BY CREATED DESC, CD_ID DESC
         ) AS rn
  FROM %s
  WHERE %s
) WHERE rn > 1
", target_table, scope_clause)

try(DBI::dbRollback(connection), silent = TRUE)
DBI::dbBegin(connection)

pre <- get_scalar(DBI::dbGetQuery(connection, qry_dup_count))
message(sprintf("%s (scope): %d older-duplicate rows to delete",
                target_table, pre))

deleted_reported <- tryCatch(
  as.integer(DBI::dbExecute(connection, qry_dedupe_delete)),
  error = function(e) { message(sprintf("  ERROR: %s", e$message)); NA_integer_ }
)

post <- get_scalar(DBI::dbGetQuery(connection, qry_dup_count))

if (isTRUE(post == 0L)) {
  DBI::dbCommit(connection)
  message(sprintf("Committed: deleted %d, %d remain in scope",
                  deleted_reported, post))
} else {
  DBI::dbRollback(connection)
  message(sprintf("Rolled back (post=%s)", post))
}

# ============================================================
# STEP 4 -- Normalize surviving CD_TIME to zero-padded HH:MM (scoped)
# ============================================================

qry_normalize <- sprintf("
UPDATE %s
SET CD_TIME = LPAD(CD_TIME, 5, '0')
WHERE LENGTH(CD_TIME) = 4
  AND %s
", target_table, scope_clause)

try(DBI::dbRollback(connection), silent = TRUE)
DBI::dbBegin(connection)

pre <- get_scalar(DBI::dbGetQuery(connection, qry_unpadded_count))
message(sprintf("%s (scope): normalizing %d rows...", target_table, pre))

updated <- tryCatch(
  as.integer(DBI::dbExecute(connection, qry_normalize)),
  error = function(e) { message(sprintf("  ERROR: %s", e$message)); NA_integer_ }
)

post <- get_scalar(DBI::dbGetQuery(connection, qry_unpadded_count))

if (isTRUE(post == 0L)) {
  DBI::dbCommit(connection)
  message(sprintf("Committed: updated %d", updated))
} else {
  DBI::dbRollback(connection)
  message(sprintf("Rolled back (post=%s)", post))
}

# ============================================================
# STEP 5 -- Verify (scoped)
# ============================================================

qry_verify_format_scoped <- sprintf(
  "SELECT COUNT(*) FROM %s WHERE %s AND LENGTH(CD_TIME) <> 5",
  target_table, scope_clause)

qry_verify_unique_scoped <- sprintf("
SELECT COUNT(*) FROM (
  SELECT STATION_ID, DATA_TYPE_ID, CD_YEAR, CD_MONTH, CD_DAY, CD_TIME
  FROM %s
  WHERE %s
  GROUP BY STATION_ID, DATA_TYPE_ID, CD_YEAR, CD_MONTH, CD_DAY, CD_TIME
  HAVING COUNT(*) > 1
)", target_table, scope_clause)

n_fmt <- get_scalar(DBI::dbGetQuery(connection, qry_verify_format_scoped))
n_dup <- get_scalar(DBI::dbGetQuery(connection, qry_verify_unique_scoped))
message(sprintf("%s (scope): unpadded=%d, duplicated_keys=%d (both expect 0)",
                target_table, n_fmt, n_dup))

# ============================================================
# STEP 5b -- Broader audit: remaining unpadded rows across ALL types
# ============================================================
# The scoped cleanup (Steps 3-4) only fixed DATA_TYPE_ID 11. Other variables
# (RAIN_MM, wind, etc.) may still have unpadded CD_TIME values. When those
# mix with padded rows of OTHER variables at the same hour, pivot_wider
# emits two half-empty rows instead of one merged row (observed in
# wide_data_v2.csv for Snare rapids Nov 2024).
#
# Before any blanket LPAD, check for duplicates table-wide -- any collision
# needs a conflict-resolution decision first (like Steps 2-3 did for type 11).

# 5b.1 -- Unpadded rows per (station, variable) across the whole table
qry_remaining_unpadded <- sprintf("
SELECT STATION_ID, DATA_TYPE_ID, COUNT(*) AS n_unpadded
FROM %s
WHERE LENGTH(CD_TIME) = 4
GROUP BY STATION_ID, DATA_TYPE_ID
ORDER BY n_unpadded DESC
", target_table)

remaining_unpadded <- DBI::dbGetQuery(connection, qry_remaining_unpadded)
message(sprintf("%s (full table): %d (station, variable) pairs still unpadded",
                target_table, nrow(remaining_unpadded)))
head(remaining_unpadded, 30)

# 5b.2 -- Would LPAD create any duplicate-key collisions?
# Count groups where (station, variable, date, normalized hour) has >1 row.
# If 0, a blanket LPAD is safe -- no row collides with another.
qry_blanket_lpad_collisions <- sprintf("
SELECT COUNT(*) AS n_collisions FROM (
  SELECT STATION_ID, DATA_TYPE_ID, CD_YEAR, CD_MONTH, CD_DAY,
         LPAD(CD_TIME, 5, '0') AS CD_TIME_NORM
  FROM %s
  GROUP BY STATION_ID, DATA_TYPE_ID, CD_YEAR, CD_MONTH, CD_DAY, LPAD(CD_TIME, 5, '0')
  HAVING COUNT(*) > 1
)", target_table)

n_collisions <- get_scalar(DBI::dbGetQuery(connection, qry_blanket_lpad_collisions))
message(sprintf("%s (full table): %d duplicate groups would collide on LPAD",
                target_table, n_collisions))

# 5b.3 -- If n_collisions == 0 -> blanket LPAD is safe.
# If n_collisions > 0 -> inspect per (station, variable) and decide
# conflict-resolution rule as we did for type 11.
#
# Inspection 2026-04-15 confirmed: the 1,580 remaining collisions are all
# on (STATION_ID=1, DATA_TYPE_ID=5) and all agree-only (same DATA_VALUE on
# both sides of every pair). Safe to dedupe with newest-wins, which for
# agree-only groups is equivalent to any other tie-breaker.
#
qry_blanket_collisions_detail <- sprintf("
SELECT STATION_ID, DATA_TYPE_ID, COUNT(*) AS n_groups
FROM (
  SELECT STATION_ID, DATA_TYPE_ID, CD_YEAR, CD_MONTH, CD_DAY,
         LPAD(CD_TIME, 5, '0') AS CD_TIME_NORM
  FROM %s
  GROUP BY STATION_ID, DATA_TYPE_ID, CD_YEAR, CD_MONTH, CD_DAY, LPAD(CD_TIME, 5, '0')
  HAVING COUNT(*) > 1
)
GROUP BY STATION_ID, DATA_TYPE_ID
ORDER BY n_groups DESC
", target_table)

blanket_collisions_detail <- DBI::dbGetQuery(connection, qry_blanket_collisions_detail)
blanket_collisions_detail

# ---- Dedupe station 1 DATA_TYPE_ID 5 agree-only collisions ----
# Run this BEFORE the blanket LPAD below. After this delete, n_collisions
# should be 0 and the blanket LPAD becomes safe.

# Safer ordering: prefer non-NULL DATA_VALUE first, then newest CREATED,
# then highest CD_ID as a final tie-break. This protects against deleting
# a real value in favor of a NULL sibling when the two exist together.
# (Diagnostic confirmed no NULLs for station 1 type 5, but the rule costs
# nothing and guards future re-runs on other scopes.)
qry_dedupe_s1t5 <- sprintf("
DELETE FROM %s
WHERE STATION_ID = 1 AND DATA_TYPE_ID = 5
  AND CD_ID IN (
    SELECT CD_ID FROM (
      SELECT CD_ID,
             ROW_NUMBER() OVER (
               PARTITION BY CD_YEAR, CD_MONTH, CD_DAY, LPAD(CD_TIME, 5, '0')
               ORDER BY CASE WHEN DATA_VALUE IS NULL THEN 1 ELSE 0 END ASC,
                        CREATED DESC,
                        CD_ID DESC
             ) AS rn
      FROM %s
      WHERE STATION_ID = 1 AND DATA_TYPE_ID = 5
    ) WHERE rn > 1
  )
", target_table, target_table)

try(DBI::dbRollback(connection), silent = TRUE)
DBI::dbBegin(connection)

pre_collisions <- get_scalar(DBI::dbGetQuery(connection, qry_blanket_lpad_collisions))
message(sprintf("Station 1 type 5: %d agree-duplicate rows to delete",
                pre_collisions))

deleted <- tryCatch(
  as.integer(DBI::dbExecute(connection, qry_dedupe_s1t5)),
  error = function(e) { message(sprintf("  ERROR: %s", e$message)); NA_integer_ }
)

post_collisions <- get_scalar(DBI::dbGetQuery(connection, qry_blanket_lpad_collisions))

if (isTRUE(post_collisions == 0L)) {
  DBI::dbCommit(connection)
  message(sprintf("Committed: deleted %d, 0 collisions remain", deleted))
} else {
  DBI::dbRollback(connection)
  message(sprintf("Rolled back (post_collisions=%s)", post_collisions))
}

# ---- Blanket LPAD (ONLY if n_collisions == 0) ----

qry_blanket_lpad <- sprintf("
UPDATE %s
SET CD_TIME = LPAD(CD_TIME, 5, '0')
WHERE LENGTH(CD_TIME) = 4
", target_table)

try(DBI::dbRollback(connection), silent = TRUE)
DBI::dbBegin(connection)

pre <- get_scalar(DBI::dbGetQuery(connection, sprintf(
  "SELECT COUNT(*) FROM %s WHERE LENGTH(CD_TIME) = 4", target_table)))
message(sprintf("%s (full table): blanket normalizing %d rows...",
                target_table, pre))

updated <- tryCatch(
  as.integer(DBI::dbExecute(connection, qry_blanket_lpad)),
  error = function(e) { message(sprintf("  ERROR: %s", e$message)); NA_integer_ }
)

post <- get_scalar(DBI::dbGetQuery(connection, sprintf(
  "SELECT COUNT(*) FROM %s WHERE LENGTH(CD_TIME) = 4", target_table)))

post_collisions <- get_scalar(DBI::dbGetQuery(connection, qry_blanket_lpad_collisions))

if (isTRUE(post == 0L) && isTRUE(post_collisions == 0L)) {
  DBI::dbCommit(connection)
  message(sprintf("Committed blanket LPAD: updated %d", updated))
} else {
  DBI::dbRollback(connection)
  message(sprintf("Rolled back (post_unpadded=%s, post_collisions=%s)",
                  post, post_collisions))
}

# ============================================================
# STEP 5c -- Fix CD_CLIMATE_DATA_3 (separate table, same issue)
# ============================================================
# Audit found 129,805 unpadded rows in CD_CLIMATE_DATA_3, all on stations
# 19, 20, 21 with DATA_TYPE_ID = 15. Zero collisions -- all are simple
# "X:00" format (0:00 .. 9:00) with no padded sibling. Blanket LPAD is safe.

# Full-table verifier across all three tables (use to confirm scope before
# and after fixes):
# DBI::dbGetQuery(connection, "
#   SELECT 'CD_CLIMATE_DATA_0' AS tbl, COUNT(*) AS n_bad_format
#   FROM ECC_CLIMATE.CD_CLIMATE_DATA_0
#   WHERE NOT REGEXP_LIKE(CD_TIME, '^[0-2][0-9]:[0-5][0-9]$') OR CD_TIME IS NULL
#   UNION ALL SELECT 'CD_CLIMATE_DATA_1', COUNT(*)
#   FROM ECC_CLIMATE.CD_CLIMATE_DATA_1
#   WHERE NOT REGEXP_LIKE(CD_TIME, '^[0-2][0-9]:[0-5][0-9]$') OR CD_TIME IS NULL
#   UNION ALL SELECT 'CD_CLIMATE_DATA_3', COUNT(*)
#   FROM ECC_CLIMATE.CD_CLIMATE_DATA_3
#   WHERE NOT REGEXP_LIKE(CD_TIME, '^[0-2][0-9]:[0-5][0-9]$') OR CD_TIME IS NULL
# ")

# ---- Blanket LPAD on CD_CLIMATE_DATA_3 ----
# qry_blanket_lpad_data3 <- "
# UPDATE ECC_CLIMATE.CD_CLIMATE_DATA_3
# SET CD_TIME = LPAD(CD_TIME, 5, '0')
# WHERE LENGTH(CD_TIME) = 4
# "
#
# try(DBI::dbRollback(connection), silent = TRUE)
# DBI::dbBegin(connection)
#
# pre <- get_scalar(DBI::dbGetQuery(connection,
#   "SELECT COUNT(*) FROM ECC_CLIMATE.CD_CLIMATE_DATA_3 WHERE LENGTH(CD_TIME) = 4"))
# message(sprintf("CD_CLIMATE_DATA_3: normalizing %d rows...", pre))
#
# updated <- tryCatch(
#   as.integer(DBI::dbExecute(connection, qry_blanket_lpad_data3)),
#   error = function(e) { message(sprintf("  ERROR: %s", e$message)); NA_integer_ }
# )
#
# post <- get_scalar(DBI::dbGetQuery(connection,
#   "SELECT COUNT(*) FROM ECC_CLIMATE.CD_CLIMATE_DATA_3 WHERE LENGTH(CD_TIME) = 4"))
#
# if (isTRUE(post == 0L)) {
#   DBI::dbCommit(connection)
#   message(sprintf("Committed: updated %d on CD_CLIMATE_DATA_3", updated))
# } else {
#   DBI::dbRollback(connection)
#   message(sprintf("Rolled back (post=%s)", post))
# }

# ============================================================
# STEP 6 -- Add constraints to prevent recurrence (whole table)
# ============================================================
# UNIQUE cannot be scoped. The ENTIRE target table must be free of unpadded
# values AND duplicate keys before adding it. Verify table-wide first:

# qry_verify_format_full <- sprintf(
#   "SELECT COUNT(*) FROM %s WHERE LENGTH(CD_TIME) <> 5", target_table)
# qry_verify_unique_full <- sprintf("
# SELECT COUNT(*) FROM (
#   SELECT STATION_ID, DATA_TYPE_ID, CD_YEAR, CD_MONTH, CD_DAY, CD_TIME
#   FROM %s
#   GROUP BY STATION_ID, DATA_TYPE_ID, CD_YEAR, CD_MONTH, CD_DAY, CD_TIME
#   HAVING COUNT(*) > 1
# )", target_table)
#
# n_fmt_full <- get_scalar(DBI::dbGetQuery(connection, qry_verify_format_full))
# n_dup_full <- get_scalar(DBI::dbGetQuery(connection, qry_verify_unique_full))
# message(sprintf("%s (full table): unpadded=%d, duplicated_keys=%d",
#                 target_table, n_fmt_full, n_dup_full))
#
# # If BOTH full-table counts are 0, add constraints:
# DBI::dbExecute(connection, sprintf(
#   "ALTER TABLE %s ADD CONSTRAINT cd1_time_format
#    CHECK (REGEXP_LIKE(CD_TIME, '^[0-2][0-9]:[0-5][0-9]$'))", target_table))
#
# DBI::dbExecute(connection, sprintf(
#   "ALTER TABLE %s ADD CONSTRAINT cd1_unique_obs UNIQUE
#    (STATION_ID, DATA_TYPE_ID, CD_YEAR, CD_MONTH, CD_DAY, CD_TIME)",
#   target_table))

# ============================================================
# STEP 7 -- Retain the local .rds backup for at least a week
# ============================================================
# Delete the .rds file manually from backup_dir once you're confident
# nothing downstream broke.

# ============================================================
# STATION 28 (Winter Lake FTS) -- reversed polarity fix
# ============================================================
# Sep 18 2025 reload shifted timestamps +1h; Dec 5 2024 load matches raw.
# So here we delete only the SEP 18 2025 rows that DUPLICATE an existing
# Dec 5 2024 row at the same normalized timestamp. New-coverage rows from
# the Sep 18 reload (i.e., dates with no Dec 5 sibling) are preserved --
# those are the only source for post-Dec-5 data.
#
# Match on normalized CD_TIME, since the Sep 18 load's CD_TIME is "X:00"
# and the Dec 5 load's is "0X:00" (both formats exist for the same hour).

# ---- Backup station 28 rows before touching them ----
# s28_backup <- DBI::dbGetQuery(connection, sprintf("
#   SELECT * FROM %s
#   WHERE STATION_ID = 28 AND DATA_TYPE_ID = 11
# ", target_table))
# saveRDS(s28_backup, file.path(backup_dir,
#   sprintf("s28_CD1_BAK_%s.rds", format(Sys.Date(), "%Y%m%d"))))
# message(sprintf("Backed up %d station-28 rows", nrow(s28_backup)))

# ---- Preview what would be deleted ----
qry_s28_dupes_preview <- sprintf("
WITH normalized AS (
  SELECT CD_ID, STATION_ID, DATA_TYPE_ID,
         CD_YEAR, CD_MONTH, CD_DAY, CD_TIME,
         LPAD(CD_TIME, 5, '0') AS CD_TIME_NORM,
         DATA_VALUE, CREATED
  FROM %s
  WHERE STATION_ID = 28 AND DATA_TYPE_ID = 11
)
SELECT new_row.*
FROM normalized new_row
WHERE TRUNC(new_row.CREATED) = DATE '2025-09-18'
  AND EXISTS (
    SELECT 1 FROM normalized old_row
    WHERE old_row.CD_YEAR      = new_row.CD_YEAR
      AND old_row.CD_MONTH     = new_row.CD_MONTH
      AND old_row.CD_DAY       = new_row.CD_DAY
      AND old_row.CD_TIME_NORM = new_row.CD_TIME_NORM
      AND old_row.CREATED      < new_row.CREATED
  )
ORDER BY new_row.CD_YEAR, new_row.CD_MONTH, new_row.CD_DAY, new_row.CD_TIME_NORM
", target_table)
#
s28_delete_preview <- DBI::dbGetQuery(connection, qry_s28_dupes_preview)
message(sprintf("Station 28: %d Sep-18 rows would be deleted (duplicates only)",
                nrow(s28_delete_preview)))
head(s28_delete_preview, 20)

# Also preview what would be PRESERVED -- Sep 18 rows with no Dec 5 sibling,
# i.e., genuine new coverage:
qry_s28_preserved <- sprintf("
WITH normalized AS (
  SELECT CD_ID, STATION_ID, CD_YEAR, CD_MONTH, CD_DAY, CD_TIME,
         LPAD(CD_TIME, 5, '0') AS CD_TIME_NORM,
         DATA_VALUE, CREATED
  FROM %s
  WHERE STATION_ID = 28 AND DATA_TYPE_ID = 11
)
SELECT new_row.*
FROM normalized new_row
WHERE TRUNC(new_row.CREATED) = DATE '2025-09-18'
  AND NOT EXISTS (
    SELECT 1 FROM normalized old_row
    WHERE old_row.CD_YEAR      = new_row.CD_YEAR
      AND old_row.CD_MONTH     = new_row.CD_MONTH
      AND old_row.CD_DAY       = new_row.CD_DAY
      AND old_row.CD_TIME_NORM = new_row.CD_TIME_NORM
      AND old_row.CREATED      < new_row.CREATED
  )
ORDER BY new_row.CD_YEAR, new_row.CD_MONTH, new_row.CD_DAY, new_row.CD_TIME_NORM
", target_table)
#
s28_preserved <- DBI::dbGetQuery(connection, qry_s28_preserved)
 message(sprintf("Station 28: %d Sep-18 rows would be PRESERVED (new coverage)",
                 nrow(s28_preserved)))
range(paste(s28_preserved$CD_YEAR, s28_preserved$CD_MONTH,
            s28_preserved$CD_DAY, sep = "-"))

# ---- Execute delete (scoped to duplicates only) ----
qry_s28_delete <- sprintf("
DELETE FROM %s
WHERE STATION_ID = 28
  AND DATA_TYPE_ID = 11
  AND TRUNC(CREATED) = DATE '2025-09-18'
  AND CD_ID IN (
    WITH normalized AS (
      SELECT CD_ID, CD_YEAR, CD_MONTH, CD_DAY,
             LPAD(CD_TIME, 5, '0') AS CD_TIME_NORM, CREATED
      FROM %s
      WHERE STATION_ID = 28 AND DATA_TYPE_ID = 11
    )
    SELECT new_row.CD_ID
    FROM normalized new_row
    WHERE TRUNC(new_row.CREATED) = DATE '2025-09-18'
      AND EXISTS (
        SELECT 1 FROM normalized old_row
        WHERE old_row.CD_YEAR      = new_row.CD_YEAR
          AND old_row.CD_MONTH     = new_row.CD_MONTH
          AND old_row.CD_DAY       = new_row.CD_DAY
          AND old_row.CD_TIME_NORM = new_row.CD_TIME_NORM
          AND old_row.CREATED      < new_row.CREATED
      )
  )
", target_table, target_table)

try(DBI::dbRollback(connection), silent = TRUE)
DBI::dbBegin(connection)

pre_count <- nrow(DBI::dbGetQuery(connection, qry_s28_dupes_preview))
message(sprintf("Station 28: deleting %d Sep-18 duplicate rows...", pre_count))

deleted_reported <- tryCatch(
  as.integer(DBI::dbExecute(connection, qry_s28_delete)),
  error = function(e) { message(sprintf("  ERROR: %s", e$message)); NA_integer_ }
)

post_count <- nrow(DBI::dbGetQuery(connection, qry_s28_dupes_preview))

if (isTRUE(post_count == 0L)) {
  DBI::dbCommit(connection)
  message(sprintf("Committed station 28: deleted %d", deleted_reported))
} else {
  DBI::dbRollback(connection)
  message(sprintf("Rolled back station 28 (post=%s)", post_count))
}

# After this delete, station 28's surviving rows should all be unpadded
# "X:00" from the Dec 5 2024 load plus any unique Sep 18 rows. Step 4
# (the scoped LPAD) does NOT include station 28 -- run a separate
# normalization specifically for it:

qry_s28_normalize <- sprintf("
UPDATE %s
SET CD_TIME = LPAD(CD_TIME, 5, '0')
WHERE STATION_ID = 28 AND DATA_TYPE_ID = 11 AND LENGTH(CD_TIME) = 4
", target_table)

try(DBI::dbRollback(connection), silent = TRUE)
DBI::dbBegin(connection)
updated <- as.integer(DBI::dbExecute(connection, qry_s28_normalize))
post <- get_scalar(DBI::dbGetQuery(connection, sprintf(
  "SELECT COUNT(*) FROM %s WHERE STATION_ID = 28 AND DATA_TYPE_ID = 11
   AND LENGTH(CD_TIME) = 4", target_table)))
if (isTRUE(post == 0L)) {
  DBI::dbCommit(connection)
  message(sprintf("Station 28: normalized %d CD_TIME values", updated))
} else {
  DBI::dbRollback(connection)
  message(sprintf("Rolled back station 28 normalize (post=%s)", post))
}
