# nwtclimate: `gnwt-operational` branch

This branch is a **snapshot of the code GNWT (ECC, NT Hydrology) runs operationally**, saved in October 2026. It is **not meant to be merged into `main`**. `main` has since been restructured into an installable package, and some GNWT workflows (transboundary reporting, the climate database load, FTS downloads) depend on the file layout kept here.

**Use this branch if** you are running anything described in:
- `NT_Hydrology/Abiotic Monitoring/ClimateDB/Climate_data_load_how_to.md`
- the Transboundary README (https://github.com/EmmaGRiley/NWTHydroclimate/tree/main/Transboundary)
- *R coding session – instructions.docx*

---

## ⚠️ Climate database uploads

**Don't upload climate data to the ECC_CLIMATE database (UAT or PROD) while Emma Gregory is on leave** unless it has been specifically arranged. Reading data from the database is fine.

---

## Download and set up

1. On GitHub, switch the branch dropdown from **main** to **gnwt-operational**, then **Code → Download ZIP**.
2. Unzip, and rename the folder to exactly `nwtclimate`. Put it at:
   `C:/Users/<you>/Documents/R_Scripts/Packages/nwtclimate/`
3. Copy the large `.rds` data files (H: drive / USB, see *R coding session – instructions.docx*) into `nwtclimate/data/`. They are too big for GitHub.
4. **Only if you run the climate database load or FTS scripts:** copy the `operational_scripts/` folder to
   `C:/Users/<you>/Documents/R_Scripts/Packages/nwtclimate_local/R/`
   That's where `Climate_data_load_how_to.md` expects them.

## Passwords: `.Renviron`, never in scripts

No passwords are stored in this repository. The scripts read them from your personal `.Renviron` file. In R, run `usethis::edit_r_environ()`, add the lines you need, save, and **restart R**:

```
# Climate database, read-only (most people only need this)
ORACLE_CLIM_USER=CLIMATE_VIEWER
ORACLE_CLIM_PWD=<from ISSS>

# Climate database, edit account (database loads / fixes only)
ORACLE_CLIM_EDIT_PWD=<from ISSS>

# FTS 360 data portal (FTS download scripts only)
FTS_EMAIL=<FTS account email>
FTS_PWD=<FTS account password>
```

**Never** type a password into a script you might share or push to GitHub. If one gets pushed by mistake, deleting it from the file doesn't remove it from GitHub's history, and the password must be changed.

---

## What's here

### `R/`: nwtclimate functions (sourced by other scripts)
| File | Purpose |
|---|---|
| `dependencies_functions.R` | Shared helpers, incl. `flag_data()` QC. **Sourced by the climate load scripts and Transboundary.** |
| `dependencies_data.R`, `download.R`, `update.R`, `filter.R`, `stitch.R`, `nt_merge.R`, `merge.R` | Build the ECCC merged climate dataset. Run through `update_climate.R` (NWTHydroclimate repo) |
| `Climate_database_getfiles_memory_efficient.R` | Read climate data from ECC_CLIMATE (read-only) |
| `Clim_stn_map.R` | Map of climate stations from the database |
| `cd_prep.R`, `climate_bindfiles.R`, `merge_ECC_ECCC.R`, `replace_coordinates.R`, `split_sites.R`, `API_import.R`, `FTS_download_function.R` | Older/supporting versions. The operational copies are in `operational_scripts/` |

### `operational_scripts/`: GNWT-only workflows (copy to `nwtclimate_local/R/`)
| File | Purpose |
|---|---|
| `climate_stn_data_convert.R` | Step A of the climate load: logger Excel → station CSVs |
| `climate_data_update.R` | Step B: bind, clean and flag all stations → `allstations_flagged_<year>.rds` |
| `cd_prep.R` | Step C: per-variable CSVs for Data Express upload |
| `climate_readonly_connection.R` | Test connection to ECC_CLIMATE (read-only) |
| `Climate_fix_CD_TIME_duplicates.R`, `FTS_fix_duplicates_2026.R` | One-off database fixes (CLIMATE_EDIT). **Don't run without Emma.** |
| `FTS_download*.R`, `FTS_*` | FTS station download/upload. Normally automated on the GNWT server, so don't run these manually |
| `map_rain_realtime.R`, `map_station_data.R`, `climate_stn_map.R` | Maps of station data |
| `QAQC_meltingsnow.R` | Manual QC of tipping-bucket snowmelt |
| others | Local copies of package functions used by the above |

The full climate load procedure is in `Climate_data_load_how_to.md` (ClimateDB folder on the shared drive).
