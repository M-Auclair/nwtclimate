#Climate database connection

install.packages("odbc")
install.packages("DBI")

#Climate production viewer connection
connection <- odbc::dbConnect(drv = odbc::odbc(), dsn = "ECC_CLIMATE", uid = Sys.getenv("ORACLE_CLIM_USER"), pwd = Sys.getenv("ORACLE_CLIM_PWD"))

#access climate table - this is fairly slow
climate <- DBI::dbReadTable(connection, DBI::Id(schema = "ECC_CLIMATE", table = "CLIMATE_PIVOT_MV"), data.frame())
