## testing retreival of Google Sheets info  to R
##
#gSheets_package <- c("googlesheets","plyr", "dplyr", "tidyr", "magrittr", "htmlTable")
gDrive_package <- c("googledrive","plyr", "dplyr", "tidyr", "magrittr", "htmlTable", "googlesheets")
if(!all(gDrive_package %in% (.packages()))) {invisible(lapply(gDrive_package, library, character.only=T, quietly = T, warn.conflicts = F, verbose=F))}
## check for packages
(.packages())
####  USE `googledrive` pkg in adidtion to g-sheets, for now...

HAB19_tbl <- drive_find(type="spreadsheet", pattern="HAB") %>%
    filter(name=="2019 HAB Partners Testing Results")

# register sheet using gs_title or gs_key

HAB19_reg <- gs_title("2019 HAB Partners Testing Results")

HAB19_dat <- gs_read(HAB19_reg, ws="HAB Lab Results",check.names=T)

HAB19_dat
