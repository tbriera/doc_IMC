################################################################################
##########  IMACLIM - R documentation : technical assumptions  #################
################################################################################


###########################
####### LIBRAIRIES and ####
#######  FUNCTIONS ########
###########################

library(tidyverse)
library(readr) #read_tsv
library(tidyr) #drop_na
library(kableExtra)

###########################
####### DATA PATHS ########
###########################

dir <- getwd()
data_path <- paste0(dir, "/data")

###########################
#######   INIT    #########
###########################

combi <- "" #no combi
### All variables
var_list <- c("rho_export", "LR_export", "avail_load", "OM_var", "OM_fix", "Life_time", "CINV_MW_ref")
### Variables with regional diff : removed lifetime and learning rate
var_list_reg <- c("rho_export", "avail_load", "OM_fix", "CINV_MW_ref")

 techno_list <- c("PFC", "PSS", "ICG", "CGS", "SUB", "USC", "UCS", "GGT", "GGS", "GGC", "OCT", "OGC", "HYD", "NUC", "NND", "CSP", "CHP", "SHY", "WND", "WNO", "CPV", "RPV", "BIGCC", "BIGCCS", "BF2", "BGT")
techno_unused <- c("NND", "CHP", "SHY", "BF2", "BGT")
imc_reg <- c("USA", "CAN", "EUR", "OECD", "FSU", "CHN", "IND", "BRA", "MDE", "AFR", "RAS", "RAL")

var_total <- list()
var_total_desc <- list()
###########################
####### IMPORTING #########
###########################

for(i in var_list){

  var_total[[i]] <- read_tsv(paste0(data_path, "/", i, combi, ".tsv"), col_names = FALSE)
  colnames(var_total[[i]]) <- techno_list
  var_total_desc[[i]] <- c(read_csv2(paste0("../figures&tables/supplementary/sources_", i, ".csv"), col_names = FALSE, col_types = "c"))

}

###########################
####### PROCESSING ########
###########################

#removing NA's only columns
var_total <- lapply(var_total, function(x) discard(x, ~all(is.na(.))))

#removing ununsed technos
var_total <- lapply(var_total, function(x) select(x, -ends_with(techno_unused)))

# adding imc_reg list to the regional datasets only
var_total[c(var_list_reg)] <- lapply(var_total[c(var_list_reg)], function(x) cbind(imc_reg, x))

#removing colnames for table, can't with lapply
for(i in var_list_reg){
  colnames(var_total[[i]])[[1]] <- ""
}


###########################
###### LATEX EXPORT #######
###########################

export_kable_latex <- function(x, y, cap) {
  kbl(x, format = "latex", caption = cap, booktabs = TRUE, digits = 2, longtable = TRUE) %>%
    kable_styling(latex_options = c("striped", "hold_position"), font_size = 8) %>%
    footnote(general = y,
          general_title = "Sources:", title_format = c("italic", "underline"), footnote_as_chunk = TRUE, threeparttable = TRUE)
}

rho_export <- export_kable_latex(var_total[["rho_export"]], var_total_desc[["rho_export"]], "Energy efficiency (rho), in %")

Life_time <- export_kable_latex(var_total[["Life_time"]], var_total_desc[["Life_time"]], "Lifetimes, in year")

CINV_MW_ref <- export_kable_latex(var_total[["CINV_MW_ref"]], var_total_desc[["CINV_MW_ref"]], "CAPEX, in thousand 2010$ per MW")

OM_var <- export_kable_latex(var_total[["OM_var"]], var_total_desc[["OM_var"]], "Variable O&M, in 2010$ per MWh")

OM_fix <- export_kable_latex(var_total[["OM_fix"]], var_total_desc[["OM_fix"]], "Fixed O&M, in thousand 2010$ per MW")

avail_load <- export_kable_latex(var_total[["avail_load"]], var_total_desc[["avail_load"]], "Availability factor (for dispatchable plants)/ Load factor (for variable renewable plants), in % ")

LR_export <- export_kable_latex(var_total[["LR_export"]], var_total_desc[["LR_export"]], "Learning rates, in %")

for (i in var_list){
  save_kable(get(i), paste0("../figures&tables/", i, ".tex"))
}
