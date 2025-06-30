# 
# Obiba's Opal - Upload Testing Datasets
#

library(DSOpal)
library(opalr)
library(tibble)

upload_testing_dataset_table <- function(opal, project_name, table_name, local_file_path) {
    if (! opal.project_exists(opal, project_name))
        opal.project_create(opal, project_name, database = "mongodb")
  
    dataset_name <- load(file = local_file_path)
    dataset      <- eval(as.symbol(dataset_name))
    data         <- as_tibble(dataset, rownames = '_row_id_')
  
    opal.table_save(opal, data, project_name, table_name, id.name = "_row_id_", force = TRUE)
}

# opal <- opal.login('administrator','datashield_test&', url='https://192.168.56.100:8443/', opts = list(ssl_verifyhost=0, ssl_verifypeer=0))
opal <- opal.login('administrator','datashield_test&', url='https://localhost:8443/', opts = list(ssl_verifyhost=0, ssl_verifypeer=0))

upload_testing_dataset_table(opal, 'DASIM', 'DASIM1', 'DASIM/DASIM1.rda')
upload_testing_dataset_table(opal, 'DASIM', 'DASIM2', 'DASIM/DASIM2.rda')
upload_testing_dataset_table(opal, 'DASIM', 'DASIM3', 'DASIM/DASIM3.rda')

upload_testing_dataset_table(opal, 'DASIM', 'DASIM1', 'DASIM/DASIM1.rda')
upload_testing_dataset_table(opal, 'DASIM', 'DASIM2', 'DASIM/DASIM2.rda')
upload_testing_dataset_table(opal, 'DASIM', 'DASIM3', 'DASIM/DASIM3.rda')

opal.logout(opal)
