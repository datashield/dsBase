# 
# Molgenis' Armadillo - Upload Testing Datasets
#

library(MolgenisArmadillo)

upload_testing_dataset_table <- function(project_name, folder_name, table_name, dataset_file_name) {
    dataset_name <- load(file = dataset_file_name)
    dataset      <- eval(as.symbol(dataset_name))
    MolgenisArmadillo::armadillo.upload_table(project_name, folder_name, dataset, table_name)
}

# MolgenisArmadillo::armadillo.set_credentials(server = 'http://127.0.0.1:9000', access_key = "molgenis", secret_key = "molgenis")
MolgenisArmadillo::armadillo.login_basic(armadillo = 'http://127.0.0.1:8080', username = "admin", password = "admin")

if (! 'datashield' %in% MolgenisArmadillo::armadillo.list_projects())
    MolgenisArmadillo::armadillo.create_project('datashield')

upload_testing_dataset_table('datashield', 'cnsim', 'CNSIM1', 'CNSIM/CNSIM1.rda')
upload_testing_dataset_table('datashield', 'cnsim', 'CNSIM2', 'CNSIM/CNSIM2.rda')
upload_testing_dataset_table('datashield', 'cnsim', 'CNSIM3', 'CNSIM/CNSIM3.rda')

upload_testing_dataset_table('datashield', 'dasim', 'DASIM1', 'DASIM/DASIM1.rda')
upload_testing_dataset_table('datashield', 'dasim', 'DASIM2', 'DASIM/DASIM2.rda')
upload_testing_dataset_table('datashield', 'dasim', 'DASIM3', 'DASIM/DASIM3.rda')

print(MolgenisArmadillo::armadillo.list_tables('datashield'))
