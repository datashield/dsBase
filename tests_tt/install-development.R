#!/usr/bin/env Rscript

#
# Install dsbase package on the R server of a opal
#

library(opaladmin)
o <- opal.login('dsadmin', 'password', 'http://localhost:8080')
dsadmin.install_package(o, 'dsbase', ref='master')

#
# Configure datashield methods in opal
#

# Clean all
dsadmin.rm_methods(o)
# Publish dsbase package methods
dsadmin.set_package_methods(o, 'dsbase')


