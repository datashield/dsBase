#!/usr/bin/env Rscript

#
# Install dsbase package on the R servers of multiple opals
#

library(opaladmin)
o1 <- opal.login('dsadmin', 'password', 'http://localhost:8080')
o2 <- opal.login('dsadmin', 'password', 'https://some.opal.host:8443',opts=list(ssl.verifyhost=0,ssl.verifypeer=0,sslversion=3))
opals<-list(o1,o2)
dsadmin.install_package(opals, 'dsbase')
# Publish dsbase package methods
dsadmin.set_package_methods(opals, 'dsbase')
