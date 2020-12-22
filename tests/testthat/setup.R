#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------
#
# Datashield test suite set up
#

context("setup - start")

library(RANN)
library(nlme)
library(stringr)
library(lme4)

source("disclosure/set_disclosure_settings.R")
source("random/set_random_seed_settings.R")

ds.test_env <- new.env()
ds.test_env$sub_env <- new.env()
ds.test_env$local.values.1 <- read.csv("data_files/DATASET1.csv", header = TRUE)
ds.test_env$local.values.2 <- read.csv("data_files/DATASET2.csv", header = TRUE)
ds.test_env$local.values.3 <- read.csv("data_files/DATASET3.csv", header = TRUE)
ds.test_env$local.values   <- rbind(ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3)
ds.test_env$null_values <- NULL
ds.test_env$sunny_day <- TRUE
ds.test_env$pie_to_eat <- 3.14
ds.test_env$pie_to_eat_integer <- as.integer(3)
ds.test_env$name <- "coronavirus"
ds.test_env$factor <- factor(c("Male", "Female", "Female", "Male", "Male","InterSex","Eunuch","Female"))
ds.test_env$list_gender_age <- list(1:100, 1:300)
ds.test_env$matrix_age <-matrix(c(4, 5, 34, 6, 7,0,9,4), nrow=2, ncol=4)


context("setup - done")
