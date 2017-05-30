library('RUnit')
library('testthat')

source("C:/Users/alex/Documents/R/BloodTransfusion/RFMTC_BloodTransfusion_KFOLD/MyRLibs/myRFMLib.R")
source("C:/Users/alex/Documents/R/BloodTransfusion/RFMTC_BloodTransfusion_KFOLD/RFMTC_KFOLD_BloodTransfusion_v6.R")


test.suite <- defineTestSuite("test.suite",
                              dirs = file.path("C:/Users/alex/Documents/R/BloodTransfusion/RFMTC_BloodTransfusion_KFOLD/tests"),
                              testFileRegexp = '^\\d+\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
