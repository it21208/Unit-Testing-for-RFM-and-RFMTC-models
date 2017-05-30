 library('RUnit')
 library('testthat')

 source("C:/Users/alex/Documents/R/BloodTransfusion/RFM_BloodTranfusion_KFOLD/MyRLibs/myRFMLib.R")
 source("C:/Users/alex/Documents/R/BloodTransfusion/RFM_BloodTranfusion_KFOLD/RFM KFOLD Using the Blood Transfusion Dataset_v6.R")


 test.suite <- defineTestSuite("test.suite",
                               dirs = file.path("C:/Users/alex/Documents/R/BloodTransfusion/RFM_BloodTranfusion_KFOLD/tests"),
                               testFileRegexp = '^\\d+\\.R')

 test.result <- runTestSuite(test.suite)

 printTextProtocol(test.result)
 









