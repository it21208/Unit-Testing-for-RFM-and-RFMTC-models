# check for string mismatches for the global constants and main section
test.String_Mismatches_Main <- function()
{
  checkEquals(
    "CDNOW RFM K-FOLD CROSS VALIDATION DATASET",
    RUN_TITLE,
    "<RUN_TITLE> string mismatch!"
  )
  checkEquals("Recency (months)",
              recencyColName,
              "<recencyColName> string mismatch!")
  checkEquals("Frequency (times)",
              frequencyColName,
              "<frequencyColName> string mismatch!")
  checkEquals("Monetary ($)",
              monetaryColName,
              "<monetaryColName> string mismatch!")
  checkEquals("Time (months)",
              timesColName,
              "<timesColName> string mismatch!")
}


# check for data type mismatches in the main section 
test.Data_Type_Mismatches <- function()
{
  checkTrue(class(fileList) == "list", "<fileList> data type mismatch")
  checkTrue(class(file) == "character", "<file> data type mismatch")
  checkTrue(class(rowIndx) == "integer", "<rowIndx> data type mismatch")
  checkTrue(class(avgResDF) == "data.frame",
            "<avgResDF> data type mismatch")
  checkTrue(class(custIDvector) == "integer",
            "<custIDvector> data type mismatch")
}


# check for length mismatches
test.length_Mismatches <- function()
{
  checkTrue(avgResDFLen == 2357, "<avgResDFLen> length mismatch")
  checkTrue(length(custIDvector) == 2357, "<custIDvector> length mismatch")
}


# check for boolean mismatches for the global constants and main sections
test.String_Mismatches_MainProgram <- function()
{
  checkTrue(hasCustID == TRUE, "<hasCustID> boolean mismatch")
  checkTrue(RFM_MATRICES_PROVIDED == FALSE,
            "<RFM_MATRICES_PROVIDED> boolean mismatch")
}


#  check for certain numeric values in the the global constants and main sections
test.numeric_Mismatches_MainProgram <- function()
{
  checkEqualsNumeric(custChurnColIndex, 6, "<custChurnColIndex> numeric mismatch")
  expect_that(if (K_FOLDS==5) {rowIndx == 2345}, is_true())
 # checkEqualsNumeric(rowIndx, 2345, "<rowIndx> numeric mismatch")
}


# check kFoldPartitionCustomerDSToTrainingTestDS()
test.kFoldPartitionCustomerDSToTrainingTestDS <- function()
{
  expect_that(
    kFoldPartitionCustomerDSToTrainingTestDS(CustDSFile,
                                             hasCustID,
                                             K_FOLDS,
                                             i_fold,
                                             fileList[["TRAINING"]],
                                             fileList[["TEST"]]),
    prints_text("kFoldPartitionCustomerDSToTrainingTestDS()")
  )
  checkEquals("ID", idColName, "<idColName> string mismatch!")
  checkEquals("CustID", custIdColName, "<custIdColName> string mismatch!")
}


# check the input/output of the prepareRFMTCdatase() function
test.prepareRFMTCdataset <- function()
{
  # check the input
  checkEquals("Churn (0/1)",
              churnColName,
              "<churnColName> string mismatch!")
  checkEquals("R-Score", rScoreColName, "<rScoreColName> string mismatch!")
  checkEquals("F-Score", fScoreColName, "<fScoreColName> string mismatch!")
  checkEquals("M-Score", mScoreColName, "<mScoreColName> string mismatch!")
  checkEquals("RFM-Score",
              rfmScoreColName,
              "<rfmScoreColName> string mismatch!")
  # check the output
  checkTrue(class(rfmtcReadyDF) == "data.frame",
            "<rfmtcReadyDF> data type mismatch")
  checkEqualsNumeric(ncol(rfmtcReadyDF), 11, "<rfmtcReadyDF> numeric mismatch")
  expect_that(
    prepareRFMTCdataset(
      file,
      c(
        idColName,
        custIdColName,
        recencyColName,
        frequencyColName,
        monetaryColName,
        timesColName,
        churnColName,
        rScoreColName,
        fScoreColName,
        mScoreColName,
        rfmScoreColName
      )
    ),
    prints_text("prepareRFMTCdataset()")
  )
}


# check the input of the calcRFMSegmentationMatrices()
test.calcRFMSegmentationMatrices <- function()
{
  # check the input
  checkTrue(RFM_MATRICES_PROVIDED == FALSE,
            "<RFM_MATRICES_PROVIDED> boolean mismatch")
  checkTrue(class(CUSTSEGMMATRICE_LST) == "list",
            "<CUSTSEGMMATRICE_LST> data type mismatch")
  # get each matrice of the list
  cust_receney_segm_lst = lapply(CUSTSEGMMATRICE_LST[1], dim)
  cust_frequency_segm_lst = lapply(CUSTSEGMMATRICE_LST[2], dim)
  cust_monetary_segm_lst = lapply(CUSTSEGMMATRICE_LST[3], dim)
  # check for the dimensions of each matrice
  checkTrue(((lapply(cust_receney_segm_lst, `[[`, 1) == 5) &&  
               (lapply(cust_receney_segm_lst, `[[`, 2) == 3)) == TRUE,
            "<CUSTSEGMMATRICE_LST <Recency>> matrice dimensions incorrect")
  checkTrue(((lapply(cust_frequency_segm_lst, `[[`, 1) == 5) &&
               (lapply(cust_frequency_segm_lst, `[[`, 2) == 3)) == TRUE,
            "<CUSTSEGMMATRICE_LST <Frequency>> matrice dimensions incorrect")
  checkTrue(((lapply(cust_monetary_segm_lst, `[[`, 1) == 5) &&
               (lapply(cust_monetary_segm_lst, `[[`, 2) == 3)) == TRUE,
            "<CUSTSEGMMATRICE_LST <Monetary>> matrice dimensions incorrect")
  expect_that(calcRFMSegmentationMatrices(rfmtcReadyDF,
                                          RFM_MATRICES_PROVIDED,
                                          CUSTSEGMMATRICE_LST),
              prints_text("calcRFMSegmentationMatrices()")
  )
}


# check the output of the function loadRFMSegmentationMatrices()
test.loadRFMSegmentationMatrices <- function()
{
  receney_segm_lst = lapply(SEGMMATRICE_LST[1], dim)
  frequency_segm_lst = lapply(SEGMMATRICE_LST[2], dim)
  monetary_segm_lst = lapply(SEGMMATRICE_LST[3], dim)
  # check for the dimensions of each matrice
  checkTrue(((lapply(receney_segm_lst, `[[`, 1) == 5) && 
               (lapply(receney_segm_lst, `[[`, 2) == 3)) == TRUE,
            "<SEGMMATRICE_LST <Recency>> matrice dimensions incorrect")
  checkTrue(((lapply(frequency_segm_lst, `[[`, 1) == 3) &&
               (lapply(frequency_segm_lst, `[[`, 2) == 3)) == TRUE,
            "<SEGMMATRICE_LST <Frequency>> matrice dimensions incorrect")
  checkTrue(((lapply(monetary_segm_lst, `[[`, 1) == 5) &&
     (lapply(monetary_segm_lst, `[[`, 2) == 3)) == TRUE,
  "<SEGMMATRICE_LST <Monetary>> matrice dimensions incorrect")
  expect_that(loadRFMSegmentationMatrices(), prints_text("loadRFMSegmentationMatrices()"))
}


# check the output of the function rfmScoreCalculation()
test.rfmScoreCalculation <- function()
{
  checkTrue(class(rfmCoefficients) == "numeric",
            "<rfmCoefficients> numeric mismatch")
  # check if all values of the colun with the total RFM score have not been computed
  checkTrue(!(all.equal(rfmtcScoredDF$`RFM-Score`, 0) == TRUE),
            "<rfmtcScoredDF> total RFM score column hasn't been computed")
  expect_that(rfmScoreCalculation(rfmtcReadyDF, SEGMMATRICE_LST, rfmCoefficients), prints_text("rfmScoreCalculation()"))
}


# check the input/output of the function calculatePB_RespProb()
test.calculatePB_RespProb <- function()
{
  checkTrue((m >= 0) == TRUE, "value of moving average is negative")
  checkTrue(length(avgResDF) == 5, "value of moving average is negative")
  expect_that(calculatePB_RespProb(avgResDF, m, "OUR_RFM"), prints_text("calculatePB_RespProb"))
}