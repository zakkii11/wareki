#' Convert Japanese Calendar, Gen-gou, into Westerm Year in sentences.
#'
#'
#' @param document Text Data including Japanese Calendar. Must be Character vector.
#' @param dummyText If something Trouble with the Output, the dummyText, a Piece of Text temporarily added to document, may be the cause. If needed, instead of the default dummyText, you may use your original dummyText, which is recommended to be a reasonably long String without using Numeric.
#' @param loop Whether the Process applies again and again to a Part once converted. Default FALSE is strongly recommended.
#'
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_sub
#' @importFrom stringr str_detect
#' @importFrom dplyr %>%
#' @importFrom utils read.csv
#' @importFrom utils data
#'
#' @return Character Vector. Document Converted.
#' @export
#'
wareki <- function(document, dummyText = NA, loop = FALSE) {
  document_vector <- c()
  for(document_x in document) {
    if (is.na(dummyText)) {
      dummyText <- "dummymojiretsudesuyo_saigonikeshimasu"
    }
    for (gengou in gengouList[, "gengou"]) {
      gengouFirstYear <- gengouList[gengouList$gengou == gengou, "year"]
      detectYear <- str_c("(", gengou, ".\u5e74|", gengou, "..\u5e74|", gengou, "...\u5e74)")
      while (str_detect(document_x, pattern = detectYear)) {
        startYear <- regexpr(detectYear, document_x, useBytes = F)
        endYear <- startYear + attr(startYear, "match.length") - 1
        myYear <- substr(document_x, startYear, endYear)
        if (str_sub(myYear, start = 3, end = 4) == gengou) {
          startYear <- regexpr(detectYear, document_x) + 2
          myYear <- substr(document_x, startYear, endYear)
        }
        if (str_sub(myYear, start = 4, end = 5) == gengou) {
          replacementTx <- substr(document_x, startYear + 2, endYear)
          replacementTx <- str_c(gengou, dummyText, replacementTx)
          document_x <- gsub(myYear, replacementTx, document_x)
        } else {
          numericDetect <- str_c("(", gengou, "|\u5e74)")
          waYear <- gsub(numericDetect, "", myYear)
          if (waYear == "\u5143") {
            waYear <- as.numeric(1)
          }
          romaNumeric <- suppressWarnings(as.numeric(waYear))
          if (is.na(romaNumeric)) {
            excluded <- as.character(waYear)
            temp <- str_c(gengou, dummyText, excluded, "\u5e74")
            document_x <- gsub(myYear, temp, document_x) %>% as.character()
          }else{
            seireki <- gengouFirstYear + romaNumeric - 1
            if (loop == FALSE) {
              convertedYear <- str_c(dummyText, seireki, "\u5e74")
            } else {
              convertedYear <- str_c(seireki, "\u5e74")
            }
            document_x <- gsub(myYear, convertedYear, document_x) %>% as.character()
          }
        }
      }
    }
    document_vector <- c(document_vector, gsub(as.character(dummyText), "", document_x))
  }
  document_vector
}
