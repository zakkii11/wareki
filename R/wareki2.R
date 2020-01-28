#' Convert Japanese Calendar, Gen-gou, into Westerm Year in Sentences.
#'
#'
#' @param document Text Data including Japanese Calendar. Must be Character vector.
#' @param dummyText If something Trouble with the Output, the dummyText, a Piece of Text temporarily  added to document, may be the cause. If needed, instead of the default dummyText, you may use your original dummyText, which is recommended to be a reasonably long String without using numeric.
#' @param loop Whether the Process applies again and again to a Part once converted. Default FALSE is strongly recommended.
#'
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_sub
#' @importFrom stringr str_detect
#' @importFrom dplyr %>%
#' @importFrom magrittr %<>%
#' @importFrom utils data
#'
#'
#' @return Character Vector. Document Converted.
#' @export
#'
wareki2 <- function(document, dummyText = NA, loop = FALSE) {
  document_vector <- c()
  for(document_x in document) {
    if (is.na(dummyText)) {
      dummyText <- "dummymojiretsudesuyo_saigonikeshimasu"
    }
    for (gengou in gengouList[, "gengou"]) {
      gengouFirstYear <- gengouList[gengouList$gengo == gengou, "year"] %>% as.numeric()
      detectYear <- str_c("(", gengou, ".", "|", gengou, "..", "|", gengou, "...", ")")
      while (str_detect(document_x, pattern = detectYear)) {
        startYear <- regexpr(detectYear, document_x, useBytes = F)
        endYear <- startYear + attr(startYear, "match.length") - 1
        myYear <- substr(document_x, startYear, endYear)
        numericDetect <- str_c("(", gengou, ")")
        waYear <- gsub(numericDetect, "", myYear)
        waYear1 <- waYear
        waYearX1 <- waYear
        if (waYear1 == "\u5143") {
          waYear1 <- as.numeric(1)
        }
        romaNumeric <- suppressWarnings(as.numeric(waYear1))
        beforeText <- str_c(gengou, waYearX1)
        if (is.na(romaNumeric)) {
          waYear2 <- str_sub(waYear, end = -2)
          waYearX2 <- waYear2
          if (waYear2 == "\u5143") {
            waYear2 <- as.numeric(1)
          }
          romaNumeric <- suppressWarnings(as.numeric(waYear2))
          beforeText <- str_c(gengou, waYearX2)
        }
        if (is.na(romaNumeric)) {
          waYear3 <- str_sub(waYear, end = -3)
          waYearX3 <- waYear3
          if (waYear3 == "\u5143") {
            waYear3 <- as.numeric(1)
          }
          romaNumeric <- suppressWarnings(as.numeric(waYear3))
          beforeText <- str_c(gengou, waYearX3)
        }
        if (is.na(romaNumeric)) {
          excluded <- as.character(waYear)
          temp <- str_c(substr(gengou, 1, 1), dummyText, substr(gengou, 2, 2), excluded)
          document_x <- gsub(myYear, temp, document_x) %>% as.character()
        } else {
          seireki <- as.numeric(gengouFirstYear) + as.numeric(romaNumeric) - 1
          seireki %<>% as.character()
          if (loop == FALSE) {
            seireki <- str_c(dummyText, seireki, dummyText)
          }
          document_x <- gsub(beforeText, seireki, document_x) %>% as.character()
        }
      }
    }
    document_vector <- c(document_vector, gsub(as.character(dummyText), "", document_x))
  }
  document_vector
}
