#' Convert Japanese Calendar, Gen-gou, into Westerm Year in sentences.
#'
#'
#' @param document text data including Japanese Calendar.
#' @param dummyText If something trouble with the output, the dummyText, a piecs of text temporary added to document, may be the cause. If needed, instead of the default dummyText, you may use your original dummyText, which is recommended to be a reasonably long String without using numeric.
#' @param loop Whether the process applies again and again to a part once converted. Default FALSE is strongly recommended.
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
#' @return Character. Document Converted.
#' @export
#'
wareki2 <- function(document, dummyText = NA, loop = FALSE) {
  if (is.na(dummyText)) {
    dummyText <- "dummymojiretsudesuyo_saigonikeshimasu"
  }
  for (gengou in gengouList[, "gengou"]) {
    gengouFirstYear <- gengouList[gengouList$gengo == gengou, "year"] %>% as.numeric()
    detectYear <- str_c("(", gengou, ".", "|", gengou, "..", "|", gengou, "...", ")")
    while (str_detect(document, pattern = detectYear)) {
      startYear <- regexpr(detectYear, document, useBytes = F)
      endYear <- startYear + attr(startYear, "match.length") - 1
      myYear <- substr(document, startYear, endYear)
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
        document <- gsub(myYear, temp, document) %>% as.character()
      } else {
        seireki <- as.numeric(gengouFirstYear) + as.numeric(romaNumeric) - 1
        seireki %<>% as.character()
        if (loop == FALSE) {
          seireki <- str_c(dummyText, seireki, dummyText)
        }
        document <- gsub(beforeText, seireki, document) %>% as.character()
      }
    }
  }
  document <- gsub(as.character(dummyText), "", document)
  document
}
