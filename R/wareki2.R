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
#'
#' @return document
#' @export
#'
wareki2 <- function(textdata, dummyText = NA, loop = FALSE){
  if(is.na(dummyText)){
    dummyText = "dummymojiretsudesuyo_saigonikeshimasu"
  }
  for(gengou in gengouList[, "元号"]){
    gengouFirstYear <- gengouList[gengouList$元号==gengou, "元年西暦"] %>% as.numeric()
    detectYear <- str_c("(",gengou, ".","|", gengou, "..","|",gengou, "...",")")
    while(str_detect(textdata, pattern=detectYear)){
      startYear <- regexpr(detectYear, textdata)
      endYear <- startYear + attr(startYear,"match.length") -1
      myYear <- substr(textdata, startYear, endYear)
      numericDetect <- str_c("(", gengou, ")")
      waYear <- gsub(numericDetect, "", myYear)
      waYear1 <- waYear
      waYearX1 <- waYear
      if(waYear1 == "元"){
        waYear1 <- as.numeric(1)
        }
      romaNumeric <- suppressWarnings(as.numeric(waYear1))
      beforeText <- str_c(gengou, waYearX1)
      if(is.na(romaNumeric)){
        waYear2 <- str_sub(waYear, end = -2)
        waYearX2 <- waYear2
        if(waYear2 == "元"){
          waYear2 <- as.numeric(1)
        }
        romaNumeric <- suppressWarnings(as.numeric(waYear2))
        beforeText <- str_c(gengou, waYearX2)
      }
      if(is.na(romaNumeric)){
        waYear3 <- str_sub(waYear, end = -3)
        waYearX3 <- waYear3
        if(waYear3 == "元"){
          waYear3 <- as.numeric(1)
        }
        romaNumeric <- suppressWarnings(as.numeric(waYear3))
        beforeText <- str_c(gengou, waYearX3)
      }
      if(is.na(romaNumeric)){
        excluded <- as.character(waYear)
        temp <- str_c(substr(gengou,1,1), dummyText, substr(gengou,2,2), excluded)
        textdata <- gsub(myYear, temp, textdata) %>% as.character()
      } else {
        seireki <- as.numeric(gengouFirstYear) + as.numeric(romaNumeric) - 1
        seireki %<>% as.character()
        if(loop == FALSE){
          seireki <- str_c(dummyText, seireki, dummyText)
        }
        textdata <- gsub(beforeText, seireki, textdata) %>% as.character()
        }
    }
  }
  textdata <- gsub(as.character(dummyText), "", textdata)
  textdata
}
