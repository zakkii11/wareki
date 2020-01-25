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
#' @importFrom utils read.csv
#' @importFrom utils data
#'
#' @return document converted
#' @export
#'
wareki <- function(document, dummyText = NA, loop = FALSE){
if(is.na(dummyText)) {
    dummyText = "dummymojiretsudesuyo_saigonikeshimasu"
  }
  for(gengou in gengouList[, "gengou"]){
    gengouFirstYear <- gengouList[gengouList$gengou==gengou, "year"]
    detectYear <- str_c("(",gengou, ".\u5e74|", gengou, "..\u5e74|",gengou, "...\u5e74)")
    while(str_detect(document, pattern=detectYear)){
      startYear <- regexpr(detectYear, document, useBytes = F)
      endYear <- startYear + attr(startYear,"match.length") -1
      myYear <- substr(document, startYear, endYear)
      if(str_sub(myYear, start = 3, end = 4) == gengou){
        startYear <- regexpr(detectYear, document) + 2
        myYear <- substr(document, startYear, endYear)
      }
      if(str_sub(myYear, start = 4, end = 5) == gengou){
        replacementTx <- substr(document, startYear+2, endYear)
        replacementTx <- str_c(gengou, dummyText, replacementTx)
        document <- gsub(myYear, replacementTx, document)
      } else {
        numericDetect <- str_c("(", gengou, "|\u5e74)")
        waYear <- gsub(numericDetect, "", myYear)
        if(waYear == "\u5143"){
          waYear <- as.numeric(1)
          }
        romaNumeric <- suppressWarnings(as.numeric(waYear))
        if(is.na(romaNumeric)){
          excluded <- as.character(waYear)
          temp <- str_c(gengou, dummyText, excluded, "\u5e74")
          document <- gsub(myYear, temp, document) %>% as.character()
        }else{
          seireki <- gengouFirstYear + romaNumeric - 1
          if(loop == FALSE){
            convertedYear <- str_c(dummyText, seireki, "\u5e74")
          } else {
            convertedYear <- str_c(seireki, "\u5e74")
          }
          document <- gsub(myYear, convertedYear, document) %>% as.character()
        }
      }
    }
  }
  document <- gsub(as.character(dummyText), "", document)
  document
}
