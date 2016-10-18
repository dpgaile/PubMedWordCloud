#' Make a Word Cloud using the pubmed abstracts arrived at from
#' a pubmed query
#'
#' @details This method uses functions from the wordcloud,
#'
#' @importFrom RISmed EUtilsSummary
#'
#' @importFrom RISmed EUtilsGet
#'
#' @importFrom RISmed ArticleTitle
#'
#' @importFrom RISmed AbstractText
#'
#' @importFrom tm VectorSource
#'
#' @importFrom tm tm_map
#'
#' @importFrom tm VCorpus
#'
#' @importFrom tm stopwords
#'
#' @importFrom tm removePunctuation
#'
#' @importFrom tm removeNumbers
#'
#' @importFrom tm removeWords
#'
#' @importFrom tm stripWhitespace
#'
#' @importFrom tm stemDocument
#'
#' @importFrom tm PlainTextDocument
#'
#' @importFrom RColorBrewer brewer.pal
#'
#' @importFrom wordcloud wordcloud
#'
#' @param SearchTerm character (1) pubmed search term
#'
#' @param min.freq integer(1) minimum word frequency for wordcloud
#'
#' @param max.words integer(1) maximum number of words for wordcloud
#'
#' @return generates a wordcloud figure
#'
#' @export
PubMedWordCloud <- function(SearchTerm, min.freq=3, max.words=250){

    res <- EUtilsSummary(SearchTerm, type="esearch", db="pubmed", datetype='pdat',
                         mindate=1900, maxdate=2100, retmax=10000)

    records=  EUtilsGet(res)

    pubmed_data <- data.frame('Title'=ArticleTitle(records),'Abstract'=AbstractText(records))
    head(pubmed_data,1)

    pubmed_data$Abstract <- as.character(pubmed_data$Abstract)
    pubmed_data$Abstract <- gsub(",", " ", pubmed_data$Abstract, fixed = TRUE)

    paperCorp <- tm_map(VCorpus(VectorSource(pubmed_data$Abstract)), removePunctuation)
    paperCorp <- tm_map(paperCorp, removeNumbers)
    # added tolower
    paperCorp <- tm_map(paperCorp, tolower)
    paperCorp <- tm_map(paperCorp, removeWords, stopwords("english"))
    # moved stripWhitespace
    paperCorp <- tm_map(paperCorp, stripWhitespace)

    paperCorp <- tm_map(paperCorp, stemDocument)

    paperCorpPTD <- tm_map(paperCorp, PlainTextDocument)

    # wordcloud(paperCorpPTD, max.words = 200, min.freq=5, random.order = FALSE)


    wordcloud(paperCorpPTD, scale=c(2,0.5), max.words=max.words, min.freq=min.freq,
                  random.order=FALSE, rot.per=0.35,
              use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))



}
