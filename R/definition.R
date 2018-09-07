definition_reply <- function(id, uri, workspace, document, position) {

    logger$info("<definition> document: ", document, ", line:", position$line, ", character:", position$character)
   
    Response$new(
        id,
        result = list(
            uri = "/Users/pformont/Work/packages/rAWS/R/credentials.R",
            range = list(
                start = list(line = 37, character = 0),
                end = list(line = 145, character = 0)
            )
        )
    )
 

}