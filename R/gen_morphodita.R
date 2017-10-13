#' Perform morphological generation
#'
#'
#' @param data A word for which its forms should be generated (must be lemma)
#' @param convert_tagset Apply specified tag set converter
#' (pdt_to_conll2009 / strip_lemma_comment / strip_lemma_id)
#' @param ... Other parameters accepted by API
#' @seealso http://lindat.mff.cuni.cz/services/morphodita/api-reference.php
gen_morphodita <- function(data,
                           convert_tagset = "strip_lemma_id", ...){

    tagset <- convert_tagset
    out <- httr::GET(
        url = "http://lindat.mff.cuni.cz/services/morphodita/api/generate",
        query = list(data = data,
                     output = "json",
                     convert_tagset = convert_tagset, ...))

    structure(
        list(
            url = out$url,
            model = jsonlite::fromJSON(
                stringr::str_conv(out$content, "UTF-8"))$model,
            tagset = tagset,
            lang = substr(jsonlite::fromJSON(
                stringr::str_conv(out$content, "UTF-8"))$model, 1, 2),
            output = do.call(rbind, lapply(jsonlite::fromJSON(
                stringr::str_conv(out$content, "UTF-8"))$result,
                function(x) x[, !names(x) %in% c("space", "spaces")]))
        ),
        class = "morphodita_api"
    )
}
