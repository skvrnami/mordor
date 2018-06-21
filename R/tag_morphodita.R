#' Strip lemma comment
#' @param out Output of tagging
#' @export
strip_lemma_comment <- function(out){
    out$output$lemma <- stringr::str_remove(out$output$lemma, "-.*|_.*")
    out
}


#' Perform morphological analysis of supplied text
#'
#' Function that send to and get requests from MorphoDiTa API call.
#' @param data Text to be analysed
#' @param tagset Apply specified tag set converter
#' (pdt_to_conll2009 / strip_lemma_comment / strip_lemma_id)
#' @param source use "docker" or "lindat" for API
#' @param strip_comment TRUE if you want to strip the lemma comment
#' @param ... Other parameters accepted by the API (see the API reference)
#' @seealso http://lindat.mff.cuni.cz/services/morphodita/api-reference.php
#' @export
tag_morphodita <- function(data,
                           tagset = "pdt_to_conll2009",
                           source = "docker",
                           strip_comment = TRUE,
                           ...){
    if(source == "docker"){
        out <- httr::GET(url = "http://localhost:4000/",
                         query = list(text = data))

        result <- structure(
            list(
                url = out$url,
                model = "czech-morfflex-pdt-161115",
                tagset = "none",
                lang = "cz",
                output = parse_xml(out$content)
            ),
            class = "morphodita_api"
        )

        if(strip_comment){
            strip_lemma_comment(result)
        }else{
            result
        }
    }else if(source == "lindat"){
        out <- httr::GET(
            url = "http://lindat.mff.cuni.cz/services/morphodita/api/tag",
            query = list(data = data,
                         output = "json",
                         convert_tagset = tagset))

        if (httr::http_type(out) != "application/json") {
            stop("API did not return json", call. = FALSE)
        }

        result <- structure(
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
        if(strip_comment){
            strip_lemma_comment(result)
        }else{
            result
        }
    }else{
        stop("Unknown source", call. = FALSE)
    }
}


#' @export
print.morphodita_api <- function(x, ...){
    cat("MorphoDiTa API call:\n\n")
    cat("Tagset:", x$tagset, "\n")
    cat("Model:", x$model, "\n")
    cat("Language:", x$lang, "\n\nOutput:\n")
    print(x$output)
    invisible(x)
}

#' Parse XML output from mordor-docker
#'
#' @param x output of tagger
parse_xml <- function(x){
    out <- suppressWarnings(xml2::read_xml(stringr::str_conv(x, "UTF-8"),
                              as_html = TRUE))
    data.frame(
        token = out %>%
            rvest::html_nodes("token") %>%
            rvest::html_text(),
        lemma = out %>%
            rvest::html_nodes("token") %>%
            rvest::html_attr("lemma"),
        tag = out %>%
            rvest::html_nodes("token") %>%
            rvest::html_attr("tag"),
        stringsAsFactors = FALSE)

}

#' Split the tags into columns
#'
#' This function takes the output of the tag_morphodita function and split the
#' returned tag so that every information about the morphology of the original
#' word such as case, number, gender, tense etc. is in separate columns.
#' @param data dataset which is returned by the tag_morphodita function
#' @seealso https://ufal.mff.cuni.cz/pdt/Morphology_and_Tagging/Doc/hmptagqr.html
#' @seealso https://ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html
#' @export
split_tags <- function(data){
    out <- data$output
    if (data$tagset == "pdt_to_conll2009"){
        out$POS <- stringr::str_match(
            out$tag, "POS=([ACDIJNPVRTXZ]{1})")[, 2]

        out$SUBPOS <- stringr::str_match(
            out$tag, "SubPOS=([A-Za-z0-9.!#,:;?@^}~]{1})")[, 2]

        out$GENDER <- stringr::str_match(
            out$tag, "Gen=([FHIMNQTXYZ]{1})")[, 2]

        out$NUMBER <- stringr::str_match(
            out$tag, "Num=([A-Za-z0-9.!#,:;?@^}~]{1})")[, 2]

        out$CASE <- stringr::str_match(
            out$tag, "Cas=([1-7X]{1})")[, 2]

        out$POSSGENDER <- NA

        out$POSSNUMBER <- stringr::str_match(out$tag, "PNu=([PS]{1})")[, 2]

        out$PERSON <- stringr::str_match(
            out$tag, "Per=([A-Za-z0-9.!#,:;?@^}~]{1})")[, 2]

        out$TENSE <- stringr::str_match(
            out$tag, "Ten=([A-Za-z0-9.!#,:;?@^}~]{1})")[, 2]

        out$GRADE <- stringr::str_match(
            out$tag, "Gra=([A-Za-z0-9.!#,:;?@^}~]{1})")[, 2]

        out$NEGATION <- stringr::str_match(
            out$tag, "Neg=([A-Za-z0-9.!#,:;?@^}~]{1})")[, 2]

        out$VOICE <- stringr::str_match(
            out$tag, "Voi=([AP]{1})")[, 2]

        out$RESERVE1 <- NA
        out$RESERVE2 <- NA

        out$VAR <- stringr::str_match(
            out$tag, "Var=([1-9]{1})")[, 2]
        out$VAR[is.na(out$VAR)] <- "-"

        out$tag <- NULL

    }else if (data$tagset %in% c("strip_lemma_id", "strip_lemma_comment", "none") &
             data$lang == "cz"){
        out$tag <- unlist(lapply(strsplit(out$tag, split = ""),
                                 function(x) paste0(x, collapse = "|")))

        # separate the tag into columns
        out <- tidyr::separate(out, tag,
                               c("POS", "SUBPOS", "GENDER", "NUMBER", "CASE",
                                 "POSSGENDER", "POSSNUMBER", "PERSON", "TENSE",
                                 "GRADE", "NEGATION", "VOICE", "RESERVE1",
                                 "RESERVE2", "VAR"), sep = "\\|")
        # replace the dash with NA
        out[out == "-"] <- NA
        out$VAR[is.na(out$VAR)] <- "-"
    }else{
        message("Tags cannot be splitted because it consist of ",
                "Penn Treebank Project POS tags.")
    }
    data$output <- out
    data
}


#' Turn the columns from tags into factor and assign them meaningful labels
#'
#' @param data data.frame in which the lemmas with tags are stored
#' @seealso https://ufal.mff.cuni.cz/pdt/Morphology_and_Tagging/Doc/hmptagqr.html
#' @import magrittr
#' @export
recode_tags <- function(data){
    out <- data$output
    if (data$lang == "cz" | data$tagset == "pdt_to_conll2009"){
        data("TAGS")
        TAGS <- TAGS
        out$POS %<>% factor(levels = TAGS$POS$Value,
                            labels = TAGS$POS$Description)
        out$SUBPOS %<>%
            factor(
                levels = TAGS$SUBPOS$Value,
                labels = TAGS$SUBPOS$Description)

        out$GENDER %<>% factor(levels = TAGS$GENDER$Value,
                               labels = TAGS$GENDER$Description)

        out$NUMBER %<>% factor(levels = TAGS$NUMBER$Value,
                               labels = TAGS$NUMBER$Description)

        out$CASE %<>% factor(levels = TAGS$CASE$Value,
                             labels = TAGS$CASE$Description)

        out$POSSGENDER %<>% factor(levels = TAGS$POSSGENDER$Value,
                                  labels = TAGS$POSSGENDER$Description)

        out$POSSNUMBER %<>% factor(levels = TAGS$POSSNUMBER$Value,
                                  labels = TAGS$POSSNUMBER$Description)

        out$PERSON %<>% factor(levels = TAGS$PERSON$Value,
                               labels = TAGS$PERSON$Description)

        out$TENSE %<>% factor(levels = TAGS$TENSE$Value,
                              labels = TAGS$TENSE$Description)

        out$GRADE %<>% factor(levels = TAGS$GRADE$Value,
                              labels = TAGS$GRADE$Description)

        out$NEGATION %<>% factor(levels = TAGS$NEGATION$Value,
                                 labels = TAGS$NEGATION$Description)

        out$VOICE %<>% factor(levels = TAGS$VOICE$Value,
                              labels = TAGS$VOICE$Description)

        out$VAR[is.na(out$VAR)] <- "-"
        out$VAR %<>% factor(
            levels = TAGS$VAR$Value,
            labels = TAGS$VAR$Description
        )
    }else{
        data("TAGS_EN")
        TAGS_EN <- TAGS_EN
        out$tag %<>% factor(levels = TAGS_EN$Value,
                            labels = TAGS_EN$Description)
    }

    data$output <- out
    data
}
