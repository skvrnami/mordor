context("Tag API Test")

context("Test tagging")
test_that("API tag output", {
    expect_equal(nrow(tag_morphodita("A je to")$output),
                 length(unlist(strsplit("A je to", split = " "))))
    expect_equal(ncol(tag_morphodita("A je to")$output),
                 3)
    expect_equal(colnames(tag_morphodita("A je to")$output),
                 c("token", "lemma", "tag"))
})


context("Test generation")
get_salutation <- function(data){
    gen_morphodita(data) %>%
        split_tags() %>%
        `[[`("output") %>%
        subset(NUMBER == "S" & CASE == 5,
               select = "form", drop = TRUE) %>%
        tail(1)
}

test_that("K. H. Mácha could use MorphoDiTas API", {
          expect_equal(get_salutation("Karel"), "Karle")
          expect_equal(get_salutation("Hynek"), "Hynku")
          expect_equal(get_salutation("Jarmila"), "Jarmilo")
          })
