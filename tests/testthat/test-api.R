context("Tag API Test")

context("Test tagging")
system("docker pull skvrnami/mordor-docker:latest")
system("docker run -p 4000:80 skvrnami/mordor-docker:latest")
test_that("API tag output (docker)", {
    expect_equal(nrow(tag_morphodita("A je to", source = "docker")$output),
                 length(unlist(strsplit("A je to", split = " "))))
    expect_equal(ncol(tag_morphodita("A je to", source = "docker")$output),
                 3)
    expect_equal(colnames(tag_morphodita("A je to", source = "docker")$output),
                 c("token", "lemma", "tag"))
})

test_that("API tag output (lindat)", {
    expect_equal(nrow(tag_morphodita("A je to", source = "lindat")$output),
                 length(unlist(strsplit("A je to", split = " "))))
    expect_equal(ncol(tag_morphodita("A je to", source = "lindat")$output),
                 3)
    expect_equal(colnames(tag_morphodita("A je to", source = "lindat")$output),
                 c("token", "lemma", "tag"))
})

test_that("API tag output returns error when source is wrong", {
    expect_error(tag_morphodita("A je to", source = "špatný"))
})

a <- tag_morphodita("A je to", source = "docker")
b <- tag_morphodita("A je to", source = "lindat", tagset = "pdt_to_conll2009")
test_that("Splitting tags returns data.frame with splitted POS tag", {
    expect_equal(ncol(split_tags(a)$output), 17)
    expect_equal(ncol(split_tags(b)$output), 17)
})

test_that("Recoding tags returns data.frame", {
    expect_equal(ncol(split_tags(a)$output), ncol(recode_tags(split_tags(a))$output))
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
