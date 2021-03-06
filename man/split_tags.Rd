% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tag_morphodita.R
\name{split_tags}
\alias{split_tags}
\title{Split the tags into columns}
\usage{
split_tags(data)
}
\arguments{
\item{data}{dataset which is returned by the tag_morphodita function}
}
\description{
This function takes the output of the tag_morphodita function and split the
returned tag so that every information about the morphology of the original
word such as case, number, gender, tense etc. is in separate columns.
}
\seealso{
https://ufal.mff.cuni.cz/pdt/Morphology_and_Tagging/Doc/hmptagqr.html

https://ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html
}
