# MorphoDiTaR - R client for MorphoDiTa API

[![Travis-CI Build Status](https://travis-ci.org/skvrnami/morphoditar.svg?branch=master)](https://travis-ci.org/skvrnami/morphoditar)

This package serves as a R client for MorphoDiTa API, 
Morphological Dictionary and Tagger - an open-source 
tool for morphological analysis of natural language texts, 
developed by Institute of Formal and Applied Linguistics, 
Faculty of Mathematics and Physics, Charles University in Prague, 
Czech Republic.

For more information see:
[http://lindat.mff.cuni.cz/services/morphodita/info.php](http://lindat.mff.cuni.cz/services/morphodita/info.php)

## Install

```
devtools::install_github("skvrnami/morphoditar")
```

## Usage

A basic overview of the package capabilities provides a toy example of Czech (more-or-less) DIY bands which is available [here](http://skvrnami.github.io/morphoditar/lyrics-usecase.html).

### To DO:

- [x] Load tag codes from files (remove hardcoding)
- [x] Properly recode Tag Var value of dash
- [ ] Check Slovak model
- [ ] Handle NA values when tagging (sending to API)
- [ ] Compatibility with quanteda


