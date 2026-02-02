# Normalize string values

This function performs a series of text cleaning and normalization on
text data. For example, it is used in
[`prepare_pharm_for_validation()`](https://gemini-medicine.github.io/Rgemini/reference/prepare_pharm_for_validation.md)
to clean up RxNorm outputs for validation. The operations include:

- Converts text to ASCII encoding, such that special characters can be
  intepreted properly by R.

- Converts all characters to lowercase.

- Trims leading and trailing whitespace.

- Removes leading and trailing periods.

- Optional: Converts to the singular form through lemmatization.

## Usage

``` r
normalize_text(x, lemma = FALSE)
```

## Arguments

- x:

  (`character`)  
  Text input to be normalized. Multiple values can be provided as a
  character vector.

- lemma:

  (`logical`)  
  If set to `TRUE`, singularizes x via
  [`textstem::lemmatize_words()`](https://rdrr.io/pkg/textstem/man/lemmatize_words.html).
  For use in the RxNorm workflow: Note that lemmatization is an NLP
  technique that may not always convert drug names or classification
  terms into their respective singular form, but this processing is
  still helpful to a certain extent to help minimize variations due to
  plural forms.

## Examples

``` r
normalize_text(" Ámoxícíllíns. ")
#> [1] "amoxicillins"
```
