
<!-- README.md is generated from README.Rmd. Please edit that file -->
wfindr
======

[![Build Status](https://travis-ci.org/idmn/wfindr.png?branch=master)](https://travis-ci.org/idmn/wfindr)

Crossword, scrabble and anagram solver
--------------------------------------

This package provides a large English words list and tools to find words by patterns. In particular, anagram finder and scrabble word finder.

### How to install

``` r
devtools::install_github("idmn/wfindr")
```

### How to use

Describe a pattern that a word should match by marking unknown letters.

-   Unknown letter is denoted by dot `"."`.
-   Dot may be followed by regex `{...}` repetition quantifier. Namely, `.{n}` means exactly n unknown letters, `.{n,}` - n or more, `.{n, m}` - from n to m.
-   Asterisk `*` denotes unknown number of unknown letters.

Then pass this pattern to a `find_word` function. Examples:

``` r
#> words starting with "aa"
find_word("aa*")
#> 4-letter words stating with "w" and ending with "d"
find_word("w..d")
#> 30 or more letter words
find_word(".{30,}")
```

You can also specify letters that you don't want to be used to fill the gaps with `ban` argument.

``` r
find_word("w..d", ban = "oe")
```

Or you can determine the list of letters to be used with `allow` argument.

``` r
find_word("w..d", allow = "oe")
```

### Scrabble solver

To find words that can be constructed from the specified set of letters, use `scrabble` function

``` r
#> words constructed from the "thing" word's letters
scrabble("thing")
```

This function is actually built on top of the `find_word` function. To give you an idea, the previous call is equivalent to this:

``` r
find_word(allow = "thing", type = "scrabble")
```

It is also possible to specify a pattern in `scrabble`, as it was in `find_word`. For, example, to get at least 4-letter words:

``` r
scrabble("thing", ".{4,}")
```

### Anagram solver

To find anagrams, use `anagram` function.

``` r
anagram("thing")
```

This function is also built on top of `find_word` and the previous call is equivalent to

``` r
find_word(allow = "thing", type = "anagram")
```
