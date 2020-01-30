# Japanese Pitch Finder
Japanese Pitch Finder is a utility to search for Japanese words and display their reading with pitch accent marks added. Optionally, it also has the capability to append a provided LaTeX command to a given document.

## Installing
You should have [Stack](https://docs.haskellstack.org/en/stable/README/) installed. Clone this repository into your favorite directory and run
```bash
stack build
```

## Usage
```bash
stack exec -- japanese-pitch-finder [-o OUTPUT_FILE] QUERY
```

`-o OUTPUT_FILE` \
Appends the code `\dictentry{QUERY}{}{OUTPUT}{}{}` to `OUTPUT_FILE`, where `OUTPUT` is the reading of `QUERY` with pitch accent marks added. See `template.tex` for an example of how to define the `\dictentry` command.

## Attributions 
This work uses a modified version (found in `data/NewWaDokuDa.tsv`) of the [WaDokuJT data](https://github.com/WaDoku/WaDokuJT-Data) by Ulrich Apel & Wadoku e.V, used under [CC BY-SA 3.0](http://creativecommons.org/licenses/by-sa/3.0/). 

# Notes
Japanese Pitch Finder works by using the [cassava](http://hackage.haskell.org/package/cassava) package to load the CSV file `data/NewWaDokuDa.tsv` into a vector of DictionaryEntries. The code for this step of the process is in `src/Dictionary/Csv.hs`. A DictionaryEntry is a type (defined in `src/Dictionary/Types.hs`) defined using record syntax to have three entries: *japanese* is the word itself, *reading* is its reading, and *accent* is a number indicating the accented mora in a word. The [Downstep](https://en.wikipedia.org/wiki/Japanese_pitch_accent#Downstep) interpretation is a nice way to understand what is meant by this. Some entries in the CSV have no accent number available, so the type of *accent* is `Maybe Int` to reflect this.

This vector of DictionaryEntries (the dictionary) is filtered by the query entered by the user, using the function filterEntries in `src/Dictionary/Search.hs`, to produce a new filtered dictionary. This filtered dictionary is printed to the screen using the functions in `src/Dictionary/Output.hs`. If the `-o` option was used, the functions in `src/Latex.hs` are used to write a string to a given LaTeX document.

## Performance
The results from benchmarking the process of converting NewWaDokuDa.tsv to a Dictionary are as follows:
```
benchmarking Dictionary.Csv.convertCsvToDictionary
time                 1.949 s    (1.530 s .. 2.548 s)
                     0.990 R²   (0.970 R² .. 1.000 R²)
mean                 1.781 s    (1.673 s .. 1.890 s)
std dev              135.1 ms   (65.32 ms .. 175.1 ms)
variance introduced by outliers: 21% (moderately inflated)
```

Benchmarks were also ran on the filterEntries function using a small set of 12 sample entries (which the author got from the wall of Japanese words in front of them). This function relies on the `Data.Vector.filter` function provided in the package [vector](http://hackage.haskell.org/package/vector). This function runs in O(n) time (unsurprisingly), and acts on a dictionary of length 615915. All of benchmarks came out to times between around 15 ms and 19 ms. For example, one sample entry produced the following benchmark result:
```
benchmarking Dictionary.Search.filterEntries for the entry 勉強する
time                 17.06 ms   (16.57 ms .. 17.45 ms)
                     0.997 R²   (0.994 R² .. 0.998 R²)
mean                 17.28 ms   (17.03 ms .. 17.48 ms)
std dev              528.8 μs   (453.4 μs .. 617.2 μs)
```
This is a full two orders of magnitude faster than the convertCsvToDictionary function. 

These results show the bottleneck in the performance of this program is in converting the CSV data to a Dictionary. Perhaps for now, it would be best to allow the user to look for multiple entries during the program using a simple text-based UI (like "Enter query: ") so that the CSV is only loaded once, regardless of how many entries the user would like to search for.
