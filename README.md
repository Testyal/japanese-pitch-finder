# Japanese Pitch Finder
Japanese Pitch Finder is a utility to search for Japanese words and display their reading with pitch accent marks added. Optionally, it also has the capability to append a LaTeX command to a given document.

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
