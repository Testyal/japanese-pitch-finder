# Japanese Pitch Finder

## Installing
```bash
stack build
```

## Usage
```bash
stack exec -- japanese-pitch-finder [-o OUTPUT_FILE] QUERY
```

`-o OUTPUT_FILE` Appends the code `\dictentry{QUERY}{}{OUTPUT}{}{}` to `OUTPUT_FILE`, where `OUTPUT` is the reading of `QUERY` with pitch accent marks added. See `template.tex` for an example of how to define the `\dictentry` command.
