# University of Wisconsin Colour Palette

The University of Wisconsin colour palette to easily construct plots.
There are three colour palettes based on the colour information found at
<https://brand.wisc.edu/print/colors/>. The University of Wisconsin
colour palette is available in CMYK and was converted to Hex using
<https://colordesigner.io/convert/cmyktohex>.

All colours within
[theHUB](https://emilioxavier.github.io/theHUB/reference/theHUB-package.md)
are defined using hex-codes (also known as "web") values.

- wisc.badgerRed (Badger/Cardinal Red; Primary Colour) Hex:
  ["#da004c"](https://www.color-hex.com/color/da004c)

- wisc.white (corresponding colour to wisc.badgerRed) Hex:
  ["#ffffff"](https://www.color-hex.com/color/ffffff)

- wisc.badgerRedTints: Ten tints of wisc.badgerRed including "#18453b"
  from
  [https://color-hex.com/color/da004c](https://www.color-hex.com/color/da004c)

- wisc.heatmap.20: Twenty tints of University of Wisconsin Badger Red to
  the lightest tint in wisc.badgerRed

- wisc.heatmap.100: One-hundred tints of University of Wisconsin Badger
  Red to the lightest tint in wisc.badgerRed
  <https://www.color-hex.com/color/da004c>

- wisc.secondary: The six colours of University of Wisconsin's secondary
  colour palette

  - dark red ["#a1002f"](https://www.color-hex.com/color/a1002f)

  - deep red (magenta)
    ["#8b0037"](https://www.color-hex.com/color/8b0037)

  - grey10 ["#e6e6e6"](https://www.color-hex.com/color/e6e6e6)

  - grey30 ["#b3b3b3"](https://www.color-hex.com/color/b3b3b3)

  - grey55 ["#737373"](https://www.color-hex.com/color/737373)

  - black ["#000000"](https://www.color-hex.com/color/000000)

- wisc.accent: The six colours of University of Wisconsin's accent
  colour palette

  - orange ["#ff8000"](https://www.color-hex.com/color/ff8000)

  - orange-yellow ["#ffbf00"](https://www.color-hex.com/color/ffbf00)

  - tan ["#e8dfa7"](https://www.color-hex.com/color/e8dfa7)

  - avocado green ["#97b85f"](https://www.color-hex.com/color/97b85f)

  - blue-grey ["#6b9999"](https://www.color-hex.com/color/6b9999)

  - dark teal ["#386666"](https://www.color-hex.com/color/386666)

## Usage

``` r
wisc.badgerRed

wisc.white

wisc.badgerRedTints

wisc.secondary

wisc.accent

wisc.heatmap.20

wisc.heatmap.100
```

## Format

A vector with the colours of interest.

An object of class `character` of length 1.

An object of class `character` of length 10.

An object of class `character` of length 6.

An object of class `character` of length 6.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 22
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 101
rows and 2 columns.

## Source

University of Wisconsin's Brand website:
<https://brand.wisc.edu/print/colors/>

## Functions

- `wisc.white`: corresponding colour to wisc.badgerRed

- `wisc.badgerRedTints`: Ten tints of wisc.badgerRed including "#da004c"

- `wisc.secondary`: The six secondary colours of the University of
  Wisconsin's color palette

- `wisc.accent`: The six accent colours of the University of Wisconsin's
  color palette

- `wisc.heatmap.20`: Twenty tints of Twenty tints of the University of
  Wisconsin Badger Red to the lightest tint in wisc.badgerRedTints

- `wisc.heatmap.100`: One-hundred tints of Twenty tints of the
  University of Wisconsin Badger Red to the lightest tint in
  wisc.badgerRedTints

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)
