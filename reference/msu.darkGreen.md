# MSU Colour Palette

The MSU colour palette to easily construct plots. There are three colour
palettes based on the colour information found at
<https://brand.msu.edu/design-visual/index.html#color>. All colours
within
[theHUB](https://emilioxavier.github.io/theHUB/reference/theHUB-package.md)
are defined using hex-codes (also known as "web") values.

- msu.darkGreen (Primary Colour) Hex:
  ["#18453b"](https://www.color-hex.com/color/18453b)

- msu.darkGreenTints: Ten tints of msu.darkGreen including "#18453b"
  from
  [https://color-hex.com/color/18453b](https://www.color-hex.com/color/18453b)

- msu.heatmap.20: Twenty tints of MSU Dark Green to the lightest tint in
  msu.darkGreenTints

- msu.heatmap.100: One-hundred tints of MSU Dark Green to the lightest
  tint in msu.darkGreenTints <https://www.color-hex.com/color/18453b>

- msu.palette: The thirteen colours of MSU's colour palette

  - MSU green ["#18453b"](https://www.color-hex.com/color/18453b)

  - kelly green ["#0db14b"](https://www.color-hex.com/color/0db14b)

  - grey ["#97a2a2"](https://www.color-hex.com/color/97a2a2)

  - orange ["#f08521"](https://www.color-hex.com/color/f08521)

  - teal ["#008183"](https://www.color-hex.com/color/008183)

  - blue-grey ["#909ab7"](https://www.color-hex.com/color/909ab7)

  - dark grey ["#535054"](https://www.color-hex.com/color/535054)

  - yellow-green ["#d1de3f"](https://www.color-hex.com/color/d1de3f)

  - cream ["#e8d9b5"](https://www.color-hex.com/color/e8d9b5)

  - texas-brown ["#c89a58"](https://www.color-hex.com/color/c89a58)

  - split pea soup green
    ["#94ae4a"](https://www.color-hex.com/color/94ae4a)

  - eggplant ["#6e005f"](https://www.color-hex.com/color/6e005f)

  - sienna ["#cb5a28"](https://www.color-hex.com/color/cb5a28)

## Usage

``` r
msu.darkGreen

msu.darkGreenTints

msu.palette

msu.heatmap.20

msu.heatmap.100
```

## Format

A vector with the colours of interest.

An object of class `character` of length 10.

An object of class `character` of length 13.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 22
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 101
rows and 2 columns.

## Source

MSU's Brand website:
<https://brand.msu.edu/design-visual/index.html#color>
<https://cal.msu.edu/about/the-marketing-communications-office/colors/>

## Functions

- `msu.darkGreenTints`: Ten tints of msu.darkGreen including "#18453b"

- `msu.palette`: The thirteen colours of MSU's color palette

- `msu.heatmap.20`: Twenty tints of MSU Dark Green to the lightest tint
  in msu.darkGreenTints

- `msu.heatmap.100`: One-hundred tints of MSU Dark Green to the lightest
  tint in msu.darkGreenTints

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)
