# wareki

**EN:** <BR>R package for Handling Japanese Calender

**JA:** <BR>和暦を処理するためのRパッケージ

## Overview

**EN:** <BR>`{wareki}` helps you working with documents which include Japanese calender. The packages will be useful when you want to replace Japanese calender in character data into Western calender. It would also be useful for modifying orthographical variants in the documents which contain both Japanese and Western calender.

**JA:** <BR>`{wareki}`は文書内の和暦を西暦に変換するパッケージです。このパッケージの用途例として、和暦で与えられたパネルデータを西暦に変換することで時系列分析の下準備に用いることができるほか、和暦と西暦の混在した文章について自然言語処理による分析を行うための前処理として西暦に統一させることで、西暦・和暦の表記ゆれを吸収することができます。

## Version

**EN:** <BR>The latest version 0.1.1 was build under R 3.6.0

**JA:** <BR>最新のバージョン0.1.1はR3.6.0の下で作成されました。

## Installation

**EN:** <BR>Use package `{devtools}` to install `{wareki}` from my GitHub.

**JA:** <BR>`{devtools}`パッケージを利用して、このリポジトリから導入してください。

```R
library(devtools)
devtools::install_github("zakkii11/wareki")
```

## Imports

`{dplyr}`, `{magrittr}`, `{stringr}`

## Vignette

```R
library(wareki)
wareki("恐らく平成20年生まれの人は、令和元年にはもう11歳になっているのだろう。")
[1] "恐らく2008年生まれの人は、2019年にはもう11歳になっているのだろう。"
```

## More Information

**EN:** <BR>Detailed information in Japanese is available at the following URL

**JA:** <BR>本パッケージに関するより詳細な解説は、下記のQiita記事をご参照ください。

> https://qiita.com/zakkiiii/items/003041bed8aeb5436e4b
