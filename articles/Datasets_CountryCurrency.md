# Dataset: Country and Currency Dataset

## Country and Currency Dataset

The Country and Currency Dataset was constructed to help the [Predictive
Analytics
Group](https://thehopefulbox.com/articles/web_only/PAG_About.html) (PAG)
explore the impact of currency exchange rates on applicants. The [Office
of Admissions](https://admissions.msu.edu/) at [Michigan State
University](https://msu.edu) (MSU) uses
[slate](https://technolutions.com/solutions/slate) for applicants
management. Unfortunately, the country names within slate do not match
the [ISO 3166](https://en.wikipedia.org/wiki/ISO_3166) standard names.
(*NB*: This is a standard and common problem across a multitude of
organizations and applications.) To remedy this inconsistency, the
following process was performed; code provided below.

- Determine the unique countries within the slate data.
- Extract the ISO 3166 information from `ISOcodes::ISO_3166_1`.
- Extract the currency name and code from `priceR::currencies()`.
  - Extract the country two-letter code from the currency code.
- Merge the ISO 3166 information and the currency information using the
  two-letter code.
- Merge the `ISOcodes::ISO_3166_1` countries with the slate country
  using the **Name** column.
- Export merged information to an Excel workbook.
- Within Excel, reorder data based on the **Name** column.
  - Instances where the slate name does not match the ISO 3166 name are
    easily recognizable because there are no **Alpha_2**, **Alpha_3**,
    **Numeric**, and **Name** entries, move the two slate cells
    (**Name.SLATE** and **SLATE.tf**) to the appropriate ISO 3166 row.
  - Wikipedia was used to determine currency used in the countries
    without a noted currency. See [Data Sources](#DataSources).
- Import the updated Excel workbook as a tibble.

## Construction of the Country and Currency Dataset

The following are the commands used to construct the Country and
Currency Dataset broken into individual steps. The editing within the
Excel workbook is not included.

``` r

library(theHUB)
library(ISOcodes)
library(priceR)
```

### Extract Country Names Information

``` r

country.iso <- as_tibble(ISOcodes::ISO_3166_1) |>
  mutate(Name.ISO=Name)
```

### Extract Country and Currency Information

``` r

country.currencies <- priceR::currencies() |>
  as_tibble() |>
  separate(col="code", into=c("Alpha_2", "currency.DROP"), remove=FALSE, sep="[[:alpha:]]{1}$") |>
  select(-currency.DROP) |>
  rename("currency"="description",
         "currency.code"="code")
```

### Merge All the Information

``` r

country.DATA <- full_join(x=country.iso, y=country.currencies, by="Alpha_2") |>
  full_join(y=SLATE.country.names, by="Name")
```

### Write to Excel workbook

``` r

WriteXLS::WriteXLS(x="country.DATA",
                   ExcelFileName="CountryISO-slate_matches.xlsx",
                   FreezeRow=1)
```

## The Dataset

The 257 International Organization for Standardization (ISO) recognized
countries and the currencies they use.

| Alpha 2 | Alpha 3 | Numeric | Name | Official Name | Name (ISO) | Currency | Currency Code |
|:---|:---|:---|:---|:---|:---|:---|:---|
| AF | AFG | 004 | Afghanistan | Islamic Republic of Afghanistan | Afghanistan | Afghan Afghani | AFN |
| AX | ALA | 248 | Åland Islands |  | Åland Islands | Euro | EUR |
| AL | ALB | 008 | Albania | Republic of Albania | Albania | Albanian Lek | ALL |
| DZ | DZA | 012 | Algeria | People’s Democratic Republic of Algeria | Algeria | Algerian Dinar | DZD |
| AS | ASM | 016 | American Samoa |  | American Samoa | United States Dollar | USD |
| AD | AND | 020 | Andorra | Principality of Andorra | Andorra | Euro | EUR |
| AO | AGO | 024 | Angola | Republic of Angola | Angola | Angolan Kwanza | AOA |
| AI | AIA | 660 | Anguilla |  | Anguilla | East Caribbean Dollar | XCD |
| AQ | ATA | 010 | Antarctica |  | Antarctica |  |  |
| AG | ATG | 028 | Antigua and Barbuda |  | Antigua and Barbuda | East Caribbean Dollar | XCD |
| AG | ATG | 029 | Antigua and Barbuda |  | Antigua and Barbuda | East Caribbean Dollar | XCD |
| AR | ARG | 032 | Argentina | Argentine Republic | Argentina | Argentine Peso | ARS |
| AM | ARM | 051 | Armenia | Republic of Armenia | Armenia | Armenian Dram | AMD |
| AW | ABW | 533 | Aruba |  | Aruba | Aruban Florin | AWG |
| AU | AUS | 036 | Australia |  | Australia | Australian Dollar | AUD |
| AT | AUT | 040 | Austria | Republic of Austria | Austria | Euro | EUR |
| AZ | AZE | 031 | Azerbaijan | Republic of Azerbaijan | Azerbaijan | Azerbaijani Manat | AZN |
| BS | BHS | 044 | Bahamas | Commonwealth of the Bahamas | Bahamas | Bahamian Dollar | BSD |
| BH | BHR | 048 | Bahrain | Kingdom of Bahrain | Bahrain | Bahraini Dinar | BHD |
| BD | BGD | 050 | Bangladesh | People’s Republic of Bangladesh | Bangladesh | Bangladeshi Taka | BDT |
| BB | BRB | 052 | Barbados |  | Barbados | Barbadian Dollar | BBD |
| BY | BLR | 112 | Belarus | Republic of Belarus | Belarus | Belarusian Ruble | BYN |
| BE | BEL | 056 | Belgium | Kingdom of Belgium | Belgium | Euro | EUR |
| BZ | BLZ | 084 | Belize |  | Belize | Belize Dollar | BZD |
| BJ | BEN | 204 | Benin | Republic of Benin | Benin | CFA Franc BCEAO | XOF |
| BM | BMU | 060 | Bermuda |  | Bermuda | Bermudan Dollar | BMD |
| BT | BTN | 064 | Bhutan | Kingdom of Bhutan | Bhutan | Bitcoin | BTC |
| BT | BTN | 064 | Bhutan | Kingdom of Bhutan | Bhutan | Bhutanese Ngultrum | BTN |
| BO | BOL | 068 | Bolivia, Plurinational State of | Plurinational State of Bolivia | Bolivia, Plurinational State of | Bolivian Boliviano | BOB |
| BQ | BES | 535 | Bonaire, Sint Eustatius and Saba | Bonaire, Sint Eustatius and Saba | Bonaire, Sint Eustatius and Saba | United States Dollar | USD |
| BQ | BES | 536 | Bonaire, Sint Eustatius and Saba | Bonaire, Sint Eustatius and Saba | Bonaire, Sint Eustatius and Saba | United States Dollar | USD |
| BQ | BES | 537 | Bonaire, Sint Eustatius and Saba | Bonaire, Sint Eustatius and Saba | Bonaire, Sint Eustatius and Saba | United States Dollar | USD |
| BA | BIH | 070 | Bosnia and Herzegovina | Republic of Bosnia and Herzegovina | Bosnia and Herzegovina | Bosnia-Herzegovina Convertible Mark | BAM |
| BW | BWA | 072 | Botswana | Republic of Botswana | Botswana | Botswanan Pula | BWP |
| BV | BVT | 074 | Bouvet Island |  | Bouvet Island | Norwegian Krone | NOK |
| BR | BRA | 076 | Brazil | Federative Republic of Brazil | Brazil | Brazilian Real | BRL |
| IO | IOT | 086 | British Indian Ocean Territory |  | British Indian Ocean Territory | United States Dollar | USD |
| BN | BRN | 096 | Brunei Darussalam |  | Brunei Darussalam | Brunei Dollar | BND |
| BG | BGR | 100 | Bulgaria | Republic of Bulgaria | Bulgaria | Bulgarian Lev | BGN |
| BF | BFA | 854 | Burkina Faso |  | Burkina Faso | CFA Franc BCEAO | XOF |
| BI | BDI | 108 | Burundi | Republic of Burundi | Burundi | Burundian Franc | BIF |
| CV | CPV | 132 | Cabo Verde | Republic of Cabo Verde | Cabo Verde | Cape Verdean Escudo | CVE |
| KH | KHM | 116 | Cambodia | Kingdom of Cambodia | Cambodia | Cambodian Riel | KHR |
| CM | CMR | 120 | Cameroon | Republic of Cameroon | Cameroon | CFA Franc BEAC | XAF |
| CA | CAN | 124 | Canada |  | Canada | Canadian Dollar | CAD |
| KY | CYM | 136 | Cayman Islands |  | Cayman Islands | Cayman Islands Dollar | KYD |
| CF | CAF | 140 | Central African Republic |  | Central African Republic | CFA Franc BEAC | XAF |
| TD | TCD | 148 | Chad | Republic of Chad | Chad | CFA Franc BEAC | XAF |
| CL | CHL | 152 | Chile | Republic of Chile | Chile | Chilean Unit of Account (UF) | CLF |
| CL | CHL | 152 | Chile | Republic of Chile | Chile | Chilean Peso | CLP |
| CN | CHN | 156 | China | People’s Republic of China | China | Chinese Yuan (Offshore) | CNH |
| CN | CHN | 156 | China | People’s Republic of China | China | Chinese Yuan | CNY |
| CX | CXR | 162 | Christmas Island |  | Christmas Island | Australian Dollar | AUD |
| CC | CCK | 166 | Cocos (Keeling) Islands |  | Cocos (Keeling) Islands | Australian Dollar | AUD |
| CO | COL | 170 | Colombia | Republic of Colombia | Colombia | Colombian Peso | COP |
| KM | COM | 174 | Comoros | Union of the Comoros | Comoros | Comorian Franc | KMF |
| CG | COG | 178 | Congo | Republic of the Congo | Congo | CFA Franc BEAC | XAF |
| CD | COD | 180 | Congo, The Democratic Republic of the |  | Congo, The Democratic Republic of the | Congolese Franc | CDF |
| CK | COK | 184 | Cook Islands |  | Cook Islands | New Zealand Dollar | NZD |
| CR | CRI | 188 | Costa Rica | Republic of Costa Rica | Costa Rica | Costa Rican Colón | CRC |
| CI | CIV | 384 | Côte d’Ivoire | Republic of Côte d’Ivoire | Côte d’Ivoire | CFA Franc BCEAO | XOF |
| HR | HRV | 191 | Croatia | Republic of Croatia | Croatia | Croatian Kuna | HRK |
| CU | CUB | 192 | Cuba | Republic of Cuba | Cuba | Cuban Convertible Peso | CUC |
| CU | CUB | 192 | Cuba | Republic of Cuba | Cuba | Cuban Peso | CUP |
| CW | CUW | 531 | Curaçao | Curaçao | Curaçao | Netherlands Antillean Guilder | ANG |
| CY | CYP | 196 | Cyprus | Republic of Cyprus | Cyprus | Euro | EUR |
| CZ | CZE | 203 | Czechia | Czech Republic | Czechia | Czech Republic Koruna | CZK |
| DK | DNK | 208 | Denmark | Kingdom of Denmark | Denmark | Danish Krone | DKK |
| DJ | DJI | 262 | Djibouti | Republic of Djibouti | Djibouti | Djiboutian Franc | DJF |
| DM | DMA | 212 | Dominica | Commonwealth of Dominica | Dominica | East Caribbean Dollar | XCD |
| DO | DOM | 214 | Dominican Republic |  | Dominican Republic | Dominican Peso | DOP |
| EC | ECU | 218 | Ecuador | Republic of Ecuador | Ecuador | United States Dollar | USD |
| EG | EGY | 818 | Egypt | Arab Republic of Egypt | Egypt | Egyptian Pound | EGP |
| SV | SLV | 222 | El Salvador | Republic of El Salvador | El Salvador | Salvadoran Colón | SVC |
| GQ | GNQ | 226 | Equatorial Guinea | Republic of Equatorial Guinea | Equatorial Guinea | CFA Franc BEAC | XAF |
| ER | ERI | 232 | Eritrea | the State of Eritrea | Eritrea | Eritrean Nakfa | ERN |
| EE | EST | 233 | Estonia | Republic of Estonia | Estonia | Euro | EUR |
| SZ | SWZ | 748 | Eswatini | Kingdom of Eswatini | Eswatini | Swazi Lilangeni | SZL |
| ET | ETH | 231 | Ethiopia | Federal Democratic Republic of Ethiopia | Ethiopia | Ethiopian Birr | ETB |
| FK | FLK | 238 | Falkland Islands (Malvinas) |  | Falkland Islands (Malvinas) | Falkland Islands Pound | FKP |
| FO | FRO | 234 | Faroe Islands |  | Faroe Islands | Faroese Krone | DKK |
| FJ | FJI | 242 | Fiji | Republic of Fiji | Fiji | Fijian Dollar | FJD |
| FI | FIN | 246 | Finland | Republic of Finland | Finland | Euro | EUR |
| FR | FRA | 250 | France | French Republic | France | Euro | EUR |
| GF | GUF | 254 | French Guiana |  | French Guiana | Euro | EUR |
| PF | PYF | 258 | French Polynesia |  | French Polynesia | CFP Franc | XPF |
| TF | ATF | 260 | French Southern Territories |  | French Southern Territories | Euro | EUR |
| GA | GAB | 266 | Gabon | Gabonese Republic | Gabon | CFA Franc BEAC | XAF |
| GM | GMB | 270 | Gambia | Republic of the Gambia | Gambia | Gambian Dalasi | GMD |
| GE | GEO | 268 | Georgia |  | Georgia | Georgian Lari | GEL |
| DE | DEU | 276 | Germany | Federal Republic of Germany | Germany | Euro | EUR |
| GH | GHA | 288 | Ghana | Republic of Ghana | Ghana | Ghanaian Cedi | GHS |
| GI | GIB | 292 | Gibraltar |  | Gibraltar | Gibraltar Pound | GIP |
| GR | GRC | 300 | Greece | Hellenic Republic | Greece | Euro | EUR |
| GL | GRL | 304 | Greenland |  | Greenland | Danish Krone | DKK |
| GD | GRD | 308 | Grenada |  | Grenada | East Caribbean Dollar | XCD |
| GP | GLP | 312 | Guadeloupe |  | Guadeloupe | Euro | EUR |
| GU | GUM | 316 | Guam |  | Guam | United States Dollar | USD |
| GT | GTM | 320 | Guatemala | Republic of Guatemala | Guatemala | Guatemalan Quetzal | GTQ |
| GG | GGY | 831 | Guernsey |  | Guernsey | Guernsey Pound | GGP |
| GN | GIN | 324 | Guinea | Republic of Guinea | Guinea | Guinean Franc | GNF |
| GW | GNB | 624 | Guinea-Bissau | Republic of Guinea-Bissau | Guinea-Bissau | CFA Franc BCEAO | XOF |
| GY | GUY | 328 | Guyana | Republic of Guyana | Guyana | Guyanaese Dollar | GYD |
| HT | HTI | 332 | Haiti | Republic of Haiti | Haiti | Haitian Gourde | HTG |
| HM | HMD | 334 | Heard Island and McDonald Islands |  | Heard Island and McDonald Islands | Australian Dollar | AUD |
| VA | VAT | 336 | Holy See (Vatican City State) |  | Holy See (Vatican City State) | Euro | EUR |
| HN | HND | 340 | Honduras | Republic of Honduras | Honduras | Honduran Lempira | HNL |
| HK | HKG | 344 | Hong Kong | Hong Kong Special Administrative Region of China | Hong Kong | Hong Kong Dollar | HKD |
| HU | HUN | 348 | Hungary | Hungary | Hungary | Hungarian Forint | HUF |
| IS | ISL | 352 | Iceland | Republic of Iceland | Iceland | Icelandic Króna | ISK |
| IN | IND | 356 | India | Republic of India | India | Indian Rupee | INR |
| ID | IDN | 360 | Indonesia | Republic of Indonesia | Indonesia | Indonesian Rupiah | IDR |
| IR | IRN | 364 | Iran, Islamic Republic of | Islamic Republic of Iran | Iran, Islamic Republic of | Iranian Rial | IRR |
| IQ | IRQ | 368 | Iraq | Republic of Iraq | Iraq | Iraqi Dinar | IQD |
| IE | IRL | 372 | Ireland |  | Ireland | Euro | EUR |
| IM | IMN | 833 | Isle of Man |  | Isle of Man | Manx pound | IMP |
| IL | ISR | 376 | Israel | State of Israel | Israel | Israeli New Sheqel | ILS |
| IT | ITA | 380 | Italy | Italian Republic | Italy | Euro | EUR |
| JM | JAM | 388 | Jamaica |  | Jamaica | Jamaican Dollar | JMD |
| JP | JPN | 392 | Japan |  | Japan | Japanese Yen | JPY |
| JE | JEY | 832 | Jersey |  | Jersey | Jersey Pound | JEP |
| JO | JOR | 400 | Jordan | Hashemite Kingdom of Jordan | Jordan | Jordanian Dinar | JOD |
| KZ | KAZ | 398 | Kazakhstan | Republic of Kazakhstan | Kazakhstan | Kazakhstani Tenge | KZT |
| KE | KEN | 404 | Kenya | Republic of Kenya | Kenya | Kenyan Shilling | KES |
| KI | KIR | 296 | Kiribati | Republic of Kiribati | Kiribati | Australian Dollar | AUD |
| KP | PRK | 408 | Korea, Democratic People’s Republic of | Democratic People’s Republic of Korea | Korea, Democratic People’s Republic of | North Korean Won | KPW |
| KR | KOR | 410 | Korea, Republic of |  | Korea, Republic of | South Korean Won | KRW |
| XK | KOV |  | Kosovo |  |  | Euro | EUR |
| KW | KWT | 414 | Kuwait | State of Kuwait | Kuwait | Kuwaiti Dinar | KWD |
| KG | KGZ | 417 | Kyrgyzstan | Kyrgyz Republic | Kyrgyzstan | Kyrgystani Som | KGS |
| LA | LAO | 418 | Lao People’s Democratic Republic |  | Lao People’s Democratic Republic | Laotian Kip | LAK |
| LV | LVA | 428 | Latvia | Republic of Latvia | Latvia | Euro | EUR |
| LB | LBN | 422 | Lebanon | Lebanese Republic | Lebanon | Lebanese Pound | LBP |
| LS | LSO | 426 | Lesotho | Kingdom of Lesotho | Lesotho | Lesotho Loti | LSL |
| LR | LBR | 430 | Liberia | Republic of Liberia | Liberia | Liberian Dollar | LRD |
| LY | LBY | 434 | Libya | Libya | Libya | Libyan Dinar | LYD |
| LI | LIE | 438 | Liechtenstein | Principality of Liechtenstein | Liechtenstein | Swiss Franc | CHF |
| LT | LTU | 440 | Lithuania | Republic of Lithuania | Lithuania | Euro | EUR |
| LU | LUX | 442 | Luxembourg | Grand Duchy of Luxembourg | Luxembourg | Euro | EUR |
| MO | MAC | 446 | Macao | Macao Special Administrative Region of China | Macao | Macanese Pataca | MOP |
| MG | MDG | 450 | Madagascar | Republic of Madagascar | Madagascar | Malagasy Ariary | MGA |
| MW | MWI | 454 | Malawi | Republic of Malawi | Malawi | Malawian Kwacha | MWK |
| MY | MYS | 458 | Malaysia |  | Malaysia | Malaysian Ringgit | MYR |
| MV | MDV | 462 | Maldives | Republic of Maldives | Maldives | Maldivian Rufiyaa | MVR |
| ML | MLI | 466 | Mali | Republic of Mali | Mali | CFA Franc BCEAO | XOF |
| MT | MLT | 470 | Malta | Republic of Malta | Malta | Euro | EUR |
| MH | MHL | 584 | Marshall Islands | Republic of the Marshall Islands | Marshall Islands | United States Dollar | USD |
| MQ | MTQ | 474 | Martinique |  | Martinique | Euro | EUR |
| MR | MRT | 478 | Mauritania | Islamic Republic of Mauritania | Mauritania | Mauritanian Ouguiya (pre-2018) | MRO |
| MR | MRT | 478 | Mauritania | Islamic Republic of Mauritania | Mauritania | Mauritanian Ouguiya | MRU |
| MU | MUS | 480 | Mauritius | Republic of Mauritius | Mauritius | Mauritian Rupee | MUR |
| YT | MYT | 175 | Mayotte |  | Mayotte | Euro | EUR |
| MX | MEX | 484 | Mexico | United Mexican States | Mexico | Mexican Peso | MXN |
| FM | FSM | 583 | Micronesia, Federated States of | Federated States of Micronesia | Micronesia, Federated States of | United States Dollar | USD |
| MD | MDA | 498 | Moldova, Republic of | Republic of Moldova | Moldova, Republic of | Moldovan Leu | MDL |
| MC | MCO | 492 | Monaco | Principality of Monaco | Monaco | Euro | EUR |
| MN | MNG | 496 | Mongolia |  | Mongolia | Mongolian Tugrik | MNT |
| ME | MNE | 499 | Montenegro | Montenegro | Montenegro | Euro | EUR |
| MS | MSR | 500 | Montserrat |  | Montserrat | East Caribbean Dollar | XCD |
| MA | MAR | 504 | Morocco | Kingdom of Morocco | Morocco | Moroccan Dirham | MAD |
| MZ | MOZ | 508 | Mozambique | Republic of Mozambique | Mozambique | Mozambican Metical | MZN |
| MM | MMR | 104 | Myanmar | Republic of Myanmar | Myanmar | Myanma Kyat | MMK |
| NA | NAM | 516 | Namibia | Republic of Namibia | Namibia | Namibian Dollar | NAD |
| NR | NRU | 520 | Nauru | Republic of Nauru | Nauru | Australian Dollar | AUD |
| NP | NPL | 524 | Nepal | Federal Democratic Republic of Nepal | Nepal | Nepalese Rupee | NPR |
| NL | NLD | 528 | Netherlands | Kingdom of the Netherlands | Netherlands | Euro | EUR |
| NC | NCL | 540 | New Caledonia |  | New Caledonia | CFP Franc | XPF |
| NZ | NZL | 554 | New Zealand |  | New Zealand | New Zealand Dollar | NZD |
| NI | NIC | 558 | Nicaragua | Republic of Nicaragua | Nicaragua | Nicaraguan Córdoba | NIO |
| NE | NER | 562 | Niger | Republic of the Niger | Niger | CFA Franc BCEAO | XOF |
| NG | NGA | 566 | Nigeria | Federal Republic of Nigeria | Nigeria | Nigerian Naira | NGN |
| NU | NIU | 570 | Niue | Niue | Niue | New Zealand Dollar | NZD |
| NF | NFK | 574 | Norfolk Island |  | Norfolk Island | Australian Dollar | AUD |
| MK | MKD | 807 | North Macedonia | Republic of North Macedonia | North Macedonia | Macedonian Denar | MKD |
| MP | MNP | 580 | Northern Mariana Islands | Commonwealth of the Northern Mariana Islands | Northern Mariana Islands | United States Dollar | USD |
| NO | NOR | 578 | Norway | Kingdom of Norway | Norway | Norwegian Krone | NOK |
| OM | OMN | 512 | Oman | Sultanate of Oman | Oman | Omani Rial | OMR |
| PK | PAK | 586 | Pakistan | Islamic Republic of Pakistan | Pakistan | Pakistani Rupee | PKR |
| PW | PLW | 585 | Palau | Republic of Palau | Palau | United States Dollar | USD |
| PS | PSE | 275 | Palestine, State of | the State of Palestine | Palestine, State of | Israeli New Sheqel | ILS |
| PA | PAN | 591 | Panama | Republic of Panama | Panama | Panamanian Balboa | PAB |
| PG | PNG | 598 | Papua New Guinea | Independent State of Papua New Guinea | Papua New Guinea | Papua New Guinean Kina | PGK |
| PY | PRY | 600 | Paraguay | Republic of Paraguay | Paraguay | Paraguayan Guarani | PYG |
| PE | PER | 604 | Peru | Republic of Peru | Peru | Peruvian Nuevo Sol | PEN |
| PH | PHL | 608 | Philippines | Republic of the Philippines | Philippines | Philippine Peso | PHP |
| PN | PCN | 612 | Pitcairn |  | Pitcairn | New Zealand Dollar | NZD |
| PL | POL | 616 | Poland | Republic of Poland | Poland | Polish Zloty | PLN |
| PT | PRT | 620 | Portugal | Portuguese Republic | Portugal | Euro | EUR |
| PR | PRI | 630 | Puerto Rico |  | Puerto Rico | United States Dollar | USD |
| QA | QAT | 634 | Qatar | State of Qatar | Qatar | Qatari Rial | QAR |
| RE | REU | 638 | Réunion |  | Réunion | Euro | EUR |
| RO | ROU | 642 | Romania |  | Romania | Romanian Leu | RON |
| RU | RUS | 643 | Russian Federation |  | Russian Federation | Russian Ruble | RUB |
| RW | RWA | 646 | Rwanda | Rwandese Republic | Rwanda | Rwandan Franc | RWF |
| BL | BLM | 652 | Saint Barthélemy |  | Saint Barthélemy | Euro | EUR |
| SH | SHN | 654 | Saint Helena, Ascension and Tristan da Cunha |  | Saint Helena, Ascension and Tristan da Cunha | Saint Helena Pound | SHP |
| KN | KNA | 659 | Saint Kitts and Nevis |  | Saint Kitts and Nevis | East Caribbean Dollar | XCD |
| LC | LCA | 662 | Saint Lucia |  | Saint Lucia | East Caribbean Dollar | XCD |
| MF | MAF | 663 | Saint Martin (French part) |  | Saint Martin (French part) | Euro | EUR |
| PM | SPM | 666 | Saint Pierre and Miquelon |  | Saint Pierre and Miquelon | Euro | EUR |
| VC | VCT | 670 | Saint Vincent and the Grenadines |  | Saint Vincent and the Grenadines | East Caribbean Dollar | XCD |
| VC | VCT | 671 | Saint Vincent and the Grenadines |  | Saint Vincent and the Grenadines | East Caribbean Dollar | XCD |
| WS | WSM | 882 | Samoa | Independent State of Samoa | Samoa | Samoan Tala | WST |
| SM | SMR | 674 | San Marino | Republic of San Marino | San Marino | Euro | EUR |
| ST | STP | 678 | Sao Tome and Principe | Democratic Republic of Sao Tome and Principe | Sao Tome and Principe | São Tomé and Príncipe Dobra (pre-2018) | STD |
| ST | STP | 678 | Sao Tome and Principe | Democratic Republic of Sao Tome and Principe | Sao Tome and Principe | São Tomé and Príncipe Dobra | STN |
| SA | SAU | 682 | Saudi Arabia | Kingdom of Saudi Arabia | Saudi Arabia | Saudi Riyal | SAR |
| SN | SEN | 686 | Senegal | Republic of Senegal | Senegal | CFA Franc BCEAO | XOF |
| RS | SRB | 688 | Serbia | Republic of Serbia | Serbia | Serbian Dinar | RSD |
| SC | SYC | 690 | Seychelles | Republic of Seychelles | Seychelles | Seychellois Rupee | SCR |
| SL | SLE | 694 | Sierra Leone | Republic of Sierra Leone | Sierra Leone | Sierra Leonean Leone | SLL |
| SG | SGP | 702 | Singapore | Republic of Singapore | Singapore | Singapore Dollar | SGD |
| SX | SXM | 534 | Sint Maarten (Dutch part) | Sint Maarten (Dutch part) | Sint Maarten (Dutch part) | Netherlands Antillean Guilder | ANG |
| SK | SVK | 703 | Slovakia | Slovak Republic | Slovakia | Euro | EUR |
| SI | SVN | 705 | Slovenia | Republic of Slovenia | Slovenia | Euro | EUR |
| SB | SLB | 090 | Solomon Islands |  | Solomon Islands | Solomon Islands Dollar | SBD |
| SO | SOM | 706 | Somalia | Federal Republic of Somalia | Somalia | Somali Shilling | SOS |
| ZA | ZAF | 710 | South Africa | Republic of South Africa | South Africa | South African Rand | ZAR |
| GS | SGS | 239 | South Georgia and the South Sandwich Islands |  | South Georgia and the South Sandwich Islands | Falkland Islands Pound | FKP |
| GS | SGS | 240 | South Georgia and the South Sandwich Islands |  | South Georgia and the South Sandwich Islands | Falkland Islands Pound | FKP |
| SS | SSD | 728 | South Sudan | Republic of South Sudan | South Sudan | South Sudanese Pound | SSP |
| ES | ESP | 724 | Spain | Kingdom of Spain | Spain | Euro | EUR |
| LK | LKA | 144 | Sri Lanka | Democratic Socialist Republic of Sri Lanka | Sri Lanka | Sri Lankan Rupee | LKR |
| SD | SDN | 729 | Sudan | Republic of the Sudan | Sudan | Sudanese Pound | SDG |
| SR | SUR | 740 | Suriname | Republic of Suriname | Suriname | Surinamese Dollar | SRD |
| SJ | SJM | 744 | Svalbard and Jan Mayen |  | Svalbard and Jan Mayen | Norwegian Krone | NOK |
| SE | SWE | 752 | Sweden | Kingdom of Sweden | Sweden | Swedish Krona | SEK |
| CH | CHE | 756 | Switzerland | Swiss Confederation | Switzerland | Swiss Franc | CHF |
| SY | SYR | 760 | Syrian Arab Republic |  | Syrian Arab Republic | Syrian Pound | SYP |
| TW | TWN | 158 | Taiwan, Province of China | Taiwan, Province of China | Taiwan, Province of China | New Taiwan Dollar | TWD |
| TJ | TJK | 762 | Tajikistan | Republic of Tajikistan | Tajikistan | Tajikistani Somoni | TJS |
| TZ | TZA | 834 | Tanzania, United Republic of | United Republic of Tanzania | Tanzania, United Republic of | Tanzanian Shilling | TZS |
| TH | THA | 764 | Thailand | Kingdom of Thailand | Thailand | Thai Baht | THB |
| TL | TLS | 626 | Timor-Leste | Democratic Republic of Timor-Leste | Timor-Leste | United States Dollar | USD |
| TG | TGO | 768 | Togo | Togolese Republic | Togo | CFA Franc BCEAO | XOF |
| TK | TKL | 772 | Tokelau |  | Tokelau | New Zealand Dollar | NZD |
| TO | TON | 776 | Tonga | Kingdom of Tonga | Tonga | Tongan Pa’anga | TOP |
| TT | TTO | 780 | Trinidad and Tobago | Republic of Trinidad and Tobago | Trinidad and Tobago | Trinidad and Tobago Dollar | TTD |
| TT | TTO | 781 | Trinidad and Tobago | Republic of Trinidad and Tobago | Trinidad and Tobago | Trinidad and Tobago Dollar | TTD |
| TN | TUN | 788 | Tunisia | Republic of Tunisia | Tunisia | Tunisian Dinar | TND |
| TR | TUR | 792 | Turkey | Republic of Turkey | Turkey | Turkish Lira | TRY |
| TM | TKM | 795 | Turkmenistan |  | Turkmenistan | Turkmenistani Manat | TMT |
| TC | TCA | 796 | Turks and Caicos Islands |  | Turks and Caicos Islands | United States Dollar | USD |
| TV | TUV | 798 | Tuvalu |  | Tuvalu | Australian Dollar | AUD |
| UG | UGA | 800 | Uganda | Republic of Uganda | Uganda | Ugandan Shilling | UGX |
| UA | UKR | 804 | Ukraine |  | Ukraine | Ukrainian Hryvnia | UAH |
| AE | ARE | 784 | United Arab Emirates |  | United Arab Emirates | United Arab Emirates Dirham | AED |
| GB | GBR | 826 | United Kingdom | United Kingdom of Great Britain and Northern Ireland | United Kingdom | British Pound Sterling | GBP |
| US | USA | 840 | United States | United States of America | United States | United States Dollar | USD |
| UM | UMI | 581 | United States Minor Outlying Islands |  | United States Minor Outlying Islands | United States Dollar | USD |
| UY | URY | 858 | Uruguay | Eastern Republic of Uruguay | Uruguay | Uruguayan Peso | UYU |
| UZ | UZB | 860 | Uzbekistan | Republic of Uzbekistan | Uzbekistan | Uzbekistan Som | UZS |
| VU | VUT | 548 | Vanuatu | Republic of Vanuatu | Vanuatu | Vanuatu Vatu | VUV |
| VE | VEN | 862 | Venezuela, Bolivarian Republic of | Bolivarian Republic of Venezuela | Venezuela, Bolivarian Republic of | Venezuelan Bolívar Fuerte (Old) | VEF |
| VE | VEN | 862 | Venezuela, Bolivarian Republic of | Bolivarian Republic of Venezuela | Venezuela, Bolivarian Republic of | Venezuelan Bolívar Soberano | VES |
| VN | VNM | 704 | Viet Nam | Socialist Republic of Viet Nam | Viet Nam | Vietnamese Dong | VND |
| VG | VGB | 092 | Virgin Islands, British | British Virgin Islands | Virgin Islands, British | United States Dollar | USD |
| VI | VIR | 850 | Virgin Islands, U.S. | Virgin Islands of the United States | Virgin Islands, U.S. | United States Dollar | USD |
| WF | WLF | 876 | Wallis and Futuna |  | Wallis and Futuna | CFP Franc | XPF |
| EH | ESH | 732 | Western Sahara |  | Western Sahara | Moroccan Dirham | MAD |
| YE | YEM | 887 | Yemen | Republic of Yemen | Yemen | Yemeni Rial | YER |
| ZM | ZMB | 894 | Zambia | Republic of Zambia | Zambia | Zambian Kwacha | ZMW |
| ZW | ZWE | 716 | Zimbabwe | Republic of Zimbabwe | Zimbabwe | Zimbabwean Dollar | ZWL |

## Data Sources

- List of countries (via `ISOcodes::ISO_3166_1`) from ISOcodes
  ([CRAN](https://cran.r-project.org/web/packages/ISOcodes/))
- List of currencies (via `priceR::currencies()`) from priceR
  ([CRAN](https://cran.r-project.org/package=priceR) &
  [GitHub](https://github.com/stevecondylios/priceR))
- Wikipedia’s [List of circulating
  currencies](https://en.wikipedia.org/wiki/List_of_circulating_currencies)
- Individual currency [Wikipedia](https://en.wikipedia.org/) pages (too
  many to list)
- Individual country [Wikipedia](https://en.wikipedia.org/) pages (also,
  too many to list)
