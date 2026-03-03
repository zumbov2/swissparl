[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/swissparl)](https://cran.r-project.org/package=swissparl)
![Lifecycle](https://img.shields.io/badge/lifecycle-maturing-orange.svg)
[![cranlogs](https://cranlogs.r-pkg.org/badges/grand-total/swissparl)](http://cran.rstudio.com/web/packages/swissparl/index.html)
<img src="logo.png" height="120px" align="right" style="padding-left:10px;background-color:white;"/>

# swissparl
This R package provides convenient access to parliamentary data of the Swiss 
Federal Assembly via the official OData [Web Services](https://ws.parlament.ch/odata.svc/) 
of the [Federal Assembly — The Swiss Parliament](https://www.parlament.ch/en). 

Starting with version 0.3.0, the package also supports the 
[OpenParlData.ch REST API](https://api.openparldata.ch/), which offers 
harmonized parliamentary data for the Swiss Federal Assembly as well 
as selected cantonal and municipal parliaments.

Functions ending in `2` access the OpenParlData API.

## Installation
Version 0.2.2 is on CRAN and can be installed as follows:

```r
install.packages("swissparl")
```

The latest version is available on GitHub (0.3.0):

```r
install.packages("devtools")
devtools::install_github("zumbov2/swissparl")
```

# Functions

## Helpers

The package provides helper functions for both supported interfaces:

- Functions without suffix access the official Swiss Parliament OData Web Services.
- Functions ending in `2` access the OpenParlData.ch REST API.

The following functions help you explore the available tables and variables.

### `get_tables`
Retrieves the names of all available OData tables (`get_tables`) and OpenParlData resources (`get_tables2`).

``` r
swissparl::get_tables()
#>
#>  [1] "Bill"                   "BillLink"               "BillStatus"            
#>  [4] "Business"               "BusinessResponsibility" "BusinessRole"          
#>  [7] "BusinessStatus"         "BusinessType"           "Canton"                
#> [10] "Citizenship"            "Committee"              "Council"               
#> [13] "External"               "LegislativePeriod"      "Meeting"               
#> [16] "MemberCommittee"        "MemberCommitteeHistory" "MemberCouncil"         
#> [19] "MemberCouncilHistory"   "MemberParlGroup"        "MemberParlGroupHistory"
#> [22] "MemberParty"            "MemberPartyHistory"     "Mutation"              
#> [25] "Objective"              "ParlGroup"              "ParlGroupHistory"      
#> [28] "Party"                  "Person"                 "PersonAddress"         
#> [31] "PersonCommunication"    "PersonEmployee"         "PersonInterest"        
#> [34] "PersonOccupation"       "Preconsultation"        "Publication"           
#> [37] "Rapporteur"             "RelatedBusiness"        "Resolution"            
#> [40] "SeatOrganisationNr"     "SeatOrganisationSr"     "Session"               
#> [43] "Subject"                "SubjectBusiness"        "Tags"                  
#> [46] "Transcript"             "Vote"                   "Voting"
```

``` r
swissparl::get_tables2()
#>
#>  [1] "access_badges"  "affairs"        "agendas"        "bodies"        
#>  [5] "contributors"   "docs"           "events"         "external_links"
#>  [9] "groups"         "interests"      "meetings"       "memberships"   
#> [13] "person_images"  "persons"        "speeches"       "texts"         
#> [17] "votes"          "votings"
```

### `get_variables`
Retrieves the names of all variables of a given table (`get_variables`) or resource (`get_variables2`).

``` r
swissparl::get_variables("Transcript")
#>
#>  [1] "CantonAbbreviation"         "CantonId"                  
#>  [3] "CantonName"                 "CouncilId"                 
#>  [5] "CouncilName"                "DisplaySpeaker"            
#>  [7] "End"                        "EndTimeWithTimezone"       
#>  [9] "Function"                   "ID"                        
#> [11] "IdSession"                  "IdSubject"                 
#> [13] "Language"                   "LanguageOfText"            
#> [15] "MeetingCouncilAbbreviation" "MeetingDate"               
#> [17] "MeetingVerbalixOid"         "Modified"                  
#> [19] "ParlGroupAbbreviation"      "ParlGroupName"             
#> [21] "PersonNumber"               "SortOrder"                 
#> [23] "SpeakerFirstName"           "SpeakerFullName"           
#> [25] "SpeakerFunction"            "SpeakerLastName"           
#> [27] "Start"                      "StartTimeWithTimezone"     
#> [29] "Text"                       "Type"                      
#> [31] "VoteBusinessNumber"         "VoteBusinessShortNumber"   
#> [33] "VoteBusinessTitle"          "VoteId"
```

``` r
swissparl::get_variables2("access_badges")
#>
#>  [1] "beneficiary_group"           "beneficiary_person_fullname"
#>  [3] "beneficiary_person_id"       "body_id"                    
#>  [5] "body_key"                    "created_at"                 
#>  [7] "external_id"                 "id"                         
#>  [9] "latest"                      "links"                      
#> [11] "person_external_id"          "person_fullname"            
#> [13] "person_id"                   "type_de"                    
#> [15] "type_fr"                     "type_harmonized"            
#> [17] "type_it"                     "updated_at"                 
#> [19] "updated_external_at"         "url_api"                    
#> [21] "valid_from"                  "valid_to"                   
#> [23] "version"
```

### `get_overview`
Wraps around `get_tables`/`get_variables` and retrieves all available tables and 
variables (`get_overview`) or resources and fields (`get_overview2`).

``` r
swissparl::get_overview(silent = T)
#>
#> # A tibble: 757 × 2
#>    table variable                
#>    <chr> <chr>                   
#>  1 Bill  BillNumber              
#>  2 Bill  BillType                
#>  3 Bill  BillTypeName            
#>  4 Bill  BusinessNumber          
#>  5 Bill  BusinessShortNumber     
#>  6 Bill  BusinessStatus          
#>  7 Bill  BusinessStatusDate      
#>  8 Bill  BusinessStatusText      
#>  9 Bill  BusinessType            
#> 10 Bill  BusinessTypeAbbreviation
#> # ℹ 747 more rows
```

``` r
swissparl::get_overview2(silent = T)
#>
#> # A tibble: 560 × 2
#>    table         variable                   
#>    <chr>         <chr>                      
#>  1 access_badges beneficiary_group          
#>  2 access_badges beneficiary_person_fullname
#>  3 access_badges beneficiary_person_id      
#>  4 access_badges body_id                    
#>  5 access_badges body_key                   
#>  6 access_badges created_at                 
#>  7 access_badges external_id                
#>  8 access_badges id                         
#>  9 access_badges latest                     
#> 10 access_badges links                      
#> # ℹ 550 more rows
```

### `get_glimpse`
Downloads the first rows of a given table (`get_glimpse`) or resource 
(`get_glimpse2`) and provides a first insight into the data structure.

``` r
swissparl::get_glimpse("Person", rows = 20)
#>
#> # A tibble: 20 × 21
#>       ID Language PersonNumber PersonIdCode Title TitleText           LastName  
#>    <int> <chr>           <int>        <int> <int> <chr>               <chr>     
#>  1     1 DE                  1         2200    NA <NA>                Aguet     
#>  2     2 DE                  2         2002    NA <NA>                Allenspach
#>  3     6 DE                  6         2004     9 dipl. Bauing. HTL   Aregger   
#>  4     7 DE                  7         2005    NA <NA>                Aubry     
#>  5     8 DE                  8         2008    NA <NA>                Bär       
#>  6     9 DE                  9         2268    10 dipl. Ing. Agr. ETH Baumann   
#>  7    10 DE                 10         2269     6 Dr. iur.            Baumberger
#>  8    11 DE                 11         2011    12 lic. phil. I        Bäumlin   
#>  9    12 DE                 12         2335   115 lic. iur.           Beerli    
#> 10    13 DE                 13         2202     3 lic. en droit       Béguin    
#> 11    14 DE                 14         2264   162 dipl. Arch. FH/STV  Bezzola   
#> 12    15 DE                 15         2270   155 dipl. Landwirt      Binder    
#> 13    16 DE                 16         2260    NA <NA>                Bircher   
#> 14    17 DE                 17         2015    NA <NA>                Bircher   
#> 15    18 DE                 18         2271    NA <NA>                Bischof   
#> 16    19 DE                 19         2336    43 dipl. Arch. ETH     Bisig     
#> 17    20 DE                 20         2016    NA <NA>                Blatter   
#> 18    21 DE                 21         2017     6 Dr. iur.            Blocher   
#> 19    22 DE                 22         2337    95 dipl. Bauing. ETH   Bloetzer  
#> 20    23 DE                 23         2018    NA <NA>                Bodenmann 
#> # ℹ 14 more variables: GenderAsString <chr>, DateOfBirth <date>,
#> #   DateOfDeath <date>, MaritalStatus <lgl>, MaritalStatusText <lgl>,
#> #   PlaceOfBirthCity <chr>, PlaceOfBirthCanton <chr>, Modified <dttm>,
#> #   FirstName <chr>, OfficialName <chr>, MilitaryRank <int>,
#> #   MilitaryRankText <chr>, NativeLanguage <lgl>, NumberOfChildren <lgl>
```

``` r
swissparl::get_glimpse2("persons", rows = 20)
#>
#> # A tibble: 20 × 62
#>       id url_api      body_key external_id external_alternative…¹ title fullname
#>    <int> <chr>        <chr>    <chr>       <lgl>                  <lgl> <chr>   
#>  1 24862 https://api… 1024     10000003    NA                     NA    Andreas…
#>  2 24863 https://api… 1024     10000004    NA                     NA    Annemar…
#>  3 24864 https://api… 1024     10000005    NA                     NA    Armin B…
#>  4 24865 https://api… 1024     10000007    NA                     NA    Christi…
#>  5 24866 https://api… 1024     10000008    NA                     NA    Christi…
#>  6 24867 https://api… 1024     10000009    NA                     NA    Christo…
#>  7 24868 https://api… 1024     10000011    NA                     NA    Daniel …
#>  8 24869 https://api… 1024     10000013    NA                     NA    Edi Sch…
#>  9 24870 https://api… 1024     10000014    NA                     NA    Ernst W…
#> 10 24871 https://api… 1024     10000015    NA                     NA    Eugen B…
#> 11 24873 https://api… 1024     10000017    NA                     NA    Franz K…
#> 12 24874 https://api… 1024     10000018    NA                     NA    Franz R…
#> 13 24875 https://api… 1024     10000019    NA                     NA    Frowin …
#> 14 24876 https://api… 1024     10000020    NA                     NA    Gisela …
#> 15 24877 https://api… 1024     10000021    NA                     NA    Hans He…
#> 16 24878 https://api… 1024     10000022    NA                     NA    Hans Sc…
#> 17 24879 https://api… 1024     10000023    NA                     NA    Hanspet…
#> 18 24880 https://api… 1024     10000024    NA                     NA    Hanspet…
#> 19 24881 https://api… 1024     10000025    NA                     NA    Hansrue…
#> 20 24882 https://api… 1024     10000026    NA                     NA    Herbert…
#> # ℹ abbreviated name: ¹​external_alternative_id
#> # ℹ 55 more variables: firstname <chr>, lastname <chr>, body_id <int>,
#> #   party_de <chr>, party_fr <lgl>, party_it <lgl>, party_external_id <lgl>,
#> #   party_harmonized_de <chr>, party_harmonized_fr <chr>,
#> #   party_harmonized_it <chr>, party_harmonized_en <chr>,
#> #   party_harmonized_wikidata_id <chr>, website_parliament_url_de <lgl>,
#> #   website_parliament_url_fr <lgl>, website_parliament_url_it <lgl>, …
```

## Main functions `get_data`

The main data retrieval functions of the package are `get_data` and `get_data2`.  
They can be used to download entire datasets or selected rows from any available
OData table (`get_data`) or OpenParlData resource (`get_data2`).

``` r
swissparl::get_data("Person", Language = "DE")
#>
#> # A tibble: 3,712 × 21
#>       ID Language PersonNumber PersonIdCode Title TitleText           LastName  
#>    <int> <chr>           <int>        <int> <int> <chr>               <chr>     
#>  1     1 DE                  1         2200    NA <NA>                Aguet     
#>  2     2 DE                  2         2002    NA <NA>                Allenspach
#>  3     6 DE                  6         2004     9 dipl. Bauing. HTL   Aregger   
#>  4     7 DE                  7         2005    NA <NA>                Aubry     
#>  5     8 DE                  8         2008    NA <NA>                Bär       
#>  6     9 DE                  9         2268    10 dipl. Ing. Agr. ETH Baumann   
#>  7    10 DE                 10         2269     6 Dr. iur.            Baumberger
#>  8    11 DE                 11         2011    12 lic. phil. I        Bäumlin   
#>  9    12 DE                 12         2335   115 lic. iur.           Beerli    
#> 10    13 DE                 13         2202     3 lic. en droit       Béguin    
#> # ℹ 3,702 more rows
#> # ℹ 14 more variables: GenderAsString <chr>, DateOfBirth <date>,
#> #   DateOfDeath <date>, MaritalStatus <int>, MaritalStatusText <chr>,
#> #   PlaceOfBirthCity <chr>, PlaceOfBirthCanton <chr>, Modified <dttm>,
#> #   FirstName <chr>, OfficialName <chr>, MilitaryRank <int>,
#> #   MilitaryRankText <chr>, NativeLanguage <chr>, NumberOfChildren <int>
```

``` r
swissparl::get_data2("persons")
#> 
#> # A tibble: 25,004 × 62
#>       id url_api      body_key external_id external_alternative…¹ title fullname
#>    <int> <chr>        <chr>    <chr>       <chr>                  <chr> <chr>   
#>  1 24862 https://api… 1024     10000003    <NA>                   <NA>  Andreas…
#>  2 24863 https://api… 1024     10000004    <NA>                   <NA>  Annemar…
#>  3 24864 https://api… 1024     10000005    <NA>                   <NA>  Armin B…
#>  4 24865 https://api… 1024     10000007    <NA>                   <NA>  Christi…
#>  5 24866 https://api… 1024     10000008    <NA>                   <NA>  Christi…
#>  6 24867 https://api… 1024     10000009    <NA>                   <NA>  Christo…
#>  7 24868 https://api… 1024     10000011    <NA>                   <NA>  Daniel …
#>  8 24869 https://api… 1024     10000013    <NA>                   <NA>  Edi Sch…
#>  9 24870 https://api… 1024     10000014    <NA>                   <NA>  Ernst W…
#> 10 24871 https://api… 1024     10000015    <NA>                   <NA>  Eugen B…
#> # ℹ 24,994 more rows
#> # ℹ abbreviated name: ¹​external_alternative_id
#> # ℹ 55 more variables: firstname <chr>, lastname <chr>, body_id <int>,
#> #   party_de <chr>, party_fr <chr>, party_it <chr>, party_external_id <chr>,
#> #   party_harmonized_de <chr>, party_harmonized_fr <chr>,
#> #   party_harmonized_it <chr>, party_harmonized_en <chr>,
#> #   party_harmonized_wikidata_id <chr>, website_parliament_url_de <chr>, …
```

### Subsetting with `...`

Both `get_data` and `get_data2` use `...` (ellipsis) to pass filter arguments to 
the respective interface, allowing flexible subsetting of tables or resources.

For example, the function can be used to download all speech transcripts of a given councillor (Odata):

``` r
swissparl::get_data(
    table = "Transcript", 
    SpeakerLastName = "Blocher", 
    Language = "DE"
    )
#> 
#> # A tibble: 1,380 x 34
#>    ID    Language IdSubject VoteId PersonNumber  Type Text 
#>    <chr> <chr>    <chr>     <lgl>         <int> <int> <chr>
#>  1 63    DE       8         NA               21     1 "<pd~
#>  2 617   DE       113       NA               21     1 "<pd~
#>  3 619   DE       113       NA               21     1 "<pd~
#>  4 639   DE       113       NA               21     1 "<pd~
#>  5 1506  DE       264       NA               21     1 "<pd~
#>  6 1519  DE       264       NA               21     1 "<pd~
#>  7 2517  DE       376       NA               21     1 "<pd~
#>  8 2565  DE       385       NA               21     1 "<pd~
#>  9 2567  DE       385       NA               21     1 "<pd~
#> 10 4254  DE       721       NA               21     1 "<pd~
#> # ... with 1,370 more rows, and 27 more variables:
#> #   MeetingCouncilAbbreviation <chr>, MeetingDate <chr>,
#> #   MeetingVerbalixOid <int>, IdSession <chr>, SpeakerFirstName <chr>,
#> #   SpeakerLastName <chr>, SpeakerFullName <chr>, SpeakerFunction <chr>,
#> #   CouncilId <int>, CouncilName <chr>, CantonId <int>, CantonName <chr>,
#> #   CantonAbbreviation <chr>, ParlGroupName <chr>,
#> #   ParlGroupAbbreviation <chr>, SortOrder <int>, Start <dttm>,
#> #   End <dttm>, Function <chr>, DisplaySpeaker <lgl>,
#> #   LanguageOfText <chr>, Modified <dttm>, StartTimeWithTimezone <dttm>,
#> #   EndTimeWithTimezone <dttm>, VoteBusinessNumber <lgl>,
#> #   VoteBusinessShortNumber <lgl>, VoteBusinessTitle <lgl>
```

Or all affairs of type "Motion" for a given canton (OpenParlData):

``` r
swissparl::get_data2(
  table = "affairs", 
  body_key = "AG",
  type_harmonized_de = "Motion"
  )
#>
#> # A tibble: 698 × 48
#>        id url_api     body_key external_id external_alternative…¹ number body_id
#>     <int> <chr>       <chr>    <chr>       <lgl>                  <chr>    <int>
#>  1 289559 https://ap… AG       6792558     NA                     26.26      250
#>  2 289557 https://ap… AG       6792540     NA                     26.25      250
#>  3 287030 https://ap… AG       6772058     NA                     25.375     250
#>  4 287031 https://ap… AG       6772063     NA                     25.374     250
#>  5 287037 https://ap… AG       6772335     NA                     25.381     250
#>  6 287036 https://ap… AG       6772294     NA                     25.380     250
#>  7 287035 https://ap… AG       6772243     NA                     25.379     250
#>  8 287040 https://ap… AG       6772662     NA                     25.384     250
#>  9 285328 https://ap… AG       6748185     NA                     25.358     250
#> 10 284925 https://ap… AG       6734978     NA                     25.340     250
#> # ℹ 688 more rows
#> # ℹ abbreviated name: ¹​external_alternative_id
#> # ℹ 41 more variables: title_de <chr>, title_fr <lgl>, title_it <lgl>,
#> #   title_rm <lgl>, title_long_de <chr>, title_long_fr <lgl>,
#> #   title_long_it <lgl>, title_long_rm <lgl>, type_name_de <chr>,
#> #   type_name_fr <lgl>, type_name_it <lgl>, type_name_rm <lgl>,
#> #   type_external_id <lgl>, type_harmonized_de <chr>, …
```

### Periods
Or it can also be used to fetch detailed information on all political businesses 
submitted during a **given period**:

``` r
swissparl::get_data(
  table = "Business", 
  SubmissionDate = c(">2025-12-31", "<2026-03-01"),
  Language = "DE"
)
#> 
#> # A tibble: 60 × 43
#>          ID Language BusinessShortNumber BusinessType BusinessTypeName        
#>       <int> <chr>    <chr>                      <int> <chr>                   
#>  1 20250095 DE       25.095                         1 Geschäft des Bundesrates
#>  2 20260008 DE       26.008                         1 Geschäft des Bundesrates
#>  3 20260018 DE       26.018                         1 Geschäft des Bundesrates
#>  4 20260019 DE       26.019                         2 Geschäft des Parlaments 
#>  5 20260020 DE       26.020                         2 Geschäft des Parlaments 
#>  6 20260021 DE       26.021                         1 Geschäft des Bundesrates
#>  7 20260022 DE       26.022                         1 Geschäft des Bundesrates
#>  8 20260024 DE       26.024                         2 Geschäft des Parlaments 
#>  9 20260029 DE       26.029                         1 Geschäft des Bundesrates
#> 10 20260031 DE       26.031                         1 Geschäft des Bundesrates
#> # ℹ 50 more rows
#> # ℹ 38 more variables: BusinessTypeAbbreviation <chr>, Title <chr>,
#> #   Description <chr>, InitialSituation <chr>, Proceedings <chr>,
#> #   DraftText <lgl>, SubmittedText <chr>, ReasonText <chr>,
#> #   DocumentationText <lgl>, MotionText <lgl>,
#> #   FederalCouncilResponseText <chr>, FederalCouncilProposal <int>,
#> #   FederalCouncilProposalText <chr>, FederalCouncilProposalDate <date>, …
```

The equivalent query using the OpenParlData interface is:

``` r
swissparl::get_data2(
  table = "affairs", 
  body_key = "CHE",
  from_begin_date = "2026-01-01",
  to_begin_date = "2026-02-28"
  )
#> 
#> # A tibble: 80 × 48
#>        id url_api     body_key external_id external_alternative…¹ number body_id
#>     <int> <chr>       <chr>    <chr>       <lgl>                  <chr>    <int>
#>  1 295827 https://ap… CHE      20269009    NA                     26.90…      42
#>  2 295826 https://ap… CHE      20269011    NA                     26.90…      42
#>  3 295824 https://ap… CHE      20269013    NA                     26.90…      42
#>  4 295823 https://ap… CHE      20269012    NA                     26.90…      42
#>  5 295822 https://ap… CHE      20269014    NA                     26.90…      42
#>  6 295830 https://ap… CHE      20269004    NA                     26.90…      42
#>  7 295829 https://ap… CHE      20269005    NA                     26.90…      42
#>  8 295834 https://ap… CHE      20263021    NA                     26.30…      42
#>  9 295832 https://ap… CHE      20269002    NA                     26.90…      42
#> 10 295831 https://ap… CHE      20269003    NA                     26.90…      42
#> # ℹ 70 more rows
#> # ℹ abbreviated name: ¹​external_alternative_id
#> # ℹ 41 more variables: title_de <chr>, title_fr <chr>, title_it <chr>,
#> #   title_rm <lgl>, title_long_de <lgl>, title_long_fr <lgl>,
#> #   title_long_it <lgl>, title_long_rm <lgl>, type_name_de <chr>,
#> #   type_name_fr <chr>, type_name_it <chr>, type_name_rm <lgl>,
#> #   type_external_id <chr>, type_harmonized_de <chr>, …
```

In addition to federal parliamentary affairs, the OpenParlData interface also 
includes related procedures such as consultation processes (*Vernehmlassungsverfahren*).


### Text search
Both interfaces support filtering via `...`, but they differ in how text search works.

#### OData: substring matching in a specific field

To a certain extent, it is possible to search for **substring matches in texts**.  
For example, to search for all political businesses that contain *CO2* in the title, enter:

``` r
swissparl::get_data(
  table = "Business", 
  Title = "~CO2", 
  Language = "DE"
  )
#> 
#> # A tibble: 336 × 43
#>          ID Language BusinessShortNumber BusinessType BusinessTypeName          
#>       <int> <chr>    <chr>                      <int> <chr>                     
#>  1 19923245 DE       92.3245                        5 Motion                    
#>  2 19952011 DE       95.2011                       10 Petition                  
#>  3 19953546 DE       95.3546                        5 Motion                    
#>  4 19970030 DE       97.030                         1 Geschäft des Bundesrates  
#>  5 20005227 DE       00.5227                       14 Fragestunde. Frage        
#>  6 20010420 DE       01.420                         4 Parlamentarische Initiati…
#>  7 20010421 DE       01.421                         4 Parlamentarische Initiati…
#>  8 20010422 DE       01.422                         4 Parlamentarische Initiati…
#>  9 20013178 DE       01.3178                        5 Motion                    
#> 10 20013225 DE       01.3225                        8 Interpellation            
#> # ℹ 326 more rows
#> # ℹ 38 more variables: BusinessTypeAbbreviation <chr>, Title <chr>,
#> #   Description <chr>, InitialSituation <chr>, Proceedings <chr>,
#> #   DraftText <lgl>, SubmittedText <chr>, ReasonText <chr>,
#> #   DocumentationText <lgl>, MotionText <lgl>,
#> #   FederalCouncilResponseText <chr>, FederalCouncilProposal <int>,
#> #   FederalCouncilProposalText <chr>, FederalCouncilProposalDate <date>, …
```

#### OpenParlData: extended full-text search

The OpenParlData API provides a dedicated full-text search via `search`. 
The behavior can be controlled with search_mode (e.g. `partial`, `exact`, `natural`, `boolean`) 
and can optionally be narrowed using search_scope and search_language.

For example, a boolean query on affairs:

``` r
swissparl::get_data2(
  table = "affairs",
  search = "(Klima | Umwelt) & Schweiz",
  search_mode = "boolean",
  max_rows = 5
  )
#> 
#> # A tibble: 5 × 48
#>       id url_api      body_key external_id external_alternative…¹ number body_id
#>    <int> <chr>        <chr>    <chr>       <chr>                  <chr>    <int>
#> 1 235917 https://api… CHE      20250022    <NA>                   25.022      42
#> 2 289599 https://api… 351      2cea54c0cd… <NA>                   2024.…     353
#> 3 167533 https://api… OW       95947       <NA>                   21.22…     264
#> 4 233553 https://api… CHE      vis535      <NA>                   VIS 5…      42
#> 5 130253 https://api… BE       1ebd30b978… 025-2022               2022.…     253
#> # ℹ abbreviated name: ¹​external_alternative_id
#> # ℹ 41 more variables: title_de <chr>, title_fr <chr>, title_it <chr>,
#> #   title_rm <lgl>, title_long_de <chr>, title_long_fr <chr>,
#> #   title_long_it <chr>, title_long_rm <lgl>, type_name_de <chr>,
#> #   type_name_fr <chr>, type_name_it <chr>, type_name_rm <lgl>,
#> #   type_external_id <chr>, type_harmonized_de <chr>, type_harmonized_fr <chr>,
#> #   type_harmonized_it <chr>, type_harmonized_rm <chr>, …
```

More information on the OpenParlData API's search modes and parameters can be found [here](https://api.openparldata.ch/documentation). 

### Advice for large queries
Large queries (especially the tables *Voting* and *Transcripts*) may result in **server-side errors** (*500 Internal Server Error*). In these cases it is recommended to download the data in smaller batches, save the individual blocks and combine them after the download. The following code snippet is from example 5, where all votes of the 50th legislature period are downloaded, session by session.

``` r
# Get Session IDs
sessions50 <- swissparl::get_data("Session", Language = "DE", LegislativePeriodNumber = 50)

# Define Function
get_voting_buffered <- function(id) {
  
  # Create folder
  folder <- "voting50"
  if(!dir.exists(folder)) dir.create(folder)
  
  # Download
  dt <- swissparl::get_data("Voting", Language = "DE", IdSession = id)
  
  # Save
  saveRDS(dt, paste0(folder, "/", id, ".rds"))
  
}

# Apply Function to Session IDs
purrr::walk(sessions50$ID, get_voting_buffered)

# Combine to One Dataset
v50 <- purrr::map_dfr(list.files("voting50", full.names = T), readRDS)
```

## Extra features
### `ggswissparl`
The function `ggswissparl` uses the in-built data frame `seating_plan` (based on the the [schematic representation of the National Council Hall](https://www.parlament.ch/en/organe/national-council/groups-chamber-nc)) to visualize the results of ballots in the National Council. Since only the current seating arrangement can be retrieved from the API, only the most recent voting results can be displayed correctly.

``` r
swissparl::get_data("Voting", Language = "DE", IdVote = 23458) %>% 
    swissparl::ggswissparl(theme = "scoreboard")
```
<img src="https://github.com/zumbov2/swissparl/blob/master/plots/scoreboard.png" width="500">  

``` r
swissparl::get_data("Voting", Language = "DE", IdVote = 23458) %>% 
    swissparl::ggswissparl(theme = "poly2")
```
<img src="https://github.com/zumbov2/swissparl/blob/master/plots/poly2.png" width="500">  

### `clean_text`
Clears all texts of line breaks and all non-text-relevant annotations (page numbers).

``` r
swissparl::get_data("Transcript", Language = "DE", ID = 112146) %>%
  pull(Text) %>%
  swissparl::clean_text()

#> [1] "Auf diese Antwort habe ich mich schon den ganzen Sonntag gefreut. (Heiterkeit) Das zur Diskussion stehende gewürzte Fleisch von Tieren der Rindviehgattung wird unter der Zolltarifnummer 1602.5099 (Schlüssel 914) ausserhalb des Zollkontingentes veranlagt. Dem schweizerischen Zolltarif kommt Gesetzesrang zu. Er basiert wie die kombinierte Nomenklatur (KN) der EU und die meisten Zolltarife weltweit auf dem international gültigen Harmonisierten System (HS). Ebenfalls materiell verbindliches internationales Staatsvertragsrecht sind gemäss Rechtsprechung des Bundesverwaltungsgerichtes die Erläuterungen zum HS. Diese sehen vor, dass gewürztes Fleisch (z. B. mit Pfeffer) als zubereitet gilt und somit grundsätzlich zum Kapitel 16 des Zolltarifs gehört. An der Grenze zu vollziehende wirtschaftliche Massnahmen im Allgemeinen und die Höhe der Zollansätze im Besonderen stellen ausdrücklich keine Gründe dar, eine Ware nicht tarifgemäss einzureihen. In Anlehnung an Anmerkung 6a zum Kapitel 2 der KN hat die Zollverwaltung zusätzlich (Heiterkeit) sogenannte 'schweizerische Erläuterungen zum Zolltarif' (Grosse Heiterkeit, Beifall) publiziert. Danach werden gewisse Erzeugnisse noch im Kapitel 2 eingereiht, denen bei der Herstellung Würzstoffe zugesetzt worden sind, sofern dadurch der Charakter einer Ware dieses Kapitels nicht verändert wird (z. B. Bündnerfleisch). (Grosse Heiterkeit) Ausgeschlossen von diesem Kapitel bleibt hingegen Fleisch, bei dem die Würzstoffe auf allen Flächen des Erzeugnisses verteilt und mit blossem Auge wahrnehmbar sind. (Heiterkeit) Nach der Besprechung vom 26. März 2010 mit Vertretern des Bauernverbandes und der Fleischbranche hat die Zollverwaltung diese Erläuterungen inzwischen auf dem Zirkularweg ergänzt. Seit dem 3. Mai 2010 gehört mit ganzen Pfefferkörnern bestreutes Fleisch ebenfalls zum Kapitel 2 des Zolltarifs. Damit wird verhindert, dass Fleischstücke mit Zusatz von ganzen Pfefferkörnern zu den markant tieferen Zollansätzen des Kapitels 16 eingeführt werden können. Eine noch weiter gehende Ausdehnung des Geltungsbereichs des Kapitels 2 stünde in eindeutigem Widerspruch zu den HS-Bestimmungen und damit auch zu den völkerrechtlichen Verpflichtungen der Schweiz. (Heiterkeit) Die Zollverwaltung hat im Rahmen ihrer Kontrolltätigkeit zudem ein Risikoprofil betreffend die Veranlagung von gewürztem Fleisch erstellt. Die entsprechenden Veranlagungen werden somit noch genauer kontrolliert. Ein höherer Zollschutz gegenüber dem geltenden in der Tarifnummer 1602.5099 müsste aus heutiger Sicht in einem Dekonsolidierungsverfahren im Rahmen der WTO aufgrund der Forderungen der Hauptlieferländer durch Zollsenkungen in anderen Tarifnummern und/oder durch ein grösseres Zollkontingent für Rind- und Kalbfleisch kompensiert werden. Die Aussicht, dass ein Dekonsolidierungsverfahren insgesamt eine bessere Situation für die inländische Schlachtvieh- und Fleischbranche mit sich bringt, ist äusserst gering. Es trifft zu, dass die eingeführte Menge unter der Tarifnummer 1602.5099 im Laufe des Jahres 2010 zugenommen hat. Im Vergleich zum jährlichen gesamtschweizerischen Konsum von verkaufsfertigem Rind- und Kalbfleisch (112 000 Tonnen) erscheint die importierte Menge jedoch eher gering (815 Tonnen bis Ende Juni 2010). Herr Nationalrat, ich bitte Sie um Verzeihung, wenn ich bisweilen einfach nicht verstanden habe, was ich Ihnen vorgelesen habe. (Heiterkeit)"
```
## Possibly relevant queries (extended continuously)
### All speeches on a specific political business
It is also possible to download all speech transcripts for a specific business. However, this requires a small detour. The first step is to extract when the business was discussed in the councils. This information is stored in the *SubjectBusiness* table.
``` r
subject <- swissparl::get_data(
  table = "SubjectBusiness",
  BusinessShortNumber = "05.057",
  Language = "DE"
  )
```
Afterwards all speeches of a specific business can be queried via the IdSubject.
``` r
swissparl::get_data(
  table = "Transcript", 
  IdSubject = as.numeric(subject$IdSubject),
  Language = "DE"
  )
```
### All votes of a specific legislative period
In a first step, we fetch all sessions of the legislative period of interest. Then we use the session IDs to obtain the associated voting decisions of the individual MPs.
``` r
sessions <- get_data("Session", Language = "DE")
sessions51 <- sessions %>% filter(LegislativePeriodNumber == 51)
votes51 <- get_data("Vote", Language = "DE", IdSession = sessions51$ID)
```

## Examples
Here are some examples of use:
- **Example 1**: [Who with Whom in the Council of States?](https://github.com/zumbov2/swissparl/blob/master/examples/ex1.md) – *A Brief Network Analysis*
- **Example 2**: [Slow Bernese?](https://github.com/zumbov2/swissparl/blob/master/examples/ex2.md) – *A Cantonal Comparison of Speaking Rates* (by [Benjamin Gföhler](https://www.benjamingfoehler.ch/))
- **Example 3**: [The Federal Council's To-Do List](https://github.com/zumbov2/swissparl/blob/master/examples/ex3.md) – *A Simple Trend Analysis of Procedural Requests*
- **Example 4**: [What Are They Talking About?](https://github.com/zumbov2/swissparl/blob/master/examples/ex4.md) – *Application of the Lingusitic Concept of Keyness to Speeches*
- **Example 5**: [Polarization in the National Council](https://github.com/zumbov2/swissparl/blob/master/examples/ex5.md) – *W-NOMINATE Scores for the 50th Legislative Term*

## Use Cases
- **NZZ, 16.4.2021**: [Die Grünen machten im Parlament am meisten mit Corona Politik](https://www.nzz.ch/visuals/ld.1606433) | [Code](https://github.com/nzzdev/st-methods/tree/master/2107-parlamentsdebatte-corona)

## swissparl for Python
[swissparlpy](https://github.com/metaodi/swissparlpy) by [Stefan Oderbolz](https://github.com/metaodi)

## Suggested Citation
Zumbach, David (2026). swissparl: Interface to the Webservices of the Swiss Parliament. R package version 0.3.0. https://CRAN.R-project.org/package=swissparl
