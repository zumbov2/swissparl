[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/swissparl)](https://cran.r-project.org/package=swissparl)
![Lifecycle](https://img.shields.io/badge/lifecycle-maturing-orange.svg)
[![Build Status](https://travis-ci.org/zumbov2/swissparl.svg?branch=master)](https://travis-ci.org/zumbov2/swissparl)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<img src="logo.png" height="180px" align="right" style="padding-left:10px;background-color:white;"/>

# swissparl 
This R package prototype is an interface to the new, still unofficial and accordingly still undocumented [Webservices](https://ws.parlament.ch/odata.svc/) of [The Federal Assembly — The Swiss Parliament](https://www.parlament.ch/en) that offer an open, machine-readable interface to the most important data on parliamentary activities. The previous version of the package (interface to the old Webservices) can be found [here](https://github.com/zumbov2/swissparl/tree/master/old_ws).

## Installation
Version 0.2.0 is on CRAN and can be installed as follows:

```r
install.packages("swissparl")
```

The latest version is available on GitHub (0.2.0):

```r
install.packages("devtools")
devtools::install_github("zumbov2/swissparl")
```

# Functions
## Helpers
The new [Webservices](https://ws.parlament.ch/odata.svc/) are comprehensive and not yet documented. The following functions will help you make friends with them:

### `get_tables`
Retrieves the names of all available tables or datasets (currently 43).
``` r
swissparl::get_tables()
#> 
#>  [1] "Bill"                   "BillLink"              
#>  [3] "BillStatus"             "Business"              
#>  [5] "BusinessResponsibility" "BusinessRole"          
#>  [7] "BusinessStatus"         "BusinessType"          
#>  [9] "Canton"                 "Citizenship"           
#> [11] "Committee"              "Council"               
#> [13] "External"               "LegislativePeriod"     
#> [15] "Meeting"                "MemberCommittee"       
#> [17] "MemberCommitteeHistory" "MemberCouncil"         
#> [19] "MemberCouncilHistory"   "MemberParlGroup"       
#> [21] "MemberParty"            "Objective"             
#> [23] "ParlGroup"              "ParlGroupHistory"      
#> [25] "Party"                  "Person"                
#> [27] "PersonAddress"          "PersonCommunication"   
#> [29] "PersonEmployee"         "PersonInterest"        
#> [31] "PersonOccupation"       "Preconsultation"       
#> [33] "Publication"            "RelatedBusiness"       
#> [35] "Resolution"             "SeatOrganisationNr"    
#> [37] "Session"                "Subject"               
#> [39] "SubjectBusiness"        "Tags"                  
#> [41] "Transcript"             "Vote"                  
#> [43] "Voting"
```
### `get_variables` 
Retrieves the names of all the variables of a given table.
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
### `get_overview` 
Wraps around `get_tables` and `get_variables` and retrieves all available tables and variables. 
``` r
swissparl::get_overview(silent = T)
#> 
#> # A tibble: 685 x 2
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
#> # ... with 675 more rows
```
### `get_glimpse`
Downloads the first rows of a given table and gives a first insight into the data structure.
``` r
swissparl::get_glimpse("Person", rows = 100)
#> 
#> # A tibble: 100 x 21
#>       ID Language PersonNumber PersonIdCode Title TitleText LastName
#>    <int> <chr>           <int>        <int> <int> <chr>     <chr>   
#>  1     1 DE                  1         2200    NA <NA>      Aguet   
#>  2     2 DE                  2         2002    NA <NA>      Allensp~
#>  3     6 DE                  6         2004     9 dipl. Ba~ Aregger 
#>  4     7 DE                  7         2005    NA <NA>      Aubry   
#>  5     8 DE                  8         2008    NA <NA>      Bär     
#>  6     9 DE                  9         2268    10 dipl. In~ Baumann 
#>  7    10 DE                 10         2269     6 Dr. iur.  Baumber~
#>  8    11 DE                 11         2011    12 lic. phi~ Bäumlin 
#>  9    12 DE                 12         2335   115 lic. iur. Beerli  
#> 10    13 DE                 13         2202     3 lic. en ~ Béguin  
#> # ... with 90 more rows, and 14 more variables: GenderAsString <chr>,
#> #   DateOfBirth <date>, DateOfDeath <date>, MaritalStatus <lgl>,
#> #   MaritalStatusText <lgl>, PlaceOfBirthCity <chr>,
#> #   PlaceOfBirthCanton <chr>, Modified <dttm>, FirstName <chr>,
#> #   OfficialName <chr>, MilitaryRank <int>, MilitaryRankText <chr>,
#> #   NativeLanguage <chr>, NumberOfChildren <lgl>
```
## Main function `get_data`
The main function of the package is `get_data`. It can be used to download entire datasets or selected rows from any available table.
``` r
swissparl::get_data("Person", Language = "DE")
#> 
#> # A tibble: 3,629 x 21
#>       ID Language PersonNumber PersonIdCode Title TitleText LastName
#>    <int> <chr>           <int>        <int> <int> <chr>     <chr>   
#>  1     1 DE                  1         2200    NA <NA>      Aguet   
#>  2     2 DE                  2         2002    NA <NA>      Allensp~
#>  3     6 DE                  6         2004     9 dipl. Ba~ Aregger 
#>  4     7 DE                  7         2005    NA <NA>      Aubry   
#>  5     8 DE                  8         2008    NA <NA>      Bär     
#>  6     9 DE                  9         2268    10 dipl. In~ Baumann 
#>  7    10 DE                 10         2269     6 Dr. iur.  Baumber~
#>  8    11 DE                 11         2011    12 lic. phi~ Bäumlin 
#>  9    12 DE                 12         2335   115 lic. iur. Beerli  
#> 10    13 DE                 13         2202     3 lic. en ~ Béguin  
#> # ... with 3,619 more rows, and 14 more variables: GenderAsString <chr>,
#> #   DateOfBirth <date>, DateOfDeath <date>, MaritalStatus <int>,
#> #   MaritalStatusText <chr>, PlaceOfBirthCity <chr>,
#> #   PlaceOfBirthCanton <chr>, Modified <dttm>, FirstName <chr>,
#> #   OfficialName <chr>, MilitaryRank <int>, MilitaryRankText <chr>,
#> #   NativeLanguage <chr>, NumberOfChildren <int>
```
### Subsetting with ...
The function uses ... (ellipsis) to subset tables and can therefore be applied very flexibly. For example, it can be used to download all speech transcripts of a given councillor: 
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
### Periods
Or it can be used to fetch detailed information on all political businesses submitted during a **given period**:
``` r
swissparl::get_data(
    table = "Business", 
    SubmissionDate = c(">2019-06-30", "<2019-12-08"), 
    Language = "DE"
    )
#> 
#> # A tibble: 815 x 43
#>        ID Language BusinessShortNu~ BusinessType BusinessTypeName
#>     <int> <chr>    <chr>                   <int> <chr>           
#>  1 2.02e7 DE       19.005                      2 Geschäft des Pa~
#>  2 2.02e7 DE       19.041                      1 Geschäft des Bu~
#>  3 2.02e7 DE       19.042                      1 Geschäft des Bu~
#>  4 2.02e7 DE       19.045                      1 Geschäft des Bu~
#>  5 2.02e7 DE       19.046                      1 Geschäft des Bu~
#>  6 2.02e7 DE       19.047                      1 Geschäft des Bu~
#>  7 2.02e7 DE       19.048                      1 Geschäft des Bu~
#>  8 2.02e7 DE       19.049                      1 Geschäft des Bu~
#>  9 2.02e7 DE       19.050                      1 Geschäft des Bu~
#> 10 2.02e7 DE       19.051                      1 Geschäft des Bu~
#> # ... with 805 more rows, and 38 more variables:
#> #   BusinessTypeAbbreviation <chr>, Title <chr>, Description <chr>,
#> #   InitialSituation <chr>, Proceedings <chr>, DraftText <lgl>,
#> #   SubmittedText <chr>, ReasonText <chr>, DocumentationText <lgl>,
#> #   MotionText <lgl>, FederalCouncilResponseText <chr>,
#> #   FederalCouncilProposal <int>, FederalCouncilProposalText <chr>,
#> #   FederalCouncilProposalDate <date>, SubmittedBy <chr>,
#> #   BusinessStatus <int>, BusinessStatusText <chr>,
#> #   BusinessStatusDate <date>, ResponsibleDepartment <int>,
#> #   ResponsibleDepartmentName <chr>,
#> #   ResponsibleDepartmentAbbreviation <chr>, IsLeadingDepartment <lgl>,
#> #   Tags <chr>, Category <chr>, Modified <dttm>, SubmissionDate <date>,
#> #   SubmissionCouncil <int>, SubmissionCouncilName <chr>,
#> #   SubmissionCouncilAbbreviation <chr>, SubmissionSession <int>,
#> #   SubmissionLegislativePeriod <int>, FirstCouncil1 <int>,
#> #   FirstCouncil1Name <chr>, FirstCouncil1Abbreviation <chr>,
#> #   FirstCouncil2 <int>, FirstCouncil2Name <chr>,
#> #   FirstCouncil2Abbreviation <chr>, TagNames <chr>
```
### Substrings
To a certain extent, it is also possible to search for **substring matches in texts**. For example, to search for all political businesses that contain *CO2* in the title, enter the following query: 
``` r
swissparl::get_data(table = "Business", Title = "~CO2", Language = "DE")
#> 
#> # A tibble: 207 x 43
#>        ID Language BusinessShortNu~ BusinessType BusinessTypeName
#>     <int> <chr>    <chr>                   <int> <chr>           
#>  1 1.99e7 DE       92.3245                     5 Motion          
#>  2 2.00e7 DE       95.2011                    10 Petition        
#>  3 2.00e7 DE       95.3546                     5 Motion          
#>  4 2.00e7 DE       97.030                      1 Geschäft des Bu~
#>  5 2.00e7 DE       00.5227                    14 Fragestunde. Fr~
#>  6 2.00e7 DE       01.420                      4 Parlamentarisch~
#>  7 2.00e7 DE       01.421                      4 Parlamentarisch~
#>  8 2.00e7 DE       01.422                      4 Parlamentarisch~
#>  9 2.00e7 DE       01.3178                     5 Motion          
#> 10 2.00e7 DE       01.3225                     8 Interpellation  
#> # ... with 197 more rows, and 38 more variables:
#> #   BusinessTypeAbbreviation <chr>, Title <chr>, Description <chr>,
#> #   InitialSituation <chr>, Proceedings <chr>, DraftText <lgl>,
#> #   SubmittedText <chr>, ReasonText <chr>, DocumentationText <lgl>,
#> #   MotionText <lgl>, FederalCouncilResponseText <chr>,
#> #   FederalCouncilProposal <int>, FederalCouncilProposalText <chr>,
#> #   FederalCouncilProposalDate <date>, SubmittedBy <chr>,
#> #   BusinessStatus <int>, BusinessStatusText <chr>,
#> #   BusinessStatusDate <date>, ResponsibleDepartment <int>,
#> #   ResponsibleDepartmentName <chr>,
#> #   ResponsibleDepartmentAbbreviation <chr>, IsLeadingDepartment <lgl>,
#> #   Tags <chr>, Category <chr>, Modified <dttm>, SubmissionDate <date>,
#> #   SubmissionCouncil <int>, SubmissionCouncilName <chr>,
#> #   SubmissionCouncilAbbreviation <chr>, SubmissionSession <int>,
#> #   SubmissionLegislativePeriod <int>, FirstCouncil1 <int>,
#> #   FirstCouncil1Name <chr>, FirstCouncil1Abbreviation <chr>,
#> #   FirstCouncil2 <int>, FirstCouncil2Name <chr>,
#> #   FirstCouncil2Abbreviation <chr>, TagNames <chr>
```

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
# Examples
Here are some application examples:  
- Example 1: [Who with Whom in the Council of States?](https://github.com/zumbov2/swissparl/blob/master/examples/ex1.md) – A Brief Network Analysis
- Example 2: Speech Rate 
- Example 3: [The Federal Council's To-Do List](https://github.com/zumbov2/swissparl/blob/master/examples/ex3.md)
- Example 4: [What Are They Talking About?](https://github.com/zumbov2/swissparl/blob/master/examples/ex4.md)
- Example 5: NOMINATE and Friends
