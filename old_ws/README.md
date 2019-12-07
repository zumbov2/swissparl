[![Build Status](https://travis-ci.org/zumbov2/swissparl.svg?branch=master)](https://travis-ci.org/zumbov2/swissparl)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

# `swissparl`
## The Swiss Parliament Webservices R API
This R package prototype is an interface to the [Webservices](https://www.parlament.ch/en/services/open-data-webservices) of the 
[The Federal Assembly — The Swiss Parliament](https://www.parlament.ch/en) that offer an open, machine-readable interface to the 
most important data on parliamentary activities.

## Installation
```r
install.packages("devtools")
devtools::install_github("zumbov2/swissparl")
```

## Example
Let's find out how [Cédric Wermuth](https://www.parlament.ch/de/biografie/c%C3%A9dric-wermuth/4057) and [Thomas Aeschi](https://www.parlament.ch/de/biografie/thomas-aeschi/4053) voted on the corporate tax reform (USR III) back in 2016. First, we need to get their IDs.
```r
# Fetching all current councillors
clrs <- swissparl::get_councillors()

# Exracting IDs
ids <- clrs %>% 
  dplyr::filter(lastName %in% c("Wermuth", "Aeschi")) %>% 
  dplyr::pull(number)
```
Now we also need the ID of the affair. Unfortunately the affair overview retrieved with `get_affairs()` doesn't contain a title variable. That's why we need to fetch the affair details...this may take some time. At least I remember that the Federal Council presented the bill in 2015. This "speeds" things up a little.
```r
# Fetching all bills/affairs of 2015
afrs <- swissparl::get_affair_details2(year = 2015)

# Exracting ID
bill <- afrs %>% 
  dplyr::filter(stringr::str_detect(title, "Unternehmenssteuerreformgesetz III")) %>% 
  dplyr::pull(id)
```
Now we have all the information we need to perform the actual query. 
```r
# Retrieving voting behavior 
vbh <- swissparl::get_votes_councillors(councillor_id = ids, affair_id = bill)

# Analyzing
vbh %>%
  dplyr::select(lastName, vote.id, councillorVote.decision) %>% 
  tidyr::spread(lastName, councillorVote.decision) %>% 
  dplyr::mutate(same = ifelse(Aeschi == Wermuth, 1, 0)) %>% 
  dplyr::group_by(same) %>% 
  dplyr::count()
```
They voted the same in 7 out of 41 cases...there are councillors who vote more similarly. 

## I want it all...
The package offers various wrapper functions that enable the mass retrieval of data: see `get_affair_details2()`, `get_councillor_details2()`, `get_affairsummaries2()`, `get_faction_members2()`, `get_committee_members2()`, `get_votes_affairs2()` and `get_votes_councillors2()`.

## More data on Swiss politics 
- [DigDemLab](https://digdemlab.io/)  
- [swissdd](https://github.com/politanch/swissdd)
