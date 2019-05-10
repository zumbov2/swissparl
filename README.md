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
Let's find out how [Cédric Wermuth](https://www.parlament.ch/de/biografie/c%C3%A9dric-wermuth/4057) and [Thomas Aeschi](https://www.parlament.ch/de/biografie/thomas-aeschi/4053) voted on the corporate tax reform (USR III) back in 2016. First, I need to get their IDs.
```r
# Fetching all current councillors
clrs <- swissparl::get_councillors2(current = T)

# Exracting IDs
ids <- clrs %>% 
  dplyr::filter(lastName %in% c("Wermuth", "Aeschi")) %>% 
  dplyr::pull(number)
```
Now I also need the ID of the affair. I remember that the Federal Council presented his bill in 2015.
```r
# Fetching bills/affairs of 2015 
afrs <- swissparl::get_affairs2(year = 2015)

# Exracting ID
bill <- afrs %>% 
  dplyr::filter(stringr::str_detect(title, "Unternehmenssteuerreformgesetz III")) %>% 
  dplyr::pull(title)
```
This takes quite some time...the API is not really designed for such queries.
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

## More on Swiss politics 
- [DigDemLab](https://digdemlab.io/)  
- [swissdd](https://github.com/politanch/swissdd)
