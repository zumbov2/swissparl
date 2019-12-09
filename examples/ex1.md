Who with whom in the Council of States?
=======================================

The function `get_tables` of `swissparl` shows that the
[Webservices](https://ws.parlament.ch/odata.svc/) contain a table called
*BusinessRole*. By using the `get_glimpse` function, we see that the
table contains data on the role of politicians in businesses.

``` r
glimpse_br <- swissparl::get_glimpse("BusinessRole", rows = 1000)
table(glimpse_br$RoleName)
```

| RoleName		| Freq		|
| ----------------------|:-------------:|
| Bekämpfer(-in)	| 4		|
| Mitunterzeichner(-in)	| 904		|
| Sprecher(-in)		| 8		|
| Urheber(-in)		| 84		|

In the first 1000 rows we find four different roles: authors,
co-signers, speakers and combatants. We want to use this information to
get an idea of who frequently collaborated with whom in the Council of
States in the past legislature period (December 2015 to November 2019).

Businesses of the 50th Legislative Period
-----------------------------------------

As a first step, we have to find out which businesses and proposals were
submitted to the Council during this period. For this we use the
function `get_data` in such a way that only the corresponding businesses
are downloaded. We get the names and values of the necessary variables
with `get_glimpse("Business")`.

``` r
biz <- swissparl::get_data(
  table = "Business", 
  SubmissionCouncilAbbreviation = "SR", 
  SubmissionLegislativePeriod = 50,
  Language = "DE"
  )
```
### Explorative Data Analysis, Pt. I

*732* entries were found. Let’s get to know them better:

``` r
# Packages
require(dplyr)
require(ggplot2)
require(hrbrthemes)

# Plot by type
biz %>% 
  group_by(BusinessTypeAbbreviation) %>% 
  count() %>% 
  ungroup() %>% 
  ggplot(aes(reorder(BusinessTypeAbbreviation, n), n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Who with whom in the Council of States?",
    subtitle = "Political businesses during the 50th legislative period by type",
    caption = "Data: Parliamentary Services of the Federal Assembly, Bern"
    ) +
  theme_ipsum_rc(grid="X") +
  theme(axis.title = element_blank())
```

![](images/g4-1.png)

[Interpellations](https://www.parlament.ch/en/%C3%BCber-das-parlament/parlamentsw%C3%B6rterbuch/parlamentsw%C3%B6rterbuch-detail?WordId=116)
and
[Motions](https://www.parlament.ch/en/%C3%BCber-das-parlament/parlamentsw%C3%B6rterbuch/parlamentsw%C3%B6rterbuch-detail?WordId=146)
are by far the most frequently used business types in the Council of
States.

``` r
# Plot by author
biz %>% 
  group_by(SubmittedBy) %>% 
  count() %>% 
  ungroup() %>% 
  top_n(10, n) %>% 
  mutate(SubmittedBy = stringr::str_remove_all(SubmittedBy, "\\s.*$")) %>% 
  ggplot(aes(reorder(SubmittedBy, n), n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Who with whom in the Council of States?",
    subtitle = "Political businesses during the 50th legislative period by author",
    caption = "Data: Parliamentary Services of the Federal Assembly, Bern"
    ) +
  theme_ipsum_rc(grid="X") +
  theme(axis.title = element_blank())
```

![](images/g5-1.png)

We see that not only individual councillors are launching businesses,
but also commissions. The missing values are likely to be due to
businesses of the Federal Council or the Council Office.

Businesses Roles
----------------

Now that we have all the businesses for the desired period, we can
download the associated business roles using `get_data`.

``` r
biz.roles <- swissparl::get_data(
  table = "BusinessRole", 
  BusinessNumber = biz$ID,
  Language = "DE"
  )
```
### Explorative Data Analysis, Pt. II

*4524* entries were found. Off to the next exploration:

``` r
# Plot by role
biz.roles %>% 
  group_by(RoleName) %>% 
  count() %>% 
  ungroup() %>% 
  ggplot(aes(reorder(RoleName, n), n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Who with whom in the Council of States?",
    subtitle = "Roles in political businesses of the 50th legislative period",
    caption = "Data: Parliamentary Services of the Federal Assembly, Bern"
    ) +
  theme_ipsum_rc(grid="X") +
  theme(axis.title = element_blank())
```

![](images/g7-1.png)

*702* authorships are matched by *3804* co-signatures (on average 5.4).
The table does not include the names of the authors and co-signatories.
We have to find another way to get them. `get_tables` tells us that the
Webservices contain named *MemberCouncil*. We try our luck.

``` r
council.members <- swissparl::get_data(
  table = "MemberCouncil", 
  ID = biz.roles$MemberCouncilNumber,
  Language = "DE"
  )
```

Looks like we got the right idea. We join the data to our `biz.role`
data.

``` r
biz.roles <- left_join(
  biz.roles,
  council.members,
  by = c("MemberCouncilNumber" = "ID")
)  
```

### Explorative Data Analysis, Pt. III

``` r
# Plot by role
biz.roles %>%
  filter(Role %in% c(3, 7)) %>% 
  mutate(name_canton = paste0(LastName, " (", CantonAbbreviation, ")")) %>% 
  group_by(name_canton, RoleName) %>%
  count() %>% 
  ungroup() %>% 
  mutate(RoleName = stringr::str_remove_all(RoleName, "\\(.*?\\)")) %>% 
  pivot_wider(names_from = RoleName, values_from = n) %>% 
  ggplot(aes(Mitunterzeichner, Urheber, label = name_canton)) +
  geom_smooth(method = lm, color = "black", fill = "grey80") +
  geom_point() +
  ggrepel::geom_text_repel() +
  labs(
    title = "Who with whom in the Council of States?",
    subtitle = "Roles in political businesses of the 50th legislative period",
    caption = "Data: Parliamentary Services of the Federal Assembly, Bern"
    ) +
  scale_y_continuous(limits = c(0, 40)) +
  theme_ipsum_rc()
```
![](images/g10-1.png)

There seems to be a big difference in the way council members act.

Network Analysis
----------------

Now that we have the necessary data, we want to address the question of
who collaborated with whom, and how often? We use the method of social
network analysis.

In order to do this, we must first find out who co-signed how often in
whose proposals. We split the authors and the co-signers into two
separate datasets, each including the business number and the councillor
id, and then join them together to create *4021* author co-signer pairs.

``` r
# Business authors
authors <- biz.roles %>%
  filter(Role == 7) %>% 
  filter(!is.na(MemberCouncilNumber)) %>% 
  select(BusinessNumber, MemberCouncilNumber) %>% 
  distinct(BusinessNumber, MemberCouncilNumber, .keep_all = T)

# Business cosigners
cosigners <- biz.roles %>%
  filter(Role == 3) %>% 
  filter(!is.na(MemberCouncilNumber)) %>% 
  select(BusinessNumber, MemberCouncilNumber) %>% 
  distinct(BusinessNumber, MemberCouncilNumber, .keep_all = T)

# Edges
acp <- left_join(
  authors, 
  cosigners, 
  by = "BusinessNumber", 
  suffix = c(".author", ".cosigner")
  )
```

We can then use this dataset to define the links or edges of our
network.

``` r
edges <- acp %>% 
  rename(
    from = "MemberCouncilNumber.cosigner",
    to = "MemberCouncilNumber.author"
    ) %>% 
  group_by(from, to) %>% 
  count() %>% 
  ungroup() %>% 
  filter(!is.na(from))
```

### Explorative Data Analysis, Pt. IV

``` r
edges %>%
  complete(from, to, fill = list(n = 0)) %>% 
  ggplot(aes(n)) +
  geom_bar() +
  labs(
    x = "Num. of\ncooperations",
    y = "Freq.",
    title = "Who with whom in the Council of States?",
    subtitle = "Frequency of author-co-signatory cooperations",
    caption = "Data: Parliamentary Services of the Federal Assembly, Bern"
    ) +
  theme_ipsum_rc() +
  theme(panel.grid.minor = element_blank())
```

![](images/g13-1.png)

If we take into account the implicitly missing values in our data, it
becomes apparent that *no cooperation* occured most frequently. On the
other hand, there also seem to have been a few councillors who very
often support each other’s businesses.

In order to display the network graphically we have to create its nodes.

``` r
nodes <- left_join(
  tibble::tibble(ID = unique(c(edges$from, edges$to))), 
  council.members %>% 
    mutate(ID = ID) %>% 
    select(ID, LastName, GenderAsString, CantonAbbreviation, PartyAbbreviation),
  by = "ID"
  )
```

Now we create a network object using `tidygraph`, which is called a
`tbl_graph`. A `tbl_graph` consists of two tibbles: our edges tibble and
the nodes tibble we just created.

``` r
cs50 <- tidygraph::tbl_graph(
  nodes = nodes %>% mutate(ID = as.character(ID)), 
  edges = edges %>% mutate_at(vars(from, to), as.character), 
  directed = F
  )
```
Finally We can plot our network with the awesome package `ggraph` - an
extension of ggplot2. We use the so-called [eigenvector
centrality](https://en.wikipedia.org/wiki/Eigenvector_centrality) to
highlight the influence of a node/councillor.

``` r
cs50 %>%
  mutate(importance = tidygraph::centrality_eigen(weights = n)) %>%
  mutate(label = paste0(LastName, " (", CantonAbbreviation, ")")) %>% 
    ggraph() + 
    geom_edge_fan(aes(alpha = n, width = n)) + 
    geom_node_point(aes(size = importance)) +
    geom_node_text(aes(label = label)) +
    theme_graph() + 
    theme(legend.position = "none")
```
![](images/g16-1.png)

In this first simple network 3 councillors stand out: Verena Diener Lenz
(glp, ZH), Benedikt Würth (CVP, SG) and Daniel Fässler (CVP, AI). After
a short research it turns out why: They all served only for a short part
of the 50th legislative period in the Council. For this reason, they are
excluded from the rest of the analysis.

``` r
nodes2 <- nodes %>% 
  filter(!ID %in% c(61, 4056, 4237)) %>% 
  mutate(ID = as.character(ID))

edges2 <- edges %>% 
  filter(!from %in% c(61, 4056, 4237)) %>% 
  filter(!to %in% c(61, 4056, 4237)) %>% 
  mutate_at(vars(from, to), as.character)

cs50_2 <- tidygraph::tbl_graph(nodes = nodes2, edges = edges2, directed = F)
```
New try:

``` r
# For reproducibility reasons
set.seed(15)

cs50_2 %>%
  mutate(party = factor(PartyAbbreviation, levels = c("SP", "SVP", "CVP", "FDP-Liberale", "GPS", "BDP", "-"))) %>% 
  mutate(importance = tidygraph::centrality_eigen(weights = n)) %>%
  mutate(label = paste0(LastName, " (", CantonAbbreviation, ")")) %>% 
  ggraph() + 
  geom_edge_fan(aes(alpha = n, width = sqrt(n)), edge_colour = "gray60") + 
  scale_edge_width(range = c(0.001, 2)) +
  scale_edge_alpha(range = c(0.001, 1)) +
  geom_node_point(aes(size = importance^2, color = party)) +
  geom_node_text(aes(label = label), size = 2, nudge_y = -0.08) +
  scale_color_manual(values = c("#EE2A3B", "#3F7B17", "#FF850C", "#104fa0", "#85B229", "#FFDD00", "grey50")) +
  scale_size(range = c(0.1, 10)) +
  labs(
    title = "Who with whom in the Council of States?",
    subtitle = "Co-signing behaviour in the 50th legislative period",
    caption = "Data: Parliamentary Services of the Federal Assembly, Bern\nNode size: eigenvector centrality"
    ) +
  theme_graph() + 
  theme_ipsum_rc() + 
  theme(
    legend.position = "none",  
    panel.grid = element_blank(), 
    axis.text = element_blank(), 
    axis.title = element_blank()
    )
```
![](images/g18-1.png)

As was to be expected, cooperation took place primarily within the
parties and the political camps. But there were exceptions: For example,
the liberal Raphaël Comte from the canton of Neuchâtel, who apparently
often worked together with council members from the nearby cantons of
Fribourg and Jura.

Among the most influential council members (according to eigenvector
centrality) were Claude Hêche, Anne Seydoux-Christe and Didier Berberat
from French-speaking Switzerland and Damian Müller, Erich Ettlin and
Joachim Eder all from central Swiss cantons.
