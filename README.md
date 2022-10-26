# trelliscope-examples2

This repository contains files that illustrate some of the newer experimental features of trelliscopejs. The R code to generate them is below, along with a description of some the features to look for in each display.

When working with trelliscopejs-lib in devopment mode, you can swap out one display for another by changing a few things in the source code:

1. In https://github.com/trelliscope/trelliscopejs-lib/blob/develop/public/index.html, create a new div with an id that matches that found in the file named "id" in the root of the example directory.
2. In https://github.com/trelliscope/trelliscopejs-lib/blob/develop/src/index.js#L259, update the id.


```r
remotes::install_github("hafen/trelliscopejs")
install.packages("tidyverse")
install.packages("gapminder")
```

```r
library(trelliscopejs)
library(tidyverse)
library(gapminder)


# we'll place all displays in a temporary directory:
path <- tempfile()
dir.create(path)
```

## "Bells and whistles" display

This display, found in the directory "gapminder_bells", shows many of the features all in one. These features include:

#### Pre-specified views

A user that generates a display can pre-specify views that they think a viewer might be interested in. This can help viewers navigate to interesting states of the display without understanding all of the controls. When these views are present, an extra icon in the left sidebar appears called "Views". Currently views just update the window location hash to whatever in specified in the view. This could be made more elegant.

#### User inputs

Currently annotations made by the user are only stored in local storage and there is an interface where you can download all of the annotations or have an email drafted that will send them to someone (this was a specific request from a user but we could make the user experience for this more general).

#### Cognostic groups

Displays can have so many cognostics to the point that it is difficult to find a cognostic you might want to filter or sort on. You can now specify cognostic "groups" that will help visually organize how the cognostics are shown in the sort and filter sidebars. Below I'm using `auto_cog = TRUE` which will analyze what is being plotted and create relavent statistical summaries and place them into groups.

```r
qplot(year, lifeExp, data = gapminder) +
  xlim(1948, 2011) + ylim(10, 95) + theme_bw() +
  facet_trelliscope(~ country + continent,
    name = "gapminder_life_expectancy",
    desc = "life expectancy vs. year by country using Gapminder data",
    nrow = 2, ncol = 6, width = 300,
    views = view_list(
      view_item(
        name = "Sorted by mean life expectancy (low to high)",
        hash = "&nrow=2&ncol=6&arr=row&pg=1&labels=country,correct,comments,lifeExp_mean&sort=lifeExp_mean;asc&filter=&fv="
      ),
      view_item(
        name = "African countries",
        hash = "&nrow=2&ncol=6&arr=row&pg=1&labels=country,correct,comments&sort=&filter=var:continent;type:select;val:Africa&sidebar=-1&fv=continent"
      )
    ),
    inputs = input_cogs(
      input_radio(
        name = "correct",
        desc = "Does the data for this panel appear to be correct?",
        options = c("yes", "no")),
      input_text(
        name = "comments",
        desc = "Comments about the data in this panel",
        width = 80, height = 4),
      feedback_email = "user@email.com"
    ),
    path = file.path(path, "gapminder_bells"))
```

## Multiple dislays and related displays

This display can be found in the "gapminder_reldisp" folder.

You can have multiple displays in a single app instance. In this case, when the app opens, a list of displays to choose from will appear. I'm not sure how often this feature is used.

On this same topic, there is the notion of "related displays", where if you create multiple displays on the same partitioning of the same dataset in the same output path, you have the option in the UI to view the two different displays side-by-side.

For example, we can create two displays with the gapminder data, one with life expectancy vs. time and another with GBP vs. time:

```r
qplot(year, lifeExp, data = gapminder) +
  xlim(1948, 2011) + ylim(10, 95) + theme_bw() +
  facet_trelliscope(~ country + continent,
    name = "gapminder_life_expectancy",
    desc = "life expectancy vs. year by country using Gapminder data",
    nrow = 2, ncol = 6, width = 300,
    path = file.path(path, "gapminder_reldisp"))

qplot(year, log10(gdpPercap), data = gapminder) +
  xlim(1948, 2011) + ylim(2.35, 5.1) + theme_bw() +
  facet_trelliscope(~ country + continent,
    name = "gapminder_gdp",
    desc = "life expectancy vs. year by country using Gapminder data",
    nrow = 2, ncol = 6, width = 300,
    path = file.path(path, "gapminder_reldisp"))
```

When the display opens, choose either display to view. Then click the folder icon with a plus sign that can be found in the top left toolbar, and choose the second display. Now each display will be shown side-by-side for each country. Note that when using related diplays, the panel layout is forced to one row and one column.

The app knows which panels from the two different displays to show based on its panel "signature", which is an md5 hash of the partitioning variables for the displays. For example, since both displays above have the same partitioning, we should expect a panel in each of the displays to have the same signature, e.g. md5 hash of Africa/Tanzania, etc.

## Panels that are not raster images

```r
library(visNetwork)

nnodes <- 100
nnedges <- 1000

nodes <- data.frame(
  id = 1:nnodes,
  label = 1:nnodes, value = rep(1, nnodes))
edges <- data.frame(
  from = sample(1:nnodes, nnedges, replace = T),
  to = sample(1:nnodes, nnedges, replace = T)) %>%
    group_by(from, to) %>%
    summarise(value = n())

network_plot <- function(id, hide_select = TRUE) {
  style <- ifelse(hide_select,
    "visibility: hidden; position: absolute", "")

  visNetwork(nodes, edges) %>%
    visIgraphLayout(layout = "layout_in_circle") %>%
    visNodes(fixed = TRUE,
      label = id,
      scaling = list(
        min = 20, max = 50,
        label = list(min = 35, max = 70,
          drawThreshold = 1, maxVisible = 100))) %>%
    visEdges(scaling = list(min = 5, max = 30)) %>%
    visOptions(highlightNearest = list(enabled = TRUE, degree = 0,
      hideColor = "rgba(200,200,200,0.2)"),
      nodesIdSelection = list(selected = as.character(id), style = style))
}

nodedat <- edges %>%
  group_by(from) %>%
  summarise(n_nodes = n(), tot_conns = sum(value)) %>%
  rename(id = from) %>%
  arrange(-n_nodes) %>%
  mutate(panel = map_plot(id, network_plot))

network_plot(1)

nodedat %>%
  arrange(-n_nodes) %>%
  trelliscope(name = "connections", nrow = 2, ncol = 4,
  path = file.path(path, "network_nonraster"))
```

## Image panels

```r
load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_7261/datasets/pokemon.Rdata"))

pokemon %>%
  mutate(
    pokemon = cog(pokemon, default_label = TRUE),
    panel = img_panel(url_image)
  ) %>%
  trelliscope(name = "pokemon", nrow = 3, ncol = 6,
    path = file.path(path, "pokemon"))
```

## Self-contained

```r
d <- ggplot2::mpg |>
  tidyr::nest(data = !dplyr::one_of(c("manufacturer", "class"))) |>
  dplyr::mutate(
    mean_cty = purrr::map_dbl(data, function(x) mean(x$cty)),
    panel = map_plot(data, function(x) ggplot2::qplot(hwy, cty, data = x)),
    class2 = factor(class)
  )

d %>% trelliscope(name = "city_vs_highway_mpg",
  self_contained = TRUE, path = file.path(path, "self_contained"))

list.files(path)
```

## With built JS library

When trelliscope is used outside of the `yarn start` environment, an html page is built that loads the js and css and instantiates a trelliscope app. An example of this is in the `with_build_library` folder of this repo.

The contents of `with_build_library/lib` is empty but you can populate it by issuing the following commands while in the trelliscopejs-lib diretory:

```
yarn build

cp -r build/static/ __path__/trelliscope-examples2/with_built_library/lib
```

Then make sure the references in the `<link/>` and `<script/>` tags in index.html are pointing to the correct files in `with_built_library/lib`.
