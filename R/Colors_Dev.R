#'Full Color List
#'
#'A full list of the individual colors used in this package, including name and
#'hex code
#'
#' @export
jta_color_list <- c(
  `jta_blue1` = "#002855",
  `jta_blue2` = "#0095C8",

  `jta_green2` = "#78BE20",
  
  `jta_grey1` = "#707372",
  `jta_grey2` = "#E0E2DB",

  `jta_marigold` = "#EFA00B",

  `jta_purple` = "#501537",
  
  `jta_teal` = "#00A599",

  `jta_red1` = "#CB333B",
  `jta_red2` = "#AF272F",

  `off_white` = "#E0E2DB"

)

### function takes a group of strings and returns the string and hex code
### can be used to call one of the colors contained in the full color list
### within a plotting function

#'Retrieve a Color
#'
#'This function takes a string or group of strings and returns hex codes
#'for the corresponding colors
#'
#'A full list of the colors in this package can be returned by running the
#'function with no input
#'
#' @export
jta_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (jta_color_list)

  jta_color_list[cols]
}

#'Full Palette List
#'
#'A full list of the color pallets used in this package
#'
#' @export
jta_color_palettes <- list(
  `blue1_ramp` = jta_cols("off_white", "jta_blue1"),
  `blue2_ramp` = jta_cols("off_white", "jta_blue2"),
  `green_ramp` = jta_cols("off_white", "jta_green2"),
  `grey1_ramp` = jta_cols("off_white", "jta_grey1"),
  `grey2_ramp` = jta_cols("off_white", "jta_grey2"),
  `marigold_red` = jta_cols("jta_marigold", "off_white", "jta_red1"),
  `marigold_teal` = jta_cols("jta_marigold", "off_white", "jta_red1"),
  `marigold_ramp` = jta_cols("off_white", "jta_marigold"),
  `purple_blue` = jta_cols("jta_purple", "off_white", "jta_blue2"),
  `purple_ramp` = jta_cols("off_white", "jta_purple"),
  `red1_ramp` = jta_cols("off_white", "jta_red1"),
  `red2_ramp` = jta_cols("off_white", "jta_red2"),
  `teal_ramp` = jta_cols("off_white", "jta_teal"),
  `jta_rainbow` = jta_cols("jta_red2", "jta_blue2","jta_blue1",
                           "jta_teal","jta_green2","off_white",
                           "jta_marigold","jta_purple")


#'Retrieve a Palette
#'
#'This function excepts a character string for a palette and returns a function
#'that can be turned into a palette
#'
#'adding a number after the function call will create a palette with that number
#'of steps
#'
#'@param palette A string representing a color palette, see 
#'for a full list of palettes
#'
#'@param reverse When true the default order of the color palette is reversed
#'
#'@return a function to create a color palette
#'
#' @export
jta_color_pal <- function(palette = "logo", reverse = FALSE, ...) {
  pal <- jta_color_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#'Create a Color Scale for ggplot
#'
#'This function is inserted into a ggplot call to replace a scale_color
#'command.
#'
#'By default the palette will be discrete.
#'
#'@param palette A string representing a color palette, see https://bfroebrpg.github.io/rpgcolorsr_pages/
#'for a full list of palettes
#'
#'@param discrete Changes between discrete or continuous scales
#'
#'@param reverse When true the default order of the color palette is reversed
#'
#'@return a function to create a color palette
#' @export
scale_color_jta <- function(palette = "logo", discrete = TRUE, reverse = FALSE, ...) {
  pal <- jta_color_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("rpg_color_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#'Create a Fill Scale for ggplot
#'
#'This function is inserted into a ggplot call to replace a scale_fill
#'command.
#'
#'By default the palette will be discrete.
#'
#'@param palette A string representing a color palette, see https://bfroebrpg.github.io/rpgcolorsr_pages/
#'for a full list of palettes
#'
#'@param discrete Changes between discrete or continuous scales
#'
#'@param reverse When true the default order of the color palette is reversed
#'
#'@return a function to create a color palette
#' @export
scale_fill_jta <- function(palette = "logo", discrete = TRUE, reverse = FALSE, ...) {
  pal <- jta_color_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("rpg_color_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


