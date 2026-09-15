# cil_table.R
# Defines table style used in CIL Quarto reports

set_flextable_defaults(
  font.family  = "Authentic Sans",
  font.size    = 10,        # ~13px (flextable uses points)
  border.color = "#0b0b0b",
  background.color = "transparent",
  padding.top    = 8,
  padding.bottom = 4,
  padding.left   = 0,
  padding.right  = 16
)

# Wraps a data frame in a CIL-styled flextable ready to embed in the page.
# Columns size to their content by default. `widths` overrides that: one value
# per column, or named values setting just those columns. `max_width` caps the
# fitted widths, never the ones `widths` names. `full_height = TRUE` drops the
# 400px cap so the whole table renders inline without a vertical scrollbar.
# `unit = "%"` reads `widths` as shares of a full-width table instead of
# absolute sizes, which is how two tables are given matching columns.
# `fill` is a named vector of cell text -> background colour, letting a
# category be scanned down a column by colour as well as read.
# Usage: data |> cil_table(widths = c("Purpose of Funds" = 4))
#        data |> cil_table(widths = c(39, 61), unit = "%")
#        data |> cil_table(fill = c("Heavy Rain" = "#d1ddee"))
cil_table <- function(data, widths = NULL, heights = NULL,
                      max_width = NULL, unit = "in", full_width = TRUE,
                      full_height = FALSE, fill = NULL) {
  ft <- flextable(data)
  pct <- identical(unit, "%")
  # Percentages never reach flextable, so they don't take the fitted path below.
  fitted <- !pct && !(is.null(widths) && is.null(max_width))
  
  if (pct) {
    if (is.null(widths)) {
      stop("`unit = \"%\"` sets column shares, so it needs `widths`.", call. = FALSE)
    }
    if (!is.null(max_width)) {
      stop("`max_width` caps fitted widths in absolute units; `unit = \"%\"` ",
           "sets shares outright. Use one or the other.", call. = FALSE)
    }
    if (!full_width) {
      stop("Percentage widths are shares of a full-width table. Leave ",
           "`full_width = TRUE`.", call. = FALSE)
    }
    if (!is.null(heights)) {
      # `unit` covers heights too, and a row can't be a percentage of anything.
      stop("`heights` needs an absolute unit, which `unit = \"%\"` overrides. ",
           "Set column shares and row heights in separate calls.", call. = FALSE)
    }
    if (length(widths) != ncol(data)) {
      # Named subsets work for absolute widths because dim_pretty() supplies a
      # baseline for the columns left out. Shares have no such baseline.
      stop("`widths` has ", length(widths), " value(s) but `data` has ",
           ncol(data), " column(s). Percentage widths need one per column.",
           call. = FALSE)
    }
  }
  
  if (fitted && full_width) {
    stop("`full_width` stretches columns to the container; `widths`/`max_width` ",
         "pin them. Use one or the other.", call. = FALSE)
  }
  
  if (!fitted) {
    # Browser-side content fitting, like Excel's Autofit. width = 0 leaves the
    # table unconstrained; width = 1 would stretch it to 100% regardless of text.
    ft <- set_table_properties(ft, if (full_width) 1 else 0, layout = "autofit")
  } else {
    # Fitted widths are the baseline, capped by max_width, then overridden by
    # anything `widths` names. "autofit" discards widths, so this must be fixed.
    fit <- unlist(dim_pretty(ft)$widths)
    if (!is.null(max_width)) fit <- pmin(fit, max_width)
    if (is.null(widths)) {
      widths <- fit
    } else if (!is.null(names(widths)) && all(nzchar(names(widths)))) {
      unknown <- setdiff(names(widths), names(data))
      if (length(unknown)) {
        stop("`widths` names no such column: ", paste(unknown, collapse = ", "),
             call. = FALSE)
      }
      fit[match(names(widths), names(data))] <- widths
      widths <- fit
    } else if (length(widths) != ncol(data)) {
      # Otherwise they land on the first columns, misaligning every one.
      stop("`widths` has ", length(widths), " value(s) but `data` has ",
           ncol(data), " column(s). Give one width per column, or name them.",
           call. = FALSE)
    }
    ft <- set_table_properties(ft, layout = "fixed")
    ft <- width(ft, j = seq_along(widths), width = widths, unit = unit)
  }
  
  if (!is.null(heights)) {
    # Heights are only emitted under the "exact" rule; "auto" ignores them.
    ft <- height(ft, height = heights, part = "body", unit = unit)
    ft <- hrule(ft, rule = "exact", part = "body")
  }
  
  if (!is.null(fill)) {
    unknown <- setdiff(names(fill), unlist(lapply(data, as.character)))
    if (length(unknown)) {
      stop("`fill` names a value no cell holds: ", paste(unknown, collapse = ", "),
           call. = FALSE)
    }
    # bg() hands the function one column at a time, so the lookup is on the cell
    # text itself. Anything unnamed -- ids, counts -- keeps the table's own
    # background rather than picking up a colour by accident.
    ft <- bg(ft, part = "body", bg = function(x) {
      matched <- fill[as.character(x)]
      ifelse(is.na(matched), "transparent", matched)
    })
  }
  
  # flextable's width() speaks only in/cm/mm, and autofit emits no column widths
  # at all, so shares are carried by markup instead: a <colgroup> the browser
  # obeys once .scroll-output--pct switches the table to `table-layout: fixed`.
  # Empty otherwise, leaving the sub() below a no-op.
  colgroup <- if (pct) {
    paste0("<colgroup>",
           paste0(sprintf('<col style="width:%s%%;">', widths), collapse = ""),
           "</colgroup>")
  } else ""
  
  ft |>
    border_remove() |>
    bold(bold = TRUE, part = "header") |>
    hline(border = fp_border(color = "#0b0b0b", width = 1.5),  part = "header") |>
    hline(border = fp_border(color = "#969696", width = 0.75), part = "body") |>
    # hline() mirrors each rule onto the next row's top edge as well as the
    # current row's bottom. Under the `border-collapse: separate` these tables
    # need for sticky headers, the two edges stack instead of merging, so every
    # interior rule renders at double weight -- only the final one, with no row
    # below it, looks right. Clearing the tops leaves a single 0.75pt line.
    border(border.top = fp_border(width = 0), part = "body") |>
    padding(
      padding.top = 0, padding.bottom = 4,
      padding.left = 0, padding.right = 8,
      part = "header"
    ) |>
    padding(
      padding.left = 2, padding.right = 8,
      part = "body"
    ) |> 
    # Both steps are needed to keep the table out of a shadow DOM, which no page
    # CSS can reach: ft.shadow drops the static host, and the rename hides the
    # wrapper from tabwid.js, which shadow-roots every .tabwid on DOMContentLoaded.
    htmltools_value(ft.shadow = FALSE) |>
    as.character() |>
    sub(pattern = 'class="tabwid"', replacement = 'class="cil-tabwid"', fixed = TRUE) |>
    sub(pattern = "(<table[^>]*>)", replacement = paste0("\\1", colgroup)) |>
    HTML() |>
    div(class = paste(
      c("scroll-output",
        if (fitted) "scroll-output--fit",
        if (pct) "scroll-output--pct",
        if (full_height) "scroll-output--full"),
      collapse = " "
    ))
}