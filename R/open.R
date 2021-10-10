library("dbplyr")
library("dplyr")
library("ggplot2")
library("RSQLite")
library("RMySQL")
# Connecting R with MySQL
open_bugs <- function() {
    db_bugzilla <- dbConnect(RMySQL::MySQL(),  dbname = "rbugs")
    db_bugs <- tbl(db_bugzilla, "bugs") |>
        filter(resolution == "")

    bugs <- db_bugs |>
        collect() |>
        select(
            bug_id, bug_status, short_desc, version, component_id, rep_platform, op_sys, creation_ts, delta_ts
        )

    db_comments <- db_bugzilla |>
        tbl("longdescs") |>
        right_join(db_bugs)
    r_core <- c(3, 5, 9, 18, 19, 28, 34, 54, 137, 151, 216, 308, 413, 420, 1249,
                1330, 2442)
    r <- db_comments |>
        collect() |>
        group_by(bug_id) |>
        summarize(r_core = ifelse(any(who %in% r_core), "yes", "no"),
                  total_comments = n() - 1,
                  people = sum(who != 2))

    db_attachments <- db_bugzilla |>
        tbl("attachments") |>
        right_join(db_bugs, by = "bug_id")

    at <- db_attachments |>
        collect() |>
        group_by(bug_id) |>
        summarize(has_attachment = ifelse(any(!is.na(attach_id)), "yes", "no"))
    dbb <- full_join(r, bugs)
    dbb <- full_join(dbb, at)

    component_names <- c("2" = "Accuracy",
                         "3" = "Analyses",
                         "4" = "Graphics",
                         "5" = "Installation",
                         "6" = "Low-level",
                         "8" = "S4methods",
                         "7" = "Misc",
                         "9" = "System-specific",
                         "10" = "Translations",
                         "11" = "Documentation",
                         "12" = "Language",
                         "13" = "Startup",
                         "14" = "Models",
                         "15" = "Add-ons",
                         "16" = "I/O",
                         "17" = "Wishlist",
                         "18" = "Mac GUI / Mac specific",
                         "19" = "Windows GUI / Window specific"
    )

    if (any(!as.character(dbb$component_id) %in% names(component_names))) {
        warning("Please report it that some components aren't known")
    }

    today <- Sys.Date()
    tp <- db_bugzilla |>
        tbl("bugs_activity") |>
        collect() |>
        filter(bug_id %in% dbb$bug_id) |>
        filter(added %in% c("CONFIRMED", "ASSIGNED")) |>
        group_by(bug_id) |>
        summarize(takes_patches = (today-max(as.Date(bug_when))) > 15)
    dbb <- merge(dbb, tp, by = "bug_id", all = TRUE, sort = FALSE) |>
        mutate(takes_patches = if_else(is.na(takes_patches), FALSE, takes_patches))
    dbDisconnect(db_bugzilla)
    dbb <- mutate(dbb,
                  creation_ts = as.POSIXct(creation_ts, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS"),
                  delta_ts = as.POSIXct(delta_ts, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")) |>
        rowwise() |>
        mutate(last_changed = max(creation_ts, delta_ts, na.rm = TRUE)) |>
        ungroup() |>
        mutate(component_id = component_names[as.character(component_id)],
               opened = format(creation_ts, "%Y-%m-%d"),
               last_changed = format(last_changed, "%Y-%m-%d %H:%M")) |>
        select(-creation_ts, -delta_ts)
    as.data.frame(dbb)
}

link_web <- function(x, id = NULL) {
    stopifnot(is.numeric(x))
    if (is.null(id)) {
        id <- x
    }

    # paste0("[", x, "](https://bugs.r-project.org/show_bug.cgi?id=", id, ")")
    paste0('<a href="https://bugs.r-project.org/show_bug.cgi?id=', id,
           '">', x, '</a>')
}


plot_component_core <- function(bugs){
    library("ggplot2")
    bugs |>
        group_by(r_core, component_id) |>
        count() |>
        ungroup() |>
        ggplot() +
        geom_tile(aes(r_core, component_id, fill = n)) +
        scale_fill_viridis_c() +
        theme_minimal() +
        labs(x = "R Core?", y = "Component", fill = "Issues")
}


plot_status_version <- function(bugs){
    library("ggplot2")
    bugs |>
        group_by(bug_status, version) |>
        count() |>
        ungroup() |>
        ggplot() +
        geom_tile(aes(bug_status, version, fill = n)) +
        scale_fill_viridis_c() +
        theme_minimal() +
        labs(x = "Status", y = "Version", fill = "Issues")
}

plot_system_attachment <- function(bugs){
    library("ggplot2")
    bugs |>
        group_by(op_sys, has_attachment) |>
        count() |>
        ungroup() |>
        ggplot() +
        geom_tile(aes(has_attachment, op_sys, fill = n)) +
        scale_fill_viridis_c() +
        theme_minimal() +
        labs(x = "Has attachment?", y = "OS", fill = "Issues")
}
