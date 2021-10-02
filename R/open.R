library("dbplyr")
library("dplyr")
library("ggplot2")
library("RSQLite")
library("RMySQL")
# Connecting R with MySQL
open_bugs <- function() {
    db_bugzilla <- dbConnect(RMySQL::MySQL(), dbname = "rbugs", user = "tester",
                             password = "password-Tester1!",
                             host = "127.0.0.1")
    db_bugs <- tbl(db_bugzilla, "bugs") %>%
        filter(resolution == "")

    bugs <- db_bugs %>%
        collect() %>%
        select(
            bug_id, bug_status, short_desc, version, component_id, rep_platform, op_sys, creation_ts, delta_ts
        )

    db_comments <- db_bugzilla %>%
        tbl("longdescs") %>%
        right_join(db_bugs)
    r_core <- c(3, 5, 9, 18, 19, 28, 34, 54, 137, 151, 216, 308, 413, 420, 1249,
                1330, 2442)
    r <- db_comments %>%
        collect() %>%
        group_by(bug_id) %>%
        summarize(r_core = ifelse(any(who %in% r_core), "yes", "no"),
                  total_comments = n() -1,
                  people = sum(who != 2))

    db_attachments <- db_bugzilla %>%
        tbl("attachments") %>%
        right_join(db_bugs, by = "bug_id")

    at <- db_attachments %>%
        collect() %>%
        group_by(bug_id) %>%
        summarize(has_attachment = ifelse(any(!is.na(attach_id)), "yes", "no"))
    dbb <- full_join(r, bugs)
    dbb <- full_join(dbb, at)

    component_names <- c("2" = "Accuracy",
                         "3" = "Analyses",
                         "4" = "Graphics",
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

    dbDisconnect(db_bugzilla)
    dbb <- dbb %>% mutate(creation_ts = as.POSIXct(creation_ts, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS"),
                          delta_ts = as.POSIXct(delta_ts, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")) %>%
        rowwise() %>%
        mutate(last_changed = max(creation_ts, delta_ts, na.rm = TRUE)) %>%
        ungroup() %>%
        select(-creation_ts, -delta_ts) %>%
        mutate(component_id = component_names[as.character(component_id)])
    as.data.frame(dbb)
}


link_web <- function(x, id = NULL) {
    stopifnot(is.numeric(x))
    if (is.null(id)) {
        id <- x
    }

    # paste0("[", x, "](https://bugs.r-project.org/show_bug.cgi?id=", id, ")")
    paste0('<a href="https://bugs.r-project.org/show_bug.cgi?id=', id, '">', x, '</a>')
}
