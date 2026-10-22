auditClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "auditClass",
    inherit = auditBase,
    private = list(
        .run = function() {

            self$results$intro$setContent(INTRO)
            self$results$step3$step3text$setContent(STEP3_TEXT)

            id <- self$options$id
            vars <- setdiff(self$options$vars, id)     # the ID is checked separately
            if (length(vars) == 0) {
                self$results$intro$setContent(paste0(
                    "Move the variables you want to check into **Variables to check**. ",
                    "Put the ID column, if there is one, in **ID variable**.\n\n", INTRO))
                return()
            }

            data <- self$data
            rowNums <- suppressWarnings(as.integer(rownames(data)))
            if (length(rowNums) != nrow(data) || anyNA(rowNums))
                rowNums <- seq_len(nrow(data))

            # extra header rows from survey exports (question text, import codes) are not answers
            hdr <- detectHeaderRows(data, c(id, vars, self$options$keep))
            if (length(hdr$rows) > 0) {
                data <- data[-hdr$rows, , drop = FALSE]
                rowNums <- rowNums[-hdr$rows]
            }

            built <- buildRules(self$options$fixes, vars)
            rules <- built$rules
            res <- auditData(data, vars, id = id, rowNums = rowNums, rules = rules,
                             assoc = isTRUE(self$options$showCorr))
            s1 <- self$results$step1
            if (length(hdr$rows) > 0) {
                hrow <- data.frame(var = "(rows at the top)", type = "qheader",
                    detail = "Question text or import codes, not answers. Left out of all checks.",
                    rows = fmtRows(hdr$rows), issue = ISSUE_LABELS[["qheader"]], stringsAsFactors = FALSE)
                res$issues <- rbind(hrow, res$issues)
            }

            # ---- Step 1: report
            o <- res$overview
            s1$overview$setRow(rowNo = 1, values = list(
                rows = o$rows, vars = o$vars, missing = o$missing,
                pmissing = paste0(format(round(o$pmissing, 1), nsmall = 1), "%"),
                duprows = o$duprows, emptyrows = o$emptyrows))
            if (!o$dupTrusted)
                s1$overview$setNote("dup", paste0("Duplicate rows aren't checked with so few columns: different people ",
                    "often give identical answers. Add an ID variable, or check at least 5 variables."), init = FALSE)

            for (i in seq_len(nrow(res$summary))) {
                r <- res$summary[i, ]
                s1$summary$addRow(rowKey = i, values = list(
                    var = r$var, stored = r$stored, valid = r$valid, pmiss = r$pmiss,
                    distinct = r$distinct, nissues = r$nissues))
            }

            if (nrow(res$issues) == 0) {
                s1$alerts$setNote("none", "No problems found.", init = FALSE)
            } else {
                for (i in seq_len(nrow(res$issues))) {
                    r <- res$issues[i, ]
                    s1$alerts$addRow(rowKey = i, values = list(
                        var = r$var, issue = r$issue, detail = r$detail, rows = r$rows))
                }
                s1$alerts$setNote("rows", "Alerts describe the original data. Row numbers match the spreadsheet; long lists are shortened.", init = FALSE)
            }
            s1$guide$setContent(guideText(res$issues))

            if (isTRUE(self$options$showMissing) && ncol(res$blank) > 0) {
                md <- s1$missingData
                md$missingMap$setState(list(blank = as.matrix(res$blank), vars = colnames(res$blank)))
                mr <- missingReport(res)
                if (is.null(mr$little)) {
                    md$mcar$setRow(rowNo = 1, values = list(chi2 = NaN, df = NaN, p = NaN,
                        verdict = "Not enough numeric variables with gaps to run the test"))
                } else {
                    l <- mr$little
                    md$mcar$setRow(rowNo = 1, values = list(chi2 = l$chi2, df = l$df, p = l$p,
                        verdict = if (l$p < .05) "Gaps are probably NOT completely random"
                                  else "No evidence against MCAR"))
                    md$mcar$setNote("vars", paste0("Based on ", l$nvars, " numeric variables (cleaned values) and ",
                                                   l$patterns, " patterns of gaps."), init = FALSE)
                }
                for (i in seq_len(nrow(mr$perVar))) {
                    r <- mr$perVar[i, ]
                    md$missVars$addRow(rowKey = i, values = list(var = r$var, nmiss = r$nmiss,
                                                                 pmiss = r$pmiss, verdict = r$verdict))
                }
                md$missVars$setNote("clean", "Uses the cleaned values, so missing-value codes count as missing. Completely empty rows are left out.", init = FALSE)
                if (nrow(mr$links) == 0) {
                    md$missLinks$setNote("none", "No links found (p < .05).", init = FALSE)
                } else {
                    for (i in seq_len(nrow(mr$links))) {
                        r <- mr$links[i, ]
                        md$missLinks$addRow(rowKey = i, values = list(var = r$var, with = r$with,
                                                                      detail = r$detail, p = r$p))
                    }
                }
                md$missText$setContent(MISSING_TEXT)
            }
            if (isTRUE(self$options$showCorr) && !is.null(res$assoc))
                s1$corrPlot$setState(res$assoc)

            if (isTRUE(self$options$showCharts)) {
                # only variables with alerts get a chart: drawing one per variable is slow on big surveys
                flagged <- unique(res$issues$var)
                for (v in vars) {
                    img <- s1$charts$get(key = v)
                    st <- if (v %in% flagged) chartState(res$profiles[[v]], res$cleaned[[v]]) else NULL
                    if (is.null(st)) img$setVisible(FALSE) else img$setState(st)
                }
            }

            # ---- Step 2: what happens to each value
            s2 <- self$results$step2
            for (v in vars) {
                tbl <- s2$maps$get(key = v)
                p <- res$profiles[[v]]
                cl <- res$cleaned[[v]]
                vm <- valueMap(p, cl)
                nOpen <- sum(cl$undecided)
                nChanged <- sum(vm$n[!(vm$why %in% c("OK", "Already missing", "Not decided yet")) & !grepl("^Appears", vm$why)])
                ttl <- if (nChanged == 0) "nothing to change" else paste0(nChanged, " value", if (nChanged != 1) "s", " will change")
                nCheck <- sum(vm$n[grepl("^Appears", vm$why)])
                if (nCheck > 0) ttl <- paste0(ttl, " \u00b7 ", nCheck, " to check")
                if (nOpen > 0) ttl <- paste0(ttl, " \u00b7 ", nOpen, " still to decide (see ? in Step 2)")
                tbl$setTitle(paste0(v, ": ", ttl))
                for (i in seq_len(nrow(vm)))
                    tbl$addRow(rowKey = i, values = list(
                        value = vm$value[i], n = vm$n[i], becomes = vm$becomes[i], why = vm$why[i]))
                note <- rangeNote(p, rules[[v]])
                if (!is.null(note)) tbl$setNote("range", note, init = FALSE)
            }

            problems <- built$problems
            if (length(problems) > 0) {
                s2$problems$setContent(paste0("- ", mdEscape(problems), collapse = "\n"))
                s2$problems$setVisible(TRUE)
            }

            # ---- Step 3b: a separate cleaned dataset
            notReady <- notReadyMessage(res, vars)
            keepCols <- setdiff(self$options$keep, c(vars, id))
            nd <- buildNewData(data, vars, id, keepCols, res,
                               tidy = isTRUE(self$options$tidyNames),
                               dropDup = isTRUE(self$options$dropDup),
                               dropEmpty = isTRUE(self$options$dropEmpty),
                               dropAdmin = isTRUE(self$options$dropAdmin),
                               desc = hdr$desc)
            self$results$step3$newData$setRow(rowNo = 1, values = list(
                status = if (is.null(notReady)) "Ready: use the buttons in Step 3" else "Not ready: values marked ? in Step 2",
                ncols = ncol(nd$df), nrows = nrow(nd$df), dropped = nd$dropped))
            if (length(nd$emptyVars) > 0)
                self$results$step3$newData$setNote("empty", paste0("Left out because they hold no data: ",
                    paste(nd$emptyVars, collapse = ", "), "."), init = FALSE)
            if (length(nd$skipped) > 0)
                self$results$step3$newData$setNote("skipped", paste(nd$skipped, collapse = " "), init = FALSE)
            if (length(nd$admin) > 0)
                self$results$step3$newData$setNote("admin", paste0("Survey admin columns left out: ",
                    paste(nd$admin, collapse = ", "), "."), init = FALSE)
            if (length(keepCols) > 0)
                self$results$step3$newData$setNote("keep", paste0("Kept unchanged: ",
                    paste(keepCols, collapse = ", "), "."), init = FALSE)

            private$.runAction("openData", notReady, function(action)
                list(data = nd$df, title = "Cleaned data"))
            private$.runAction("exportData", notReady, function(action) {
                utils::write.csv(nd$df, action$params$fullPath, row.names = FALSE, na = "", fileEncoding = "UTF-8")
                list(filename = "cleaned_data.csv")
            })

            # ---- Step 3: cleaned variables (not for empty ones)
            if (isTRUE(self$options$cleanOV)) {
                open <- vapply(vars, function(v) sum(res$cleaned[[v]]$undecided), 0)
                keep <- vars[res$summary$stored != "Empty" & open == 0]
                created <- self$results$step3$created
                for (i in seq_along(vars)) {
                    v <- vars[i]
                    clean <- if (v %in% keep) paste0(suggestName(v), "_clean")
                             else if (open[[v]] > 0) paste0("not yet: ", open[[v]], " value", if (open[[v]] != 1) "s", " still to decide")
                             else "not created: no data"
                    created$addRow(rowKey = i, values = list(
                        var = v, clean = clean,
                        changed = res$summary$changed[res$summary$var == v]))
                }
                if (length(keep) > 0) {
                    out <- self$results$cleanOV
                    out$set(keys = keep,
                            titles = paste0(vapply(keep, suggestName, ""), "_clean"),
                            descriptions = paste0("Cleaned copy of ", keep, " (Data Check)"),
                            measureTypes = unname(vapply(keep, function(v) res$cleaned[[v]]$measureType, "")))
                    out$setRowNums(rownames(data))
                    for (v in keep) {
                        vals <- res$cleaned[[v]]$values
                        if (!is.null(res$cleaned[[v]]$levels)) vals <- factor(vals, levels = res$cleaned[[v]]$levels, ordered = TRUE)
                        else if (res$cleaned[[v]]$measureType == "nominal") vals <- factor(vals)
                        out$setValues(key = v, values = vals)
                    }
                }
            }
        },

        .runAction = function(name, notReady, fun) {
            if (!isTRUE(self$options[[name]])) return()
            option <- self$options$option(name)
            if (is.null(option$perform)) return()
            option$perform(function(action) {
                if (!is.null(notReady)) stop(notReady, call. = FALSE)
                fun(action)
            })
        },

        # ---- charts
        .plotVar = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            cols <- c("Unchanged" = "#9BB7B8", "Will change" = "#C0702A", "Missing code" = "#7A3F0C")
            if (st$type == "num") {
                d <- data.frame(x = st$x, grp = st$grp)
                p <- ggplot2::ggplot(d, ggplot2::aes(x = x, fill = grp)) +
                    ggplot2::geom_histogram(bins = st$bins, colour = "white") +
                    ggplot2::labs(x = NULL, y = "Rows", fill = NULL)
            } else {
                d <- data.frame(label = factor(st$label, levels = rev(st$label)), n = st$n, grp = st$grp)
                p <- ggplot2::ggplot(d, ggplot2::aes(x = label, y = n, fill = grp)) +
                    ggplot2::geom_col(width = 0.7) +
                    ggplot2::coord_flip() +
                    ggplot2::labs(x = NULL, y = "Rows", fill = NULL)
            }
            p + ggplot2::scale_fill_manual(values = cols, drop = TRUE) + ggtheme +
                ggplot2::theme(legend.position = "bottom")
        },

        .plotMissing = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            b <- st$blank
            d <- data.frame(row = rep(seq_len(nrow(b)), ncol(b)),
                            var = factor(rep(st$vars, each = nrow(b)), levels = rev(st$vars)),
                            missing = factor(ifelse(as.vector(b), "Missing", "Present"), levels = c("Present", "Missing")))
            ggplot2::ggplot(d, ggplot2::aes(x = row, y = var, fill = missing)) +
                ggplot2::geom_raster() +
                ggplot2::scale_fill_manual(values = c(Present = "#E3EFEF", Missing = "#7A3F0C")) +
                ggplot2::labs(x = "Row", y = NULL, fill = NULL) + ggtheme +
                ggplot2::theme(legend.position = "bottom")
        },

        .plotAssoc = function(image, ggtheme, theme, ...) {
            m <- image$state
            if (is.null(m)) return(FALSE)
            d <- data.frame(a = factor(rep(rownames(m), ncol(m)), levels = rownames(m)),
                            b = factor(rep(colnames(m), each = nrow(m)), levels = rev(colnames(m))),
                            v = as.vector(m))
            ggplot2::ggplot(d, ggplot2::aes(x = a, y = b, fill = v)) +
                ggplot2::geom_tile(colour = "white") +
                ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(v), "", sprintf("%.2f", v))), size = 3) +
                ggplot2::scale_fill_gradient(low = "#FBFBF9", high = "#0E5A5F", limits = c(0, 1), na.value = "grey90") +
                ggplot2::labs(x = NULL, y = NULL, fill = "Strength") + ggtheme +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
        })
)

# chart data for one variable; NULL = no chart
chartState <- function(p, cl) {
    if (p$nValid == 0) return(NULL)
    changes <- cl$how != ""
    if (p$numericLike) {
        ok <- p$valid & !is.na(p$num)
        grp <- ifelse(p$sentinel[ok], "Missing code", ifelse(changes[ok], "Will change", "Unchanged"))
        return(list(type = "num", x = p$num[ok], grp = grp,
                    bins = min(30, max(5, length(unique(p$num[ok]))))))
    }
    lab <- ifelse(p$valid, ifelse(p$s != trimws(p$s), paste0("“", p$s, "”"), p$s), NA)
    d <- data.frame(label = lab[p$valid], ch = changes[p$valid], stringsAsFactors = FALSE)
    tab <- sort(table(d$label), decreasing = TRUE)
    top <- names(tab)[seq_len(min(12, length(tab)))]
    grp <- vapply(top, function(l) if (any(d$ch[d$label == l])) "Will change" else "Unchanged", "")
    n <- as.integer(tab[top])
    if (length(tab) > 12) {
        top <- c(top, paste0("other values (", length(tab) - 12, ")"))
        n <- c(n, sum(tab[-(1:12)]))
        grp <- c(grp, "Unchanged")
    }
    list(type = "cat", label = top, n = n, grp = unname(grp))
}
