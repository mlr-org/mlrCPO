#' @title Probability Encoding
#'
#' @description
#' TODO: what is this actually called?
#'
#' Converts factor columns into columns giving the probability
#' for each target class to have this target, given the column value.
#'
#' @template cpo_description
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoProbEncode = makeCPOExtended("prob.encode",  # nolint
  .dataformat = "factor",
  .properties.adding = c("factors", "ordered"),
  .properties.needed = "numerics",
  .properties.target = c("twoclass", "multiclass", "classif"),
  .fix.factors = TRUE,
  cpo.trafo = {
    probs = lapply(data, function(col)
      sapply(levels(target[[1]]), function(tl)
        sapply(c(levels(col), NA), function(cl)
          mean(target[[1]][is.na(cl) | col == cl] == tl))))

    cpo.retrafo = function(data) {
      ret = do.call(cbind, lapply(seq_along(data), function(idx) {
        curdat = data[[idx]]
        levels(curdat) = c(levels(curdat), ".TEMP.MISSING")
        curdat[is.na(curdat)] = ".TEMP.MISSING"
        dummyenc = sapply(levels(curdat), function(lvl) as.integer(curdat == lvl))
        dummyenc %*% probs[[idx]]
      }))
      as.data.frame(ret)
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
registerCPO(cpoProbEncode, "data", "feature conversion", "Convert factorial columns in classification tasks to numeric columns by probability encoding them")

#' @title Impact Encoding
#'
#' @description
#' Impact encoding as done by vtreat
#'
#' @template cpo_description
#'
#' @param smoothing [\code{numeric(1)}]\cr
#'   A finite positive value used for smoothing.
#'   Default is \code{1e-4}.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoImpactEncodeClassif = makeCPOExtended("impact.encode.classif",  # nolint
  smoothing = 1e-4: numeric[~0, ~.],
  .dataformat = "factor",
  .properties.adding = c("factors", "ordered"),
  .properties.needed = "numerics",
  .properties.target = c("twoclass", "multiclass", "classif"),
  .fix.factors = TRUE,
  cpo.trafo = {
    probs = lapply(data, function(col)
      sapply(levels(target[[1]]), function(tl) {
        tprop = mean(target[[1]] == tl)
        tplogit = log(tprop / (1 - tprop))
        sapply(c(levels(col), NA), function(cl) {
          condprob = (sum(target[[1]][is.na(cl) | col == cl] == tl) + smoothing) / (sum(is.na(cl) | col == cl) + 2 * smoothing)
          cplogit = log(condprob / (1 - condprob))
          cplogit - tplogit
        })
      }))

    cpo.retrafo = function(data) {
      ret = do.call(cbind, lapply(seq_along(data), function(idx) {
        curdat = data[[idx]]
        levels(curdat) = c(levels(curdat), ".TEMP.MISSING")
        curdat[is.na(curdat)] = ".TEMP.MISSING"
        dummyenc = sapply(levels(curdat), function(lvl) as.integer(curdat == lvl))
        dummyenc %*% probs[[idx]]
      }))
      as.data.frame(ret)
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
registerCPO(cpoImpactEncodeClassif, "data", "feature conversion", "Convert factorial columns in classification tasks to numeric columns by impact encoding them")


#' @title Impact Encoding
#'
#' @description
#' Impact encoding as done by vtreat
#'
#' @template cpo_description
#'
#' @param smoothing [\code{numeric(1)}]\cr
#'   A finite positive value used for smoothing.
#'   Default is \code{1e-4}.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoImpactEncodeRegr = makeCPOExtended("impact.encode.regr",  # nolint
  smoothing = 1e-4: numeric[~0, ~.],
  .dataformat = "factor",
  .properties.adding = c("factors", "ordered"),
  .properties.needed = "numerics",
  .properties.target = "regr",  # TODO: multiclass?
  .fix.factors = TRUE,
  cpo.trafo = {
    meanimp = mean(target[[1]])
    impact = lapply(data, function(col)
      c(sapply(levels(col), function(lvl) {
        (sum(target[[1]][col == lvl]) + smoothing * meanimp) / (sum(col == lvl) + smoothing) - meanimp
      }), .TEMP.MISSING = meanimp))
    cpo.retrafo = function(data) {
      ret = do.call(cbind, lapply(seq_along(data), function(idx) {
        curdat = data[[idx]]
        levels(curdat) = c(levels(curdat), ".TEMP.MISSING")
        curdat[is.na(curdat)] = ".TEMP.MISSING"
        impact[[idx]][curdat]
      }))
      colnames(ret) = colnames(data)
      rownames(ret) = rownames(data)
      as.data.frame(ret)
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
registerCPO(cpoImpactEncodeRegr, "data", "feature conversion", "Convert factorial columns in regression tasks to numeric columns by impact encoding them")



#' @title CPO Dummy Encoder
#'
#' @template cpo_description
#'
#' @param reference.cat [\code{logical}]\cr
#'   If \dQuote{reference.cat} is \code{TRUE}, the first level of every factor column
#'   is taken as the reference category and the encoding is \code{c(0, 0, 0, ...)}.
#'   If this is \code{FALSE}, the encoding is always one-hot-encoding. Default is \code{FALSE}.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoDummyEncode = makeCPOExtended("dummyencode", reference.cat = FALSE: logical, .dataformat = "df.features",  # nolint
  .properties.needed = "numerics", .properties.adding = c("factors", "ordered"),
  cpo.trafo = {
    lvls = lapply(data, levels)

    cpo.retrafo = function(data) {
      datas = lapply(names(data), function(d) {
        if (is.factor(data[[d]])) {
          df = do.call(data.frame, lapply(lvls[[d]], function(l)
            as.integer(data[[d]] == l)))
          names(df) = paste0(d, lvls[[d]])
          if (reference.cat) {
            # check that no unknown factors occurred
            cleand = data[[d]][!is.na(data[[d]])]
            if (!all(cleand %in% lvls[[d]])) {
              stopf("dummyencode retrafo encountered factor level that wasn't seen before. Try using cpoFixFactors.")
            }
            df[-1]
          } else {
            # unseen factors are ok here.
            df
          }
        } else {
          data[d]
        }
      })
      do.call(cbind, datas)
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
registerCPO(cpoDummyEncode, "data", "feature conversion", "Convert factorial columns to numeric columns by dummy encoding them")
