#' @title Probability Encoding
#'
#' @template cpo_doc_intro
#'
#' @description
#' TODO: what is this actually called?
#'
#' Converts factor columns into columns giving the probability
#' for each target class to have this target, given the column value.
#'
#' @template cpo_doc_outro
#' @export
cpoProbEncode = makeCPO("prob.encode",  # nolint
  dataformat = "factor",
  properties.adding = c("factors", "ordered"),
  properties.needed = "numerics",
  properties.target = c("twoclass", "multiclass", "classif"),
  fix.factors = TRUE,
  cpo.train = {
    lapply(data, function(col)
      sapply(levels(target[[1]]), function(tl)
        vnapply(c(levels(col), NA), function(cl)
          mean(target[[1]][is.na(cl) | col == cl] == tl))))
  },
  cpo.retrafo = {
    ret = do.call(cbind, lapply(seq_along(data), function(idx) {
      curdat = data[[idx]]
      levels(curdat) = c(levels(curdat), ".TEMP.MISSING")
      curdat[is.na(curdat)] = ".TEMP.MISSING"
      dummyenc = sapply(levels(curdat), function(lvl) as.integer(curdat == lvl))
      dummyenc %*% control[[idx]]
    }))
    as.data.frame(ret)
  })
registerCPO(cpoProbEncode, "data", "feature conversion", "Convert factorial columns in classification tasks to numeric columns by probability encoding them")

#' @title Impact Encoding
#'
#' @template cpo_doc_intro
#'
#' @description
#' Impact encoding as done by vtreat
#'
#' @param smoothing [\code{numeric(1)}]\cr
#'   A finite positive value used for smoothing.
#'   Default is \code{1e-4}.
#'
#' @template cpo_doc_outro
#' @export
cpoImpactEncodeClassif = makeCPO("impact.encode.classif",  # nolint
  pSS(smoothing = 1e-4: numeric[~0, ~.]),
  dataformat = "factor",
  properties.adding = c("factors", "ordered"),
  properties.needed = "numerics",
  properties.target = c("twoclass", "multiclass", "classif"),
  fix.factors = TRUE,
  cpo.train = {
    lapply(data, function(col)
      sapply(levels(target[[1]]), function(tl) {
        tprop = mean(target[[1]] == tl)
        tplogit = log(tprop / (1 - tprop))
        vnapply(c(levels(col), NA), function(cl) {
          condprob = (sum(target[[1]][is.na(cl) | col == cl] == tl) + smoothing) / (sum(is.na(cl) | col == cl) + 2 * smoothing)
          cplogit = log(condprob / (1 - condprob))
          cplogit - tplogit
        })
      }))
  },
  cpo.retrafo = {
    ret = do.call(cbind, lapply(seq_along(data), function(idx) {
      curdat = data[[idx]]
      levels(curdat) = c(levels(curdat), ".TEMP.MISSING")
      curdat[is.na(curdat)] = ".TEMP.MISSING"
      dummyenc = sapply(levels(curdat), function(lvl) as.integer(curdat == lvl))
      dummyenc %*% control[[idx]]
    }))
    as.data.frame(ret)
  })
registerCPO(cpoImpactEncodeClassif, "data", "feature conversion", "Convert factorial columns in classification tasks to numeric columns by impact encoding them")


#' @title Impact Encoding
#'
#' @template cpo_doc_intro
#'
#' @description
#' Impact encoding as done by vtreat
#'
#' @param smoothing [\code{numeric(1)}]\cr
#'   A finite positive value used for smoothing.
#'   Default is \code{1e-4}.
#' @template cpo_doc_outro
#' @export
cpoImpactEncodeRegr = makeCPO("impact.encode.regr",  # nolint
  pSS(smoothing = 1e-4: numeric[~0, ~.]),
  dataformat = "factor",
  properties.adding = c("factors", "ordered"),
  properties.needed = "numerics",
  properties.target = "regr",  # TODO: multiclass?
  fix.factors = TRUE,
  cpo.train = {
    meanimp = mean(target[[1]])
    lapply(data, function(col)
      c(vnapply(levels(col), function(lvl) {
        (sum(target[[1]][col == lvl]) + smoothing * meanimp) / (sum(col == lvl) + smoothing) - meanimp
      }), .TEMP.MISSING = meanimp))
  }, cpo.retrafo = {
    ret = do.call(cbind, lapply(seq_along(data), function(idx) {
      curdat = data[[idx]]
      levels(curdat) = c(levels(curdat), ".TEMP.MISSING")
      curdat[is.na(curdat)] = ".TEMP.MISSING"
      control[[idx]][curdat]
    }))
    colnames(ret) = colnames(data)
    rownames(ret) = rownames(data)
    as.data.frame(ret)
  })
registerCPO(cpoImpactEncodeRegr, "data", "feature conversion", "Convert factorial columns in regression tasks to numeric columns by impact encoding them")



#' @title CPO Dummy Encoder
#'
#' @template cpo_doc_intro
#'
#' @param reference.cat [\code{logical}]\cr
#'   If \dQuote{reference.cat} is \code{TRUE}, the first level of every factor column
#'   is taken as the reference category and the encoding is \code{c(0, 0, 0, ...)}.
#'   If this is \code{FALSE}, the encoding is always one-hot-encoding. Default is \code{FALSE}.
#' @template cpo_doc_outro
#' @export
cpoDummyEncode = makeCPO("dummyencode",  # nolint
  pSS(reference.cat = FALSE: logical),
  dataformat = "df.features",
  properties.needed = "numerics",
  properties.adding = c("factors", "ordered"),
  cpo.train = {
    lapply(data, levels)
  },
  cpo.retrafo = {
    lvls = control
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
  })
registerCPO(cpoDummyEncode, "data", "feature conversion", "Convert factorial columns to numeric columns by dummy encoding them")
