
#' @title Split Numeric Features into Quantile Bins
#'
#' @template cpo_doc_intro
#'
#' @param numsplits [\code{numeric(1)}]\cr
#'   Number of bins to create. Default is \code{2}.
#'
#' @template cpo_doc_outro
#' @export
cpoQuantileBinNumerics = makeCPO("bin.numerics",  # nolint
  pSS(numsplits = 2: integer[2, ]),
  properties.needed = "ordered",
  properties.adding = "numerics",
  dataformat = "numeric",
  cpo.train = {
    lapply(data, function(d)
      unique(c(-Inf, quantile(d, (1:(numsplits - 1)) / numsplits, na.rm = TRUE), Inf)))
  },
  cpo.retrafo = {
    as.data.frame(mapply(function(d, b) ordered(cut(d, breaks = b)), d = data, b = control, SIMPLIFY = FALSE),
      row.names = rownames(data))
  })
registerCPO(cpoQuantileBinNumerics, "data", "feature conversion", "Convert Numerics to Ordered by binning.")
