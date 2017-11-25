
#' @title Split Numeric Features into Quantile Bins
#'
#' @template cpo_description
#'
#' @param numsplits [\code{numeric(1)}]\cr
#'   Number of bins to create. Default is \code{2}.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoQuantileBinNumerics = makeCPOExtended("bin.numerics", numsplits = 2: integer[2, ],  # nolint
  .properties.needed = "ordered", .properties.adding = "numerics",
  .dataformat = "numeric", cpo.trafo = {
    breaks = lapply(data, function(d)
      unique(c(-Inf, quantile(d, (1:(numsplits - 1)) / numsplits, na.rm = TRUE), Inf)))
    cpo.retrafo = function(data) {
      as.data.frame(mapply(function(d, b) ordered(cut(d, breaks = b)), d = data, b = breaks, SIMPLIFY = FALSE),
        row.names = rownames(data))
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
registerCPO(cpoCollapseFact, "data", "feature conversion", "Convert Numerics to Ordered by binning.")