

# train, retrafo, traininvert, invert:
# should all have parameter 'param',
# retrafo, traininvert, invert should have 'control'
# train, invert should have 'target'
# train, retrafo, traininvert should have 'data'
# invert should have 'predict.type'

# train: function(data, target, param) --> control
# retrafo: function(data, [target, ], control, param) -> data / target
# traininvert: function(data, control, param) -> control.invert
# invert: function(target, predict.type, control) -> target
# fr - functional retrafo
# fi - functional invert
# sl - stateless
# ci - constant invert
# sl and ci influence how often train & traininvert are called.
# dataformat is checked automatically
# properties.adding, properties.needed, properties.target are filled automatically if not given
# keepformat: whether to change column layout. TRUE -> keep input layout. FALSE -> emulate split layout
generalMakeCPO = function(name,
                          train = function(data, target, param) NULL,
                          retrafo = function(data, control, param, target) data,
                          traininvert = function(data, control, param) if (!missing(control)) control,
                          invert = function(target, predict.type, control, param) target,
  type = c("simple", "extended", "target", "target.extended", "retrafoless"),
  fr = FALSE, fi = FALSE, sl = FALSE, ci = FALSE, dataformat = "df.features",
  dataformat.factor.with.ordered = TRUE,
  convertfrom = "regr", convertto = convertfrom, properties.data = c("numerics", "factors", "ordered", "missings"),
  properties.adding = NULL, properties.needed = NULL, properties.target = NULL, predict.type.map = c(response = "response"),
  keepformat = TRUE, fix.factors = FALSE)  {
  type = match.arg(type)
  istocpo = type %in% c("target", "target.extended")

  if (is.null(properties.target)) {
    if (istocpo) {
      properties.target = convertfrom
      if (convertfrom == "classif") {
        properties.target %c=% c("twoclass", "multiclass")
      }
    } else {
      properties.target = c(cpo.tasktypes, cpo.targetproperties)
    }
  }

  if (is.null(properties.adding)) {
    properties.adding = character(0)
    if (istocpo && convertfrom != convertto) {
      if (convertfrom == "classif") {
        properties.adding %c=% c("twoclass", "multiclass")
      }
    }

  }

  if (is.null(properties.needed)) {
    properties.needed = character(0)
    if (istocpo && convertfrom != convertto) {
#      properties.needed = convertto
      if (convertto == "classif") {
        properties.needed %c=% c("twoclass", "multiclass")
      }
    }
  }

  pastePar = function(...) {
    pars = list(...)
    if (any(BBmisc::viapply(pars, length) == 0)) {
      character(0)
    } else {
      do.call(paste, pars)
    }
  }

  paste0Par = function(...) {
    pars = list(...)
    if (any(BBmisc::viapply(pars, length) == 0)) {
      character(0)
    } else {
      do.call(paste0, pars)
    }
  }

  ps = pSS(param = 1: integer[, ])

  reduceDataformat = function(data, target, whichfun, where) {
    indata = data

    if (whichfun == "invert") {
      assertNull(data)
      if (!is.data.frame(target)) {
        target = data.frame(target.converted = target)
      }
      return(list(data = data, target = target))
    }

    # turn data into a df.all df
    if (dataformat %in% c("factor", "ordered", "numeric")) {
      dataformat = "df.features"
    }

    if (dataformat == "split") {
      emptytarget = data[[1]][character(0)]
    } else if ("Task" %in% class(data)) {
      emptytarget = getTaskData(data)[character(0)]
    } else {
      emptytarget = data[character(0)]
    }

    if (dataformat == "task" && where == "trafo") {
      assert(identical(target, getTaskTargetNames(data)))
    }

    if (dataformat %in% c("df.all", "task")) {
      if (where == "retrafo" && istocpo && whichfun != "traininvert" && !is.null(target)) {
        if (dataformat == "task") {
          assert(identical(data, getTaskData(target, target.extra = TRUE)$data))
          data = target
          target = getTaskTargetNames(data)
        } else {
          assertSubset(names(data), names(target))
          aux = names(data)
          data = target
          target = setdiff(names(data), aux)
        }
        indata = data
        where = "trafo"
      }
      if (where == "trafo") {
        if (dataformat == "task") {
          target = getTaskData(data, features = character(0))
          data = getTaskData(data, target.extra = TRUE)$data
        } else {
          assertSubset(target, colnames(data))
          target.names = target
          target = data[target.names]
          data = dropNamed(data, target.names)
        }
      }
    }
    if (whichfun != "train" && (whichfun != "retrafo" || type %in% c("simple", "extended"))) {
      target = NULL
    }

    if (dataformat == "split") {
      data = sapply(names(data), function(x) {
        ret = data[[x]]
        names(ret) = pastePar(x, names(ret), sep = ".")
        ret
      }, simplify = FALSE)
    } else {
      coltypes = BBmisc::vcapply(data, function(x) class(x)[1])
      if (dataformat.factor.with.ordered) {
        coltypes[coltypes == "ordered"] = "factor"
      }
      coltypes[coltypes == "integer"] = "numeric"
      coltypes[!coltypes %in% c("numeric", "factor", "ordered")] = "other"
      names(coltypes) = NULL
      data = sapply(c("numeric", "factor", if (!dataformat.factor.with.ordered) "ordered", "other"), function(x) {
        data = data[coltypes == x]
        names(data) = pastePar(x, names(data), sep = ".")
        data
      }, simplify = FALSE)
    }
    data = do.call(cbind, unname(data))

    if (!is.null(target)) {
      names(target) = pastePar("target", names(target), sep = ".")
      data = cbind(data, target)

      target = if (whichfun == "train" || (!type %in% c("simple", "extended") && whichfun == "retrafo")) names(target)
    }
    order = seq_along(data)
    names(order) = gsub("^(numeric|ordered|factor|target|other)\\.", "", colnames(data))
    if ("Task" %in% class(indata)) {
      innames = colnames(getTaskData(indata))
    } else if (dataformat != "split") {
      innames = colnames(indata)
    }
    if (dataformat != "split" && keepformat) {
      innames %c=% setdiff(names(order), innames)
      data = data[order[intersect(innames, names(order))]]
    }

    list(data = data, target = target)
  }

  addDataformat = function(result, olddata, oldtarget, whichfun, predict.type) {
    # turn data back into original format
    # only for 'retrafo', 'invert'

    if (dataformat %in% c("factor", "ordered", "numeric")) {
      dataformat = "df.features"
    }

    if (whichfun == "retrafo") {

      if (!istocpo) {
        # focpo, rocpo
        if (dataformat %in% c("df.all", "task")) {
          if ("Task" %in% class(olddata)) {
            if (type == "retrafoless") {
              whichtarget = grep("^target\\.", names(result))
              colnames(result) = gsub("^(numeric|ordered|factor|target|other)\\.", "", colnames(result))
              tnames = colnames(result)[whichtarget]
              olddata$task.desc$target = tnames
              return(changeData(olddata, result))
            }
            colnames(result) = gsub("^(numeric|ordered|factor|target|other)\\.", "", colnames(result))
            if (all(getTaskFeatureNames(olddata) == colnames(result))) {
              newdata = getTaskData(olddata)
              newdata[colnames(result)] = result
            } else {
              newdata = cbind(getTaskData(olddata, features = character(0)), result)
            }
            return(changeData(olddata, newdata))
          } else if (is.character(oldtarget) && all(oldtarget %in% colnames(olddata)) && type != "retrafoless") {
            assert(!any(grepl("^target\\.", names(result))))
            colnames(result) = gsub("^(numeric|ordered|factor|target|other)\\.", "", colnames(result))
            if (all(colnames(result) == setdiff(colnames(olddata), oldtarget))) {
              olddata[!colnames(olddata) %in% oldtarget] = result
              return(olddata)
            } else {
              return(cbind(olddata[oldtarget], result))
            }
          }
          dataformat = "df.features"
        }
        assert(type == "retrafoless", !any(grepl("^target\\.", names(result))))
      } else {
        # tocpo
        if (is.null(oldtarget)) {
          # nothing to do
          return(NULL)
        }
        if (dataformat == "df.all") {
          colnames(result) = gsub(paste0Par("^(numeric|ordered|factor|target|other)\\."), "", colnames(result))
          return(result)
        } else if (dataformat == "task") {
          if ("Task" %in% class(olddata)) {
            oldtarget = olddata
          }
          target = grep("^target\\.", colnames(result), value = TRUE)
          target = gsub("^target\\.", "", target)
          colnames(result) = gsub("^(numeric|ordered|factor|target|other)\\.", "", colnames(result))
          if (convertfrom == convertto) {
            oldtask = oldtarget
          } else {
            oldtask = NULL
          }
          return(constructTask(result, target, convertto, getTaskId(oldtarget), convertfrom == "classif" && isLevelFlipped(oldtarget)))
        }
        if (!dataformat %in% c("df.all", "task")) {
          result = result[grepl("^target\\.", names(result))]
          names(result) = gsub("^target\\.", "", names(result))
          return(result)
        }
        assert(FALSE)
      }
      if (dataformat == "split") {
        result = sapply(c("numeric", "factor", if (!dataformat.factor.with.ordered) "ordered", "other"), function(x) {
          result = result[grepl(paste0Par("^", x, "\\."), names(result))]
          names(result) = gsub(paste0Par("^", x, "\\."), "", names(result))
          result
        })
      } else if (dataformat == "df.features") {
#        result = result[grep("^target\\.", colnames(result), invert = !istocpo)]
        colnames(result) = gsub(paste0Par("^(numeric|ordered|factor|target|other)\\."), "", colnames(result))
      }
    } else {
      assert(whichfun == "invert")
      if (convertfrom != "multilabel" && predict.type == "response") {
        assert(ncol(result) == 1)
        result = result[[1]]
      }
    }

    result
  }

  convertDF = function(oldfun, whichfun) {
    whichfun
    if (is.null(oldfun)) {
      return(NULL)
    }
    function(...) {
      indata = list(...)
      if (is.null(indata$where)) {
        where = switch(whichfun, train = "trafo", retrafo = "retrafo", traininvert = "traininvert")
      } else {
        where = indata$where
        indata$where = NULL
      }
      if (whichfun == "train" && where == "trafo") {
        tnames = if (dataformat %in% c("task", "df.all")) indata$target else colnames(indata$target)
      } else {
        tnames = NULL
      }
      if (whichfun %in% c("retrafo", "traininvert") && "control" %in% names(indata)) {
        if (is.null(indata$target) && (where == "trafo" || (where == "retrafo" && whichfun == "traininvert"))) {
          indata$target = indata$control$tnames
        }
        indata$control = indata$control$res
      }
      olddata = indata
      convdata = reduceDataformat(indata$data, indata$target, whichfun, where)
      for (pn in c("data", "target")) {
        indata[[pn]] = convdata[[pn]]
      }
      result = do.call(oldfun, indata)
      if (whichfun %in% c("retrafo", "invert")) {
        assert(whichfun == "retrafo" || !is.null(indata$predict.type))
        result = addDataformat(result, olddata$data, olddata$target, whichfun, indata$predict.type)
      } else if (whichfun == "train") {
        result = list(res = result, tnames = tnames)
      }
      result
    }
  }

  for (fun in c("train", "retrafo", "traininvert", "invert")) {
    assign(fun, convertDF(get(fun), fun))
  }

  generate = function(...) {
    target = "NOTGIVEN"
    control = "NOTGIVEN"
    control.invert = "NOTGIVEN"
    switch(type,
      retrafoless = makeCPORetrafoless(...,
        cpo.trafo = {
          retrafo(data = data, target = target, param = param,
            control = train(data = data, target = target, param = param), where = "trafo")
        }),
      simple = {
        if (sl) {
          assert(!fr)
          makeCPO(..., cpo.train = NULL,
            cpo.retrafo = {
              assert(target == "NOTGIVEN")
              retrafo(data = data, param = param,
                control = train(data = data, target = NULL, param = param, where = "retrafo"))
            })
        } else if (fr) {
          makeCPO(..., cpo.retrafo = NULL,
            cpo.train = {
              control = train(data = data, target = target, param = param)
              function(data) {
                retrafo(data = data, param = param, control = control)
              }
            })
        } else {
          makeCPO(...,
            cpo.train = {
              train(data = data, target = target, param = param)
            }, cpo.retrafo = {
              assert(target == "NOTGIVEN")
              retrafo(data = data, param = param, control = control)
            })
        }
      },
      extended = {
        if (fr) {
          makeCPOExtendedTrafo(..., cpo.retrafo = NULL,
            cpo.trafo = {
              control = train(data = data, target = target, param = param)
              cpo.retrafo = function(data) {
                retrafo(data = data, param = param, control = control)
              }
              retrafo(data = data, param = param, control = control, where = "trafo")
            })
        } else {
          makeCPOExtendedTrafo(...,
            cpo.trafo = {
              control = train(data = data, target = target, param = param)
              retrafo(data = data, param = param, control = control, where = "trafo")
            },
            cpo.retrafo = {
              assert(target == "NOTGIVEN")
              retrafo(data = data, param = param, control = control)
            })
        }
      },
      target = {
        if (fi) {
          assert(!ci)
          assert(!sl)
          if (fr) {
            # fi, fr
            makeCPOTargetOp(...,
              cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = NULL,
              cpo.train = {
                control = train(data = data, target = target, param = param)
                cpo.retrafo = function(data, target) {
                  retrafo(data = data, target = target, param = param, control = control)
                }
                cpo.train.invert = function(data) {
                  control.invert = traininvert(data = data, control = control, param = param)
                  function(target, predict.type) {
                    invert(target = target, predict.type = predict.type, control = control.invert, param = param)
                  }
                }
              })
          } else { # or
            # fi, or
            makeCPOTargetOp(...,
              cpo.invert = NULL,
              cpo.train = {
                train(data = data, target = target, param = param)
              },
              cpo.retrafo = {
                retrafo(data = data, target = target, param = param, control = control)
              },
              cpo.train.invert = {
                control.invert = traininvert(data = data, control = control, param = param)
                function(target, predict.type) {
                  invert(target = target, predict.type = predict.type, control = control.invert, param = param)
                }
              })
          }
        } else { # oi
          if (fr) {
            assert(!sl)
            if (ci) {
              # (oi), fr, constant invert
              makeCPOTargetOp(..., constant.invert = TRUE,
                cpo.train.invert = NULL, cpo.invert = NULL, cpo.retrafo = NULL,
                cpo.train = {
                  control = train(data = data, target = target, param = param)
                  cpo.retrafo = function(data, target) {
                    retrafo(data = data, target = target, param = param, control = control)
                  }
                  control.invert = traininvert(data = data, control = control, param = param, where = "trafo")
                  cpo.invert = function(target, predict.type) {
                    invert(target = target, predict.type = predict.type, control = control.invert, param = param)
                  }
                })
            } else {
              # oi, fr
              makeCPOTargetOp(...,
                cpo.retrafo = NULL, cpo.train.invert = NULL,
                cpo.train = {
                  control = train(data = data, target = target, param = param)
                  cpo.retrafo = function(data, target) {
                    retrafo(data = data, target = target, param = param, control = control)
                  }
                  cpo.train.invert = function(data) {
                    traininvert(data = data, control = control, param = param)
                  }
                },
                cpo.invert = {
                  invert(target = target, predict.type = predict.type, control = control.invert, param = param)
                })
            }
          } else {  # or
            if (ci) {
              if (sl) {
                # oi, or, const invert, stateless
                makeCPOTargetOp(..., constant.invert = TRUE,
                  cpo.train = NULL, cpo.train.invert = NULL,
                  cpo.retrafo = {
                    control = train(data = data, target = target, param = param, where = "retrafo")
                    retrafo(data = data, target = target, param = param, control = control)
                  },
                  cpo.invert = {
                    assert(control == "NOTGIVEN")
                    assert(control.invert == "NOTGIVEN")
                    invert(target = target, predict.type = predict.type, control = NULL, param = param)
                  })
              } else {
                # oi, or, const invert
                makeCPOTargetOp(..., constant.invert = TRUE,
                  cpo.train.invert = NULL,
                  cpo.train = {
                    control = train(data = data, target = target, param = param)
                    list(control = control,
                      control.invert = traininvert(data = data, control = control, param = param, where = "trafo"))
                  },
                  cpo.retrafo = {
                    retrafo(data = data, target = target, param = param, control = control$control)
                  },
                  cpo.invert = {
                    invert(target = target, predict.type = predict.type, control = control.invert$control.invert, param = param)
                  })
              }
            } else {
              assert(!sl)
              # oi, or
              makeCPOTargetOp(...,
                cpo.train = {
                  train(data = data, target = target, param = param)
                },
                cpo.train.invert = {
                  traininvert(data = data, control = control, param = param)
                },
                cpo.retrafo = {
                  retrafo(data = data, target = target, param = param, control = control)
                },
                cpo.invert = {
                  invert(target = target, predict.type = predict.type, control = control.invert, param = param)
                })
            }
          }
        }
      },
      target.extended = {
        if (fi) {
          if (fr) {
            # fi, fr
            makeCPOExtendedTargetOp(...,
              constant.invert = ci,
              cpo.retrafo = NULL, cpo.invert = NULL,
              cpo.trafo = {
                control = train(data = data, target = target, param = param)
                control.invert = traininvert(data = data, control = control, param = param, where = "trafo")
                cpo.invert = function(target, predict.type) {
                  invert(target = target, predict.type = predict.type, control = control.invert)
                }
                cpo.retrafo = function(data, target) {
                  control.invert = traininvert(data = data, control = control, param = param, where = "retrafo")
                  cpo.invert = function(target, predict.type) {
                    invert(target = target, predict.type = predict.type, control = control.invert)
                  }
                  retrafo(data = data, target = target, param = param, control = control)
                }
                retrafo(data = data, target = target, param = param, control = control, where = "trafo")
              })

          } else { # or
            # fi, or
            makeCPOExtendedTargetOp(...,
              constant.invert = ci,
              cpo.invert = NULL,
              cpo.trafo = {
                control = train(data = data, target = target, param = param)
                control.invert = traininvert(data = data, control = control, param = param, where = "trafo")
                cpo.invert = function(target, predict.type) {
                  invert(target = target, predict.type = predict.type, control = control.invert)
                }
                retrafo(data = data, target = target, param = param, control = control, where = "trafo")
              },
              cpo.retrafo = {
                control.invert = traininvert(data = data, control = control, param = param, where = "retrafo")
                cpo.invert = function(target, predict.type) {
                  invert(target = target, predict.type = predict.type, control = control.invert)
                }
                retrafo(data = data, target = target, param = param, control = control)
              })
          }
        } else {  # oi
          if (fr) {
            # oi, fr
            makeCPOExtendedTargetOp(...,
              constant.invert = ci,
              cpo.retrafo = NULL,
              cpo.trafo = {
                control = train(data = data, target = target, param = param)
                control.invert = traininvert(data = data, control = control, param = param, where = "trafo")
                cpo.retrafo = function(data, target) {
                  control.invert = traininvert(data = data, control = control, param = param, where = "retrafo")
                  retrafo(data = data, target = target, param = param, control = control)
                }
                retrafo(data = data, target = target, param = param, control = control, where = "trafo")
              },
              cpo.invert = {
                invert(target = target, predict.type = predict.type, control = control.invert)
              })
          } else { # or
            # oi, or
            makeCPOExtendedTargetOp(...,
              constant.invert = ci,
              cpo.trafo = {
                control = train(data = data, target = target, param = param)
                control.invert = traininvert(data = data, control = control, param = param, where = "trafo")
                retrafo(data = data, target = target, param = param, control = control, where = "trafo")
              },
              cpo.retrafo = {
                control.invert = traininvert(data = data, control = control, param = param, where = "retrafo")
                retrafo(data = data, target = target, param = param, control = control)
              },
              cpo.invert = {
                invert(target = target, predict.type = predict.type, control = control.invert)
              })

          }  # if (fr)
        }  # if (or)
      })  # target.extended, switch
  }

  args = list(cpo.name = name, par.set = ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered,
    properties.data = properties.data, properties.adding = properties.adding, properties.needed = properties.needed,
    properties.target = properties.target, fix.factors = fix.factors)

  if (type %in% c("target.extended", "target")) {
    args %c=% list(task.type.out = convertto, predict.type.map = predict.type.map)
  }
  do.call(generate, args)
}
#  fr = FALSE, fi = FALSE, sl = FALSE, ci = FALSE


allowedGMC = rbind(  # nolint
    list(type = "simple",          fr = FALSE, fi = FALSE, sl = FALSE, ci = FALSE),
    list(type = "simple",          fr = FALSE, fi = FALSE, sl = TRUE,  ci = FALSE),
    list(type = "simple",          fr = TRUE,  fi = FALSE, sl = FALSE, ci = FALSE),
    list(type = "extended",        fr = FALSE, fi = FALSE, sl = FALSE, ci = FALSE),
    list(type = "extended",        fr = TRUE,  fi = FALSE, sl = FALSE, ci = FALSE),
    list(type = "retrafoless",     fr = FALSE, fi = FALSE, sl = FALSE, ci = FALSE),
    list(type = "target",          fr = FALSE, fi = FALSE, sl = FALSE, ci = FALSE),
    list(type = "target",          fr = TRUE,  fi = FALSE, sl = FALSE, ci = FALSE),
    list(type = "target",          fr = FALSE, fi = TRUE,  sl = FALSE, ci = FALSE),
    list(type = "target",          fr = TRUE,  fi = TRUE,  sl = FALSE, ci = FALSE),
    list(type = "target",          fr = FALSE, fi = FALSE, sl = TRUE,  ci = TRUE),
    list(type = "target",          fr = FALSE, fi = FALSE, sl = FALSE, ci = TRUE),
    list(type = "target",          fr = TRUE,  fi = FALSE, sl = FALSE, ci = TRUE),
    list(type = "target.extended", fr = FALSE, fi = FALSE, sl = FALSE, ci = FALSE),
    list(type = "target.extended", fr = TRUE,  fi = FALSE, sl = FALSE, ci = FALSE),
    list(type = "target.extended", fr = FALSE, fi = TRUE,  sl = FALSE, ci = FALSE),
    list(type = "target.extended", fr = TRUE,  fi = TRUE,  sl = FALSE, ci = FALSE),
    list(type = "target.extended", fr = FALSE, fi = FALSE, sl = FALSE, ci = TRUE),
    list(type = "target.extended", fr = TRUE,  fi = FALSE, sl = FALSE, ci = TRUE),
    list(type = "target.extended", fr = FALSE, fi = TRUE,  sl = FALSE, ci = TRUE),
    list(type = "target.extended", fr = TRUE,  fi = TRUE,  sl = FALSE, ci = TRUE))


applyGMC = function(name, strict,
                    fr = NULL, fi = NULL, sl = NULL, ci = NULL,
                    type = c("target", "target.extended", "simple", "extended", "retrafoless"),
                    dataformats = c("df.features", "split", "df.all", "task"),
                    convertfrom = "cluster", convertto = convertfrom,
                    train = function(data, target, param) NULL,
                    retrafo = function(data, control, param, target) data,
                    traininvert = function(data, control, param)  if (!missing(control)) control,
                    invert = function(target, predict.type, control, param) target,
                    keepformat = TRUE, fix.factors = FALSE,
                    properties.needed = NULL, properties.adding = NULL, properties.target = NULL,
                    applyfun) {
  dotype = type
  assertSubset(type, c("target", "target.extended", "simple", "extended", "retrafoless"))
  for (dfx in dataformats) {
    for (lineno in seq_len(nrow(allowedGMC))) {
      line = allowedGMC[lineno, ]
      if ((!is.null(fr) && fr != line$fr) ||
          (!is.null(fi) && fi != line$fi) ||
          (!is.null(sl) && sl != line$sl) ||
          (!is.null(ci) && ci != line$ci) ||
          (line$type == "retrafoless" && !dfx %in% c("df.all", "task")) ||
          (!line$type %in% dotype)){
        next
      }
      type = line$type

      cpo = generalMakeCPO(name = name, type = type, keepformat = keepformat,
        fr = line$fr, fi = line$fi, sl = line$sl, ci = line$ci,
        dataformat = dfx, convertfrom = convertfrom,
        convertto = convertto, fix.factors = fix.factors,
        dataformat.factor.with.ordered = !strict,
        train = train, retrafo = retrafo, traininvert = traininvert,
        invert = invert,
        properties.needed = properties.needed,
        properties.adding = properties.adding,
        properties.target = properties.target)
      applyfun(cpo, type, line, dfx)
    }
  }
}
