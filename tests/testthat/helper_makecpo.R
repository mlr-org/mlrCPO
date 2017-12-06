

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
generalMakeCPO = function(name, train, retrafo, traininvert = NULL, invert = NULL,
  type = c("simple", "extended", "target", "target.extended", "retrafoless"),
  fr = FALSE, fi = FALSE, sl = FALSE, ci = FALSE, dataformat = "df.features",
  dataformat.factor.with.ordered = TRUE,
  convertfrom = "regr", convertto = convertfrom, properties.data = c("numerics", "factors", "ordered", "missings"),
  properties.adding = NULL, properties.needed = NULL, properties.target = NULL, predict.type.map = cpo.identity.predict.type.map)  {
  type = match.arg(type)
  istocpo = type %in% c("target", "target.extended")

  if (is.null(properties.target)) {
    if (istocpo) {
      properties.target = convertfrom
      if (convertfrom == "classif") {
        properties.target %c=% c("oneclass", "twoclass", "multiclass")
      }
    } else {
      properties.target = c(cpo.tasktypes, cpo.targetproperties)
    }
  }

  if (is.null(properties.adding)) {
    if (istocpo && convertfrom != convertto) {
      properties.adding = properties.target
    } else {
      properties.adding = character(0)
    }
  }

  if (is.null(properties.needed)) {
    if (istocpo && convertfrom != convertto) {
      properties.needed = convertto
      if (convertto == "classif") {
        properties.needed %c=% c("twoclass", "multiclass")
      }
    } else {
      properties.needed = character(0)
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

  reduceDataformat = function(data, target, whichfun) {
    # turn data into a df.all df
    if (dataformat %in% c("factor", "ordered", "numeric")) {
      dataformat = "df.features"
    }

    if (dataformat == "split") {
      emptytarget = data[[1]][character(0)]
    } else {
      emptytarget = data[character(0)]
    }

    if ("task" %in% class(data)) {
      assert(identical(target, getTaskTargetNames(data)))
    }

    if (whichfun == "invert") {
      assertNull(data)
      if (!is.data.frame(target)) {
        target = data.frame(target.converted = target)
      }
      return(list(data = data, target = target))
    }
    if (whichfun == "traininvert") {
      assertNull(target)
      target = emptytarget
      if (dataformat == "task" && class(data) %in% "Task") {
        data = getTaskData(data, target.extra = TRUE)$data
        dataformat = "df.features"
      } else if (dataformat == "df.all") {
        dataformat = "df.features"
      }
    }
    if (whichfun == "train") {
      if (!sl && type != "retrafoless") {
        assert(!is.null(target))
      } else if (type != "target" || is.null(target)) {
        assertNull(target)
        if (dataformat %in% c("task", "df.all")) {
          dataformat = "df.features"
          target = emptytarget
        }
      }
    }
    if (whichfun == "retrafo") {
      if (istocpo) {
        if (dataformat %in% c("df.all", "task")) {
          if (dataformat == "task") {
            assert(identical(data, getTaskData(target, target.extra = TRUE)$data))
            data = target
          } else {
            assertSubset(names(data), names(target))
            aux = names(data)
            data = target
            target = setdiff(names(data), aux)
          }
        }
      } else {
        if (dataformat == "task") {
          if ("task" %in% class(data)) {
            assert(!is.null(target))
            data = getTaskData(data, target.extra = TRUE)$data
          }
          dataformat = "df.features"
        } else if (dataformat == "df.all") {
          if (!is.null(target)) {
            data = dropNamed(data, target)
          }
          dataformat = "df.features"
        }
        target = emptytarget
      }
    }

    switch(dataformat,
      task = {
        target = getTaskData(data, features = character(0))
        data = getTaskData(data, target.extra = TRUE)$data
      },
      df.all = {
        assertSubset(target, colnames(data))
        target.names = target
        target = data[target.names]
        data = dropNamed(data, target.names)
      })
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
    if (whichfun == "traininvert") {
      target = NULL
    }

    if (!is.null(target)) {
      names(target) = pastePar("target", names(target), sep = ".")
      data = cbind(data, target)
      target = if (whichfun == "train" || (istocpo && whichfun == "retrafo")) names(target)
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
      assert(is.null(oldtarget) == !istocpo)
      if (is.null(oldtarget)) {
        # focpo, rocpo
        if (dataformat %in% c("df.all", "task")) {
          dataformat = "df.features"
        }
      } else {
        # tocpo
        if (dataformat == "df.all") {
          colnames(result) = gsub(paste0Par("^(numeric|ordered|factor|target)\\."), "", colnames(result))
          return(result)
        } else if (dataformat == "task") {
          target = grep("^target\\.", colnames(result), value = TRUE)
          target = gsub("^target\\.", "", target)
          colnames(result) = gsub("^(numeric|ordered|factor|target)\\.", "", colnames(result))
          if (convertfrom == convertto) {
            return(changeData(oldtarget, result))
          } else {
            return(constructTask(NULL, result, target, convertto, getTaskId(oldtarget)))
          }
        }
      }

      if (istocpo && !dataformat %in% c("df.all", "task")) {
        result = result[grepl("^target\\.", names(result))]
        names(result) = gsub("^target\\.", "", names(result))
        return(result)
      }
      assert(istocpo || dataformat %in% c("df.all", "task") || !any(grepl("^target\\.", names(result))))
      if (dataformat == "split") {
        result = sapply(c("numeric", "factor", if (!dataformat.factor.with.ordered) "ordered", "other"), function(x) {
          result = result[grepl(paste0Par("^", x, "\\."), names(result))]
          names(result) = gsub(paste0Par("^", x, "\\."), "", names(result))
          result
        })
      } else if (dataformat == "df.features") {
        result = result[grep("^target\\.", colnames(result), invert = !istocpo)]
        colnames(result) = gsub(paste0Par("^(numeric|ordered|factor|target)\\."), "", colnames(result))
      }
    } else {
      assert(whichfun == "invert")
      if (convertto != "multilabel" && predict.type == "response") {
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
      convdata = reduceDataformat(indata$data, indata$target, whichfun)
      for (pn in c("data", "target")) {
        indata[[pn]] = convdata[[pn]]
      }
      result = do.call(oldfun, indata)
      if (whichfun %in% c("retrafo", "invert")) {
        assert(whichfun == "retrafo" || !is.null(indata$predict.type))
        result = addDataformat(result, indata$data, indata$target, whichfun, indata$predict.type)
      }
      result
    }
  }

  for (fun in c("train", "retrafo", "traininvert", "invert")) {
    assign(fun, convertDF(get(fun), fun))
  }

  generate = function(...) {
    target = "NOTGIVEN"
    switch(type,
      retrafoless = makeCPORetrafoless(...,
        cpo.retrafo = {
          assert(target == "NOTGIVEN")
          retrafo(data = data, param = param,
            control = train(data = data, target = NULL, param = param))
        }),
      simple = {
        if (sl) {
          assert(!fr)
          makeCPO(..., cpo.train = NULL,
            cpo.retrafo = {
              assert(target == "NOTGIVEN")
              retrafo(data = data, param = param,
                control = train(data = data, target = NULL, param = param))
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
              if (dataformat == "df.all") {
                trg = data[target]
                rec = function(x) cbind(x, trg)
                data = dropNamed(data, target)
              } else {
                rec = identity
              }
              rec(cpo.retrafo(data))
            })
        } else {
          makeCPOExtendedTrafo(...,
            cpo.trafo = {
              control = train(data = data, target = target, param = param)
              if (dataformat == "df.all") {
                trg = data[target]
                rec = function(x) cbind(x, trg)
                data = dropNamed(data, target)
              } else {
                rec = identity
              }
              rec(retrafo(data = data, param = param, control = control))
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
                  if (dataformat == "df.all") {
                    data = dropNamed(data, target)
                  }
                  control.invert = traininvert(data = data, control = control, param = param)
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
                    data2 = data
                    target2 = target
                    if (dataformat == "df.all") {
                      data2 = target
                      target2 = setdiff(names(target), names(data))
                    } else if (dataformat == "task") {
                      data2 = target
                      target2 = getTaskTargetNames(target)
                    }
                    control = train(data = data2, target = target2, param = param)
                    retrafo(data = data, target = target, param = param, control = control)
                  },
                  cpo.invert = {
                    invert(target = target, predict.type = predict.type, control = NULL, param = param)
                  })
              } else {
                # oi, or, const invert
                makeCPOTargetOp(..., constant.invert = TRUE,
                  cpo.train.invert = NULL,
                  cpo.train = {
                    control = train(data = data, target = target, param = param)
                    if (dataformat == "df.all") {
                      data = dropNamed(data, target)
                    }
                    list(control = control,
                      control.invert = traininvert(data = data, control = control, param = param))
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
                data2 = data
                if (dataformat == "df.all") {
                  data2 = dropNamed(data, target)
                }
                control.invert = traininvert(data = data2, control = control, param = param)
                cpo.invert = function(target, predict.type) {
                  invert(target = target, predict.type = predict.type, control = control.invert)
                }
                cpo.retrafo = function(data, target) {
                  control.invert = traininvert(data = data, control = control, param = param)
                  cpo.invert = function(target, predict.type) {
                    invert(target = target, predict.type = predict.type, control = control.invert)
                  }
                  retrafo(data = data, target = target, param = param, control = control)
                }
                retrafo(data = data, target = target)
              })

          } else { # or
            # fi, or
            makeCPOExtendedTargetOp(...,
              constant.invert = ci,
              cpo.invert = NULL,
              cpo.trafo = {
                control = train(data = data, target = target, param = param)
                data2 = data
                if (dataformat == "df.all") {
                  data2 = dropNamed(data, target)
                }
                control.invert = traininvert(data = data2, control = control, param = param)
                cpo.invert = function(target, predict.type) {
                  invert(target = target, predict.type = predict.type, control = control.invert)
                }
                retrafo(data = data, target = target, param = param, control = control)
              },
              cpo.retrafo = {
                control.invert = traininvert(data = data, control = control, param = param)
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
                data2 = data
                if (dataformat == "df.all") {
                  data2 = dropNamed(data, target)
                }
                control.invert = traininvert(data = data2, control = control, param = param)
                cpo.retrafo = function(data, target) {
                  control.invert = traininvert(data = data, control = control, param = param)
                  retrafo(data = data, target = target, param = param, control = control)
                }
                cpo.retrafo(data = data, target = target)
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
                data2 = data
                if (dataformat == "df.all") {
                  data2 = dropNamed(data, target)
                }
                control.invert = traininvert(data = data2, control = control, param = param)
                retrafo(data = data, target = target, param = param, control = control)
              },
              cpo.retrafo = {
                control.invert = traininvert(data = data, control = control, param = param)
                retrafo(data = data, target = target, param = param, control = control)
              },
              cpo.invert = {
                invert(target = target, predict.type = predict.type, control = control.invert)
              })

          }  # if (fr)
        }  # if (or)
      })  # target.extended, switch
  }

  generate(cpo.name = name, par.set = ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered,
    properties.data = properties.data, properties.adding = properties.adding, properties.needed = properties.needed,
    properties.target = properties.target)
}
#  fr = FALSE, fi = FALSE, sl = FALSE, ci = FALSE


allowedGMC = rbind(
    list(type = "simple",          fr = FALSE, fi = FALSE, sl = FALSE, ci = FALSE),
    list(type = "simple",          fr = FALSE, fi = FALSE, sl = TRUE , ci = FALSE),
    list(type = "simple",          fr = TRUE , fi = FALSE, sl = FALSE, ci = FALSE),
    list(type = "extended",        fr = FALSE, fi = FALSE, sl = FALSE, ci = FALSE),
    list(type = "extended",        fr = TRUE , fi = FALSE, sl = FALSE, ci = FALSE),
    list(type = "target",          fr = FALSE, fi = FALSE, sl = FALSE, ci = FALSE),
    list(type = "target",          fr = TRUE , fi = FALSE, sl = FALSE, ci = FALSE),
    list(type = "target",          fr = FALSE, fi = TRUE , sl = FALSE, ci = FALSE),
    list(type = "target",          fr = TRUE , fi = TRUE , sl = FALSE, ci = FALSE),
    list(type = "target",          fr = FALSE, fi = FALSE, sl = TRUE , ci = TRUE ),
    list(type = "target",          fr = FALSE, fi = FALSE, sl = FALSE, ci = TRUE ),
    list(type = "target",          fr = TRUE , fi = FALSE, sl = FALSE, ci = TRUE ),
    list(type = "target.extended", fr = FALSE, fi = FALSE, sl = FALSE, ci = FALSE),
    list(type = "target.extended", fr = TRUE , fi = FALSE, sl = FALSE, ci = FALSE),
    list(type = "target.extended", fr = FALSE, fi = TRUE , sl = FALSE, ci = FALSE),
    list(type = "target.extended", fr = TRUE , fi = TRUE , sl = FALSE, ci = FALSE),
    list(type = "target.extended", fr = FALSE, fi = FALSE, sl = FALSE, ci = TRUE ),
    list(type = "target.extended", fr = TRUE , fi = FALSE, sl = FALSE, ci = TRUE ),
    list(type = "target.extended", fr = FALSE, fi = TRUE , sl = FALSE, ci = TRUE ),
    list(type = "target.extended", fr = TRUE , fi = TRUE , sl = FALSE, ci = TRUE ))
