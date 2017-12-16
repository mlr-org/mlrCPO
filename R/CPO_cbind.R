#' @include CPO_meta.R operators.R NULLCPO.R

#' @title \dQuote{cbind} the Result of Multiple CPOs
#'
#' @template cpo_doc_intro
#'
#' @description
#' Build a \code{\link{CPO}} that represents the operations of its input parameters,
#' performed in parallel and put together column wise.
#'
#' For example, to construct a \code{\link[mlr]{Task}} that contains the original
#' data, as well as the data after scaling, one could do
#'
#' \code{task \%>>\% cpoCbind(NULLCPO, cpoScale())}
#'
#' The result of \code{cpoCbind} is itself a CPO which exports its constituents'
#' hyperparameters. CPOs with the same type / ID get combined automatically.
#' To get networks, e.g. of the form
#' \preformatted{
#'       ,-C--E-.
#'      /    /   \
#' A---B----D-----F---G
#' }
#' one coul use the code
#' \preformatted{
#' initcpo = A \%>>\% B
#' route1 = initcpo \%>>\% D
#' route2 = cpoCbind(route1, initcpo \%>>\% C) \%>>\% E
#' result = cpoCbind(route1, route2) \%>>\% F \%>>\% G
#' }
#'
#' \code{cpoCbind} finds common paths among its arguments and combines them into one operation.
#' This saves computation and makes it possible for one exported hyperparameter to
#' influence multiple of \code{cpoCbind}'s inputs. However, if you want to use the same
#' operation with different parameters on different parts of \code{cpoCbind} input,
#' you must give these operations different IDs. If CPOs that could represent an identical CPO,
#' with the same IDs (or both with IDs absent) but different parameter settings, \code{affect.*} settings
#' or different parameter exportations occur, an error will be thrown.
#'
#' @param ... [\code{\link{CPO}}]\cr
#'   The \code{\link{CPO}}s to cbind. These must be Feature Operation CPOs.
#'   Named arguments will result in the respective
#'   columns being prefixed with the name. This is highly recommended if there
#'   is any chance of name collision otherwise. it is possible to use the same
#'   name multiple times (provided the resulting column names don't clash).
#' @param .cpos [\code{list} of \code{\link{CPO}}]\cr
#'   Alternatively, give the CPOs to cbind as a list. Default is \code{list()}.
#' @template cpo_doc_outro
#' @family special CPOs
#' @export
cpoCbind = function(..., .cpos = list()) {
  cpos = c(list(...), .cpos)

  assertList(cpos, types = "CPO", any.missing = FALSE)

  props = collectProperties(cpos, "cbind")
  props.creator = propertiesToMakeCPOProperties(props, "feature")

  # the graph representing the operations being performed; see makeCPOGraphItem for more info.
  # In short:
  # types: SOURCE, CPO, CBIND. SOURCE has no parents, CPO has only one parent.
  # 'parents', 'children' identify other items by their index in the 'cpograph' list
  # items in graph are sorted so that parents always come before children, with the
  # "SOURCE" item at position 1.
  cpograph = list(makeCPOGraphItem(type = "SOURCE", parents = integer(0), children = integer(0), content = NULL))

  # iterate through the single elements of 'cpos' and insert them in the graph one by one
  dangling = setNames(integer(0), character(0))  # "loose ends" in the graph
  for (cidx in seq_along(cpos)) {
    cname = firstNonNull(names(cpos)[cidx], "")
    newcpos = as.list(cpos[[cidx]])  # split into list of primitive CPOs
    curparent = 1L
    for (cpoprim in newcpos) {
      if ("CPOCbind" %in% class(cpoprim)) {
        # the element was a CPOCbind itself, so we insert its whole graph
        cpograph = uniteGraph(cpograph, curparent, cpoprim$unexported.pars[[".CPO"]], getHyperPars(cpoprim))
      } else {
        # any other element just gets a new element added to the cpograph
        cpograph %c=% list(makeCPOGraphItem(type = "CPO", parents = curparent, children = integer(0), content = cpoprim))
        cpograph[[curparent]]$children %c=% length(cpograph)
      }
      curparent = length(cpograph)
    }
    names(curparent) = cname
    dangling %c=% curparent
  }
  # the last element of the graph is the cbind element that ties all loose ends together
  cpograph %c=% list(makeCPOGraphItem(type = "CBIND", parents = unname(dangling), children = integer(0), content = names(dangling)))
  # the last element is therefore a child of all loose ends
  for (d in dangling) {
    cpograph[[d]]$children %c=% length(cpograph)
  }

  # check that if there is the same input (usually through NULLCPO), the names are different
  dupparents = names2(dangling)[dangling %in% dangling[duplicated(dangling)]]
  if (length({dupnames = unique(dupparents[duplicated(dupparents)])})) {
    stopf("Duplicating inputs must always have different names, but there are duplicated entries %s.",
      ifelse(all(is.na(dupnames)), "which are unnamed", collapse(Filter(function(x) !is.na(x), dupnames), sep = ", ")))
  }

  if (length(cpos)) {
    cpograph = synchronizeGraph(cpograph)
  }

  cgraphs = cascadeGraph(cpograph)

  pipeCPO(lapply(cgraphs, function(graph) {
    assert(graph[[1]]$type == "SOURCE")
    if (graph[[length(graph)]]$type != "CBIND") {
      # The graph is only a single CPO
      assert(length(graph) == 2)
      assert(identical(graph[[2]]$parents, 1L))
      assertClass(graph[[2]]$content, "CPO")
      return(graph[[2]]$content)
    }

    allcpos = extractSubList(Filter(function(x) x$type == "CPO", graph), "content", simplify = FALSE)

    if (length({badtype = setdiff(unique(unlist(lapply(allcpos, getCPOOperatingType))), "feature")})) {
      stopf("cpoCbind can only handle Feature Operation CPOs, but found CPOs with Operating Types '%s'",
        collapse(badtype, "', '"))
    }

    par.set = c(makeParamSet(), do.call(base::c, lapply(allcpos, getParamSet)))
    par.vals = do.call(base::c, lapply(allcpos, getHyperPars))
    export.params = getParamIds(par.set)  # don't export the .CPO
    par.set %c=% makeParamSet(makeUntypedLearnerParam(".CPO"))
    par.vals %c=% list(.CPO = graph)

    control = NULL  # pacify static code analyser

    addClasses(makeCPOExtendedTrafo("cbind", par.set, par.vals, dataformat = "task",
      properties.data = props.creator$properties.data,
      export.params = export.params,
      properties.adding = props.creator$properties.adding,
      properties.needed = props.creator$properties.needed,
      properties.target = props.creator$properties.target,
      cpo.trafo = function(data, target, .CPO, ...) {
        args = list(...)
        ag = applyGraph(.CPO, data, TRUE, args)
        control = ag$graph
        ag$data
      }, cpo.retrafo = function(data, control, ...) {
        applyGraph(control, data, FALSE, NULL)$data
      })(id = NULL), "CPOCbind")
  }))
}

# Iterate through the graph objects in order of dependency, apply each CPO to the data.
applyGraph = function(graph, data, is.trafo, args) {
  graph[[1]]$data[[1]] = data
  for (n in seq_along(graph)) {
    curgi = graph[[n]]
    outdata = switch(curgi$type,
      SOURCE = curgi$data[[1]],
      CPO = {
        assert(length(curgi$data) == 1)
        if (is.trafo) {
          ps = getParamSet(curgi$content)
          curcpo = setHyperPars(curgi$content, par.vals = subsetParams(args, ps))
        } else {
          curcpo = curgi$content
        }
        trafod = curgi$data[[1]] %>>% curcpo
        if (is.trafo) {
          curgi$content = retrafo(trafod)
          retrafo(trafod) = NULL
        }
        trafod
      },
      CBIND = {
        datas = curgi$data
        if (is.trafo) {
          datas = lapply(datas, function(x) getTaskData(x, target.extra = TRUE)$data)
        }
        assert(length(datas) == length(curgi$content))
        assert(!any(vlapply(datas, is.null)))
        datas = lapply(seq_along(datas), function(i) {
          df = data.frame(datas[[i]])
          prefix = curgi$content[i]
          if (!is.null(prefix) && prefix != "" && length(df)) {
            names(df) = paste(prefix, names(df), sep = ".")
          }
          df
        })
        if (is.trafo) {
          datas = c(list(getTaskData(data, features = character(0))), datas)
        }
        cnames = unlist(lapply(datas, names))
        dupnames = cnames[duplicated(cnames)]
        if (length(dupnames)) {
          stopf("Names %s duplicated", collapse(unique(dupnames)))
        }
        datas = do.call(cbind, datas)
        if (is.trafo) {
          changeData(data, datas)
        } else {
          datas
        }
      })
    curgi$data = list()  # free some memory
    for (c in curgi$children) {
      insert.candidates = Filter(function(idx) {
        length(graph[[c]]$data) < idx || is.null(graph[[c]]$data[[idx]])
      }, which(graph[[c]]$parents == n))
      assert(length(insert.candidates) >= 1)
      graph[[c]]$data[[insert.candidates[1]]] = outdata
    }
    graph[[n]] = curgi
    if (!length(curgi$children)) {
      assert(n == length(graph))
      assert(curgi$type == "CBIND")
      return(list(data = outdata, graph = graph))
    }
  }
}


# Creator for a single graph item used in CPOCbind.
# type is 'SOURCE' (the data goes in here; a graph has only one of these. has no parents, one or more children.),
#   'CPO' (the node represents a CPO applied to data; has exactly one parent and one child)
#   or 'CBIND' (node that applies 'cbind' to incoming data streams; has multiple parents, at most one child.)
# parents is of type 'integer', a vector of indices of parent objects in the containing list of CPOGraphItems
#   'SOURCE' CPOGraphItems have no parents, so it is an integer(0) in that case.
# children is an integer vector pointing to the node's children.
# content: the 'content' of the node: either the CPO in a "CPO" node, or the names assigned to each created block for a 'CBIND' block.
makeCPOGraphItem = function(type, parents, children, content) {
  assertChoice(type, c("SOURCE", "CPO", "CBIND"))
  assertInteger(parents)
  assertInteger(children)
  if (type == "CPO") {
    assertClass(content, "CPO")
  } else if (type == "CBIND") {
    assertCharacter(content)
  }
  makeS3Obj("CPOGraphItem",
    type = type, parents = parents, children = children, content = content, data = NULL)
}

#' @export
setCPOId.CPOCbind = function(cpo, id) {
  if (!is.null(id)) {
    stop("Cannot set CPO ID of a CPOCbind object.")
  }
  cpo
}


# This connects two cpoCbind operations together.
# If CBIND_1 is a graph {(SOURCE -> CPOA -> SINK), (SOURCE -> CPOB -> SINK)}
# and CBIND_2 is a graph {(SOURCE -> CPOC -> SINK), (SOURCE -> CBIND_1 (!!!) -> SINK)}
# then we incorporate the graph of CBIND_1 into CBIND_2 to get
# {(SOURCE -> CPOC -> SINK), (SOURCE -> CPOA -> SINK), (SOURCE -> CPOB -> SINK)}.
# In pictures:
#
#  CBIND_1:    ,-> CPOA -.
#             /           \
#    SOURCE -:             :-> SINK
#             \           /
#              `-> CPOB -'
#
#  CBIND_2:    ,-> CPOC ----.
#             /              \
#    SOURCE -:                :-> SINK
#             \              /
#              `-> CBIND_1 -'
#                          (!!!)
#
#  -->
#
#              ,-> CPOC ---.
#             /             \
#    SOURCE -+---> CPOA--.   >-> SINK
#             \           >-'
#              `-> CPOB -'
#
# Graphs that have been 'united' also need to be 'synchronized' (see `synchronizeGraph`) to detect duplicate entries
# to not compute them twice. (E.g. if CBIND_2 had 'CPOB' instead of 'CPOC', the 'CPOB' would only be invoked once
# and its result written twice.)
# @param cpograph [list of CPOGraphItem] the "parent" graph where the `childgraph` should be included
# @param sourceIdx [numeric(1)] the index into `cpograph` of the element that will be the data source for `childgraph`
# @param childgraph [list of CPOGraphItem] the "child" graph to add to `cpograph`
# @param par.vals [list] list of parameter values to set `childgraph` elements to
# @return [list of CPOGraphItem] the updated graph.
uniteGraph = function(cpograph, sourceIdx, childgraph, par.vals) {
  idxoffset = length(cpograph) - 1  # all indices of childgraph get shifted by this much. '-1' because of 1-based arrays

  for (cidx in seq_along(childgraph)) {
    if (cidx == 1) {  # skip "SOURCE" element
      next
    }
    newidx = cidx + idxoffset
    childitem = childgraph[[cidx]]
    for (pidx in seq_along(childitem$parents)) {
      parent = childitem$parents[pidx]
      if (parent == 1) {
        cpograph[[sourceIdx]]$children %c=% newidx
        childitem$parents[pidx] = sourceIdx
      } else {
        childitem$parents[pidx] = parent + idxoffset
      }
    }
    childitem$children %+=% idxoffset

    if ("CPO" %in% class(childitem$content)) {
      childitem$content = setHyperPars(childitem$content, par.vals = subsetParams(par.vals, getParamSet(childitem$content)))
    }
    childgraph[[cidx]] = childitem
  }
  c(cpograph, childgraph[-1])
}

# This checks the graph for operations that can be combined into one operation.
# If we have e.g.
# SOURCE -+-> CPOA -> CPOB -.
#          \                 \
#           `-> CPOA -> CPOC -+-> SINK
# then we will find CPOA to be a duplicate and get the graph
# SOURCE -> CPOA -+-> CPOB -.
#                  \         \
#                   `-> CPOC -+-> SINK
# @param cpograph [list of CPOGraphItem] the graph to simplify
# @return [list of CPOGraphItem] the simplified graph
synchronizeGraph = function(cpograph) {
  newgraph = list()  # the new synchronized graph is built gradually
  for (oldgraph.index in seq_along(cpograph)) {
    # we step through the old graph and move every element
    # to the end of 'newgraph' that does
    # not have an equivalent sibling already in 'newgraph'.
    # Whenever we do that, we overwrite the slot in the old
    # graph with an integer pointing to the position in newgraph
    # that we moved the item to.
    # Therefore, 'cpograph' is a list of CPOGraphItem for indices
    # >= oldgraph.index, and a list of numbers pointing to newgraph
    # for indices < oldgraph.index.
    graphitem = cpograph[[oldgraph.index]]
    # we delete the $children slot and add the children whenever
    # we add them to newgraph.
    graphitem$children = integer(0)
    # need to update the $parents slot to point to the right positions
    # in newgraph:
    graphitem$parents = viapply(graphitem$parents, function(i) cpograph[[i]])
    if (oldgraph.index == 1) {
      assert(length(graphitem$parents) == 0)  # source has no parents
      siblings = integer(0)
    } else {
      assert(length(graphitem$parents) > 0)  # nonsource has at least one parent
      siblings = newgraph[[graphitem$parents[1]]]$children
    }
    # see if graphitem equals one of its siblings. We need only to check the first paren'ts siblings,
    # since equality <=> same parents
    matchingsib = 0
    for (s in siblings) {
      # note that 'siblings' is a $children slot from a newgraph element.
      # therefore it points to newgraph items that have been added before
      # graphitem.
      sib = newgraph[[s]]
      # now we check equivalence
      if (sib$type != graphitem$type) {
        next
      }
      assertChoice(sib$type, c("CPO", "CBIND"))  # "SOURCE" is not the sibling of any element
      if (sib$type == "CPO") {
        scpo = sib$content
        gcpo = graphitem$content
        # for CPO, check equality of underlying cpo and of id
        # The last one is necessary for meta-CPOs like multiplex or cpoCase.
        # When these values differ, the CPOs are unambiguously different.
        if (!identicalCPO(scpo, gcpo) || !identical(getCPOId(scpo), getCPOId(gcpo))) {
          next
        }

        # Now we compare differences that are close enough that (1) it is questionable whether they are
        # intentional and (2) hyperparameter clashes could happen.
        # Compare: different hyperparameters, affect args, unexported args
        if (!identical(getHyperPars(scpo), getHyperPars(gcpo))) {
          stopf("Error: Two CPOS %s are ambiguously identical but have different hyperparameter settings.",
                scpo$debug.name)
        }
        if (!identical(getCPOAffect(scpo), getCPOAffect(gcpo))) {
          stopf("Error: Two CPOS %s are ambiguously identical but have different affect.* settings.",
                scpo$debug.name)
        }
        if (!identical(scpo$unexported.pars, gcpo$unexported.pars)) {
          stopf("Error: Two CPOS %s are ambiguously identical but have different unexported arguments.",
                scpo$debug.name)
        }
        # TODO: whenever more distinguishing hidden state comes along, it needs to be checked here.
        # assert(identical(scpo, gcpo))
      } else { # CBIND
        # cbinds are identical if their parents are identical and the names are identical
        if (!identical(sib$content, graphitem$content)) {
          next
        }
        if (!identical(sib$parents, graphitem$parents)) {
          # TODO: test whether this is actually reachable.
          next
        }
      }
      assert(!matchingsib || matchingsib == s)  # the new sibs are supposed to be unique each (otherwise we missed an opportunity to sync)
      matchingsib = s
    }
    if (matchingsib) {
      # the item is replaced by matchingsib: Therefore
      # we don't copy the item, but just let the cpograph
      # item of this element point to the newgraph item
      # of the matchingsib.
      cpograph[[oldgraph.index]] = matchingsib
    } else {
      newgraph %c=% list(graphitem)
      curidx = length(newgraph)
      for (p in graphitem$parents) {
        newgraph[[p]]$children %c=% curidx
      }
      cpograph[[oldgraph.index]] = curidx
    }
  }
  newgraph
}

# split the graph into vertices that can be CPOs on their own.
# E.g.
#
# CPOA -+---> CPOB ---+-> CPOD
#        \           /
#         `-> CPOC -'
# -->
#  /    \     /   ,-> CPOB -.    \     /    \
# | CPOA | + | -<:           :>-> | + | CPOD |
#  \    /     \   `-> CPOC -'    /     \    /
#
# @param cpograph [list of CPOGraphItem] The graph
# @return [list of list of CPOGraphItem] The smallest constituents of the graph
#   able to be CPOs on their own.
cascadeGraph = function(cpograph) {
  # first we just split up the graph
  dummy.source = makeCPOGraphItem(type = "SOURCE", parents = integer(0), children = integer(0), content = NULL)
  graphs = list()  # list of graphs
  curgraph = NULL  # the current collection of graph vertices
  lookahead = 0    # the last vertex that still needs to be included in 'curgraph'
  offset = 0L  # how much to subtract from parents / children
              # pointers to make them local to their subgraph in curgraph
  for (idx in seq_along(cpograph)) {
    next.elt = cpograph[[idx]]
    next.children = next.elt$children

    next.elt$parents %-=% offset
    assertInteger(next.elt$parents, lower = 1)
    if (lookahead <= idx) {
      if (length(next.children)) {
        assert(min(next.children) == idx + 1)
      }
      next.elt$children = integer(0)
      curgraph %c=% list(next.elt)
      if (idx > 1) {
        # the first item should not go heere
        graphs %c=% list(curgraph)
      }
      curgraph = list(dummy.source)
      offset = idx - 1L  # because of 1-based arrays: the 'idx' element becomes element no 1 in the new graph
      curgraph[[1]]$children = next.children - offset
      assertInteger(curgraph[[1]]$children, lower = 2)
    } else {
      next.elt$children %-=% offset
      curgraph %c=% list(next.elt)
    }
    lookahead = max(lookahead, next.children)
  }
  assert(length(curgraph) == 1)
  graphs
}

#################################
# Printing                      #
#################################

#' @export
# This is more for debugging; the user should not see the "graph" in itself
# (and uses only print.CPOCbind), since the graph itself is hidden quite deeply.
print.CPOGraphItem = function(x, ...) {
  # "CPOGraphItem [<<name>>] P[<<indices of parents>>] C[<<indices of children>>]"
  catf("CPOGraphItem [%s] %s P[%s] C[%s]", x$type,
    switch(x$type, SOURCE = "", CPO = getCPOName(x$content), CBIND = paste0("{", collapse(x$content), "}"), "INVALID"),
    collapse(x$parents), collapse(x$children))
}

#' @export
# print the CPOCbind object, including the graph.
print.CPOCbind = function(x, verbose = FALSE, width = getOption("width"), ...) {
  graph = x$unexported.pars[[".CPO"]]
  x$unexported.pars = NULL
  x$unexported.par.set = NULL
  NextMethod("print")
  if (!verbose) {
    return(invisible(NULL))
  }
  par.vals = getHyperPars(x)
  descriptions = vcapply(graph[-1], function(x) {
    cont = x$content
    switch(x$type, CBIND = paste0("CBIND[", collapse(cont), "]"),
      CPO = {
        cont = setHyperPars(cont, par.vals = subsetParams(par.vals, getParamSet(cont)))
        utils::capture.output(print(cont))
      }, "INVALID")
  })
  offset = length(graph) + 1
  children = lapply(graph[-1], function(x) offset - setdiff(x$parents, 1))
  if (length(children)) {
    printGraph(rev(children), rev(descriptions), width)
  }
}

# pretty-print the graph of a cpoCbind object.
# Each node of the graph is implicitly numbered 1..n, so only
# the edges, and the descriptions, of each node need to be given.
# parameters:
#   children: list of numeric vectors, indicating child indices
#             node 1 is assumed to not have any parents.
#   descriptions: Descriptions to print
#   width: maximum printing width
printGraph = function(children, descriptions, width = getOption("width")) {
  # get list of indices of parents
  parents = replicate(length(children), integer(0), simplify = FALSE)
  for (idx in seq_along(children)) {
    for (child in children[[idx]]) {
      parents[[child]] = c(parents[[child]], idx)
    }
  }
  pcopy = parents

  # topological sort
  queue = 1
  curidx = 1
  ordereds = numeric(length(children))

  lanes = 0L

  mainlines = character(0)
  paddinglines = character(0)

  while (length(queue)) {
    paddinglines = c(collapse(ifelse(lanes, "|", " "), sep = " "), paddinglines)

    current = queue[1]
    queue = queue[-1]
    ordereds[curidx] = current
    curidx %+=% 1

    candidates = children[[current]]
    for (cand in candidates) {
      parents[[cand]] = setdiff(parents[[cand]], current)
      if (!length(parents[[cand]])) {
        queue %c=% cand
      }
    }

    lanes.in = which(lanes %in% pcopy[[current]])  # each lane contains the idx of the vertex out of which it comes
    for (l in lanes.in) {  # check if any lane's vertex's last child is being printed
      children[[lanes[l]]] = setdiff(children[[lanes[l]]], current)
      if (!length(children[[lanes[l]]])) {
        lanes[l] = 0
      }
    }
    first.candidates = intersect(which(lanes == 0), lanes.in)
    second.candidates = which(lanes == 0)
    lane.out = c(first.candidates, second.candidates)[1]
    if (lane.out == length(lanes)) {
      lanes %c=% 0
    }
    lanes[lane.out] = ifelse(length(children[[current]]), current, 0)
    first.interesting.lane = min(lanes.in, lane.out)
    last.interesting.lane = max(lanes.in, lane.out)
    outputchr = collapse(vcapply(seq_along(lanes), function(li) {
      lane = lanes[li]
      if (li == lane.out) {
        chr1 = "O"
      } else if (li %in% lanes.in) {
        chr1 = "+"
      } else if (li < last.interesting.lane && li > first.interesting.lane) {
        chr1 = "-"
      } else if (lane != 0) {
        chr1 = "|"
      } else {
        chr1 = " "
      }
      if (li < last.interesting.lane && li >= first.interesting.lane) {
        if (li == lane.out) {
          chr2 = ">"
        } else if (li == lane.out - 1) {
          chr2 = "<"
        } else {
          chr2 = "-"
        }
      } else {
        chr2 = " "
      }
      paste0(chr1, chr2)
    }), sep = "")
    mainlines = c(outputchr, mainlines)
  }
  ordereds = rev(ordereds)

  graphwidth = length(lanes) * 2
  textwidth = width - graphwidth

  if (textwidth > 20) {
    for (i in seq_along(mainlines)) {
      itemtext = c(strwrap(descriptions[ordereds[i]], width = textwidth), "")
      itemtext = c(paste0(mainlines[i], itemtext[1]), paste0(paddinglines[i], itemtext[-1]))
      cat(itemtext, sep = "\n")
    }
  } else {
    for (i in seq_along(mainlines)) {
      itemtext = c(paste(mainlines[i], i), paddinglines[i])
      cat(itemtext, sep = "\n")
    }
    for (i in seq_along(mainlines)) {
      itemtext = strwrap(paste(i, descriptions[ordereds[i]], sep = ": "), width = textwidth)
      cat(itemtext, sep = "\n")
    }
  }
}

cpoCbind = wrapFauxCPOConstructor(cpoCbind)  # nolint
registerCPO(cpoCbind, "meta", NULL, "Combine multiple CPO operations by joining their outputs column-wise.")
