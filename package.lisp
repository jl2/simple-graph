;;;; package.lisp

(defpackage #:simple-graph
  (:nicknames :sg)
  (:use #:cl #:alexandria)
  (:export #:graph
           #:digraph
           #:adjacency-list
           #:depth-first-search
           #:breadth-first-search
           #:add-node
           #:add-edge
           #:node-count
           #:edge-count
           #:in-degree
           #:out-degree
           #:nodes
           #:to-dot
           #:to-png
           #:random-graph))

