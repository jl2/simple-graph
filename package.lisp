;;;; package.lisp

(defpackage #:simple-graph
  (:use #:cl #:alexandria)
  (:export #:graph
           #:digraph
           #:adjacency-list
           #:add-node
           #:add-edge
           #:node-count
           #:edge-count
           #:nodes
           #:to-dot
           #:to-png
           #:random-graph))

