;;;; simple-graph.lisp

(in-package #:simple-graph)

(defgeneric add-node (graph node))
(defgeneric add-edge (graph first second))
(defgeneric nodes (graph))
(defgeneric node-count (graph))
(defgeneric edge-count (graph))
(defgeneric depth-first-search (graph function start))
(defgeneric breadth-first-search (graph function start))
(defgeneric in-degree (graph node))
(defgeneric out-degree (graph node))
(defgeneric to-dot (graph stream))
(defgeneric to-png (graph file-name))

(defclass graph ()
  ((adjacency-list :initform nil :type (or null cons)))
  (:documentation "A simple graph data structure."))

(defclass digraph (graph)
  ()
  (:documentation "A simple digraph data structure."))

(defmethod add-node ((graph graph) node)
  (With-slots (adjacency-list) graph
    (pushnew (list node)
             adjacency-list :key #'car)
    (setf adjacency-list (sort adjacency-list #'< :key #'car))))

(defmethod add-edge ((graph graph) first second)
  (add-node graph first)
  (add-node graph second)
  (With-slots (adjacency-list) graph
    (let ((new-first (push second (cdr (assoc first adjacency-list))))
          (new-second (push first (cdr (assoc second adjacency-list)))))
      (rplacd (assoc first adjacency-list) new-first)
      (rplacd (assoc second adjacency-list) new-second))))

(defmethod nodes ((graph graph))
  (with-slots (adjacency-list) graph
    (mapcar #'car adjacency-list)))

(defmethod node-count ((graph graph))
  (With-slots (adjacency-list) graph
    (length adjacency-list)))

(defmethod edge-count ((graph graph))
  (with-slots (adjacency-list) graph
    (/ (apply #'+ (mapcar (compose #'length #'cdr) adjacency-list)) 2)))

(defmethod depth-first-search ((graph graph) function start)
  (let ((visited nil))
    (with-slots (adjacency-list) graph
      (labels
          ((dfs (node)
             (when (not (member node visited))
               (let ((starting (assoc node adjacency-list)))
                 (when starting
                   (pushnew (car starting) visited)
                   (dolist (next (cdr starting))
                     (when (not (member next visited))
                       (dfs next)))
                   (funcall function (car starting)))))))
        (dfs start)))))

(defmethod breadth-first-search ((graph graph) function start)
  (let ((visited nil))
    (with-slots (adjacency-list) graph
      (labels
          ((bfs (node depth)
             (let ((starting (assoc node adjacency-list)))
               (cond
                 ((and starting (= 0 depth) (not (member node visited)))
                  (pushnew node visited)
                  (funcall function node)
                  (set-difference (cdr starting) visited))
                 ((and starting)
                  (if (> depth 0)
                      (apply (curry #'concatenate 'list)
                             (mapcar (lambda (next)
                                       (if next
                                           (bfs next (- depth 1))
                                           nil))
                                     (cdr starting)))
                      nil))
                 (t nil)))))
        (loop for i from 0
           while (bfs start i))))))

(defmethod out-degree ((graph graph) node)
  (with-slots (adjacency-list) graph
    (length (cdr (assoc node adjacency-list)))))

(defun my-count (item list)
  (loop for it in list when (= item it) summing 1))

(defmethod in-degree ((graph graph) node)
  (with-slots (adjacency-list) graph
    (loop for next-node in adjacency-list
       summing (my-count node (cdr next-node)))))

(defmethod to-dot ((graph graph) stream)
  (with-slots (adjacency-list) graph
    (format stream "graph {~%")
    (dolist (node adjacency-list)
      (dolist (goes-to (cdr node))
        (when (< (car node) goes-to)
          (format stream "~a -- ~a;~%" (car node) goes-to))))
    (format stream "}~%")))

(defmethod to-png ((graph graph) file-name)
  (let ((dot-graph (with-output-to-string (outs) (to-dot graph outs))))
    (with-input-from-string (ins dot-graph)
      (let ((command (format nil "dot -Tpng -o\"~a\"" file-name)))
        (uiop:run-program command :input ins :force-shell t :output t :error-output t)))))

(defmethod add-edge ((graph digraph) first second)
  (add-node graph first)
  (add-node graph second)
  (With-slots (adjacency-list) graph
    (let ((new-first (push second (cdr (assoc first adjacency-list)))))
      (rplacd (assoc first adjacency-list) new-first))))

(defmethod to-dot ((graph digraph) stream)
  (with-slots (adjacency-list) graph
    (format stream "digraph {~%")
    (dolist (node adjacency-list)
      (dolist (goes-to (cdr node))
        (format stream "~a -> ~a;~%" (car node) goes-to)))
    (format stream "}~%")))


(defun random-graph (edge-count &key (graph-type 'digraph) (max-node 100))
  (let ((graph (make-instance graph-type)))
    (dotimes (i edge-count)
      (add-edge graph (random max-node) (random max-node)))
    graph))
