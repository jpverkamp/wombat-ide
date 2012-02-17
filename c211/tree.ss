; Provides the C211 tree modules
(library
  (c211 tree)
  (export
    tree leaf empty-tree
    tree? leaf? empty-tree?
    root-value left-subtree right-subtree
    draw-tree)

  (import (chezscheme))

  ; create the datatypes
  (define :tree (make-record-type "tree" '(value left right)))
  (define :empty-tree (make-record-type "empty-tree" '()))

  ; create constructors for the types of trees
  (define tree (record-constructor :tree))
  (define (leaf value) (tree value (empty-tree) (empty-tree)))
  (define empty-tree (record-constructor :empty-tree))

  ; create accessors for the fields
  (define root-value (record-accessor :tree 0))
  (define left-subtree (record-accessor :tree 1))
  (define right-subtree (record-accessor :tree 2))

  ; predicates to determine types
  (define (tree? tr)
    (or ((record-predicate :tree) tr)
        ((record-predicate :empty-tree) tr)))
  (define (leaf? tr)
    (and ((record-predicate :tree) tr)
         (empty-tree? (left-subtree tr))
         (empty-tree? (right-subtree tr))))
  (define empty-tree? (record-predicate :empty-tree))

  ; helper to export a tree to a string
  (define (export-tree tr)
    (with-output-to-string
      (lambda ()
        (write tr))))

; draw a tree
 (define (draw-tree tr)
   #f))