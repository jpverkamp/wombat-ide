; Provides the C211 tree library

; License: source-license.txt
; If this code is used independently, copy the license here.

#|
Constructors:
  (tree v l r)
    - creates a tree with value v and left and right subtrees l and r
  (leaf v)
    - identical to (tree v (empty-tree) (empty-tree))
  (empty-tree)
    - creates a new empty tree

Predicates:
  (tree? t)
    - tests if something is a tree (either a full tree, a leaf, or empty)
  (leaf? t)
    - tests if something is a tree with no children (and not empty)
  (empty-tree? t)
    - tests if something is an empty tree (no children or value)

Accessors:
  (root-value t)
    - get the value out of the tree
  (left-subtree t)
    - access the left subtree of a tree
  (right-subtree t)
    - access the right subtree of a tree

Other:
  (draw-tree t)
    - display a graphical representation of the tree
|#

(library
  (c211 tree)
  (export
    tree leaf empty-tree
    tree? leaf? empty-tree?
    root-value left-subtree right-subtree
    left-subtree? right-subtree?
    draw-tree)

  (import (except (chezscheme) lambda define))
  (import (wombat define))
  (import (wombat java))

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

  ; return #t if the left/right subtree is a tree and non-empty
  (define (left-subtree? tr)
    (and (tree? tr)
         (tree? (left-subtree tr))
         (not (empty-tree? (left-subtree tr)))))
  (define (right-subtree? tr)
    (and (tree? tr)
         (tree? (right-subtree tr))
         (not (empty-tree? (right-subtree tr)))))

  ; draw a tree
  (define (draw-tree tr)
    (define (export-tree tr)
      (cond
        [(empty-tree? tr) "#!empty-tree\n"]
        [(tree? tr) (string-append
                      (format "#!tree ~a" (root-value tr)) "\n"
                      (export-tree (left-subtree tr))
                      (export-tree (right-subtree tr)))]
        [else (format "~a\n" tr)]))
    (call-to-java draw-tree (export-tree tr))
    (void)))