## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

suppressPackageStartupMessages(library(magrittr, quietly = TRUE))
suppressPackageStartupMessages(library(pmatch, quietly = TRUE))


## ------------------------------------------------------------------------
enum := ONE | TWO | THREE

## ------------------------------------------------------------------------
ONE
TWO
THREE

## ------------------------------------------------------------------------
elements <- list(ONE, TWO, THREE)
for (elm in elements) {
    value <- cases(elm,
                   ONE -> 1,
                   TWO -> 2,
                   THREE -> 3)
    cat("Element", toString(elm), "maps to value", toString(value), "\n")
}

## ------------------------------------------------------------------------
elements <- list(ONE, TWO, THREE)
for (elm in elements) {
    value <- cases(elm,
                   ONE -> 1,
                   v -> v)
    cat("Element", toString(elm), "maps to value", toString(value), "\n")
}

## ------------------------------------------------------------------------
zero_one_two_three := ZERO | ONE(x) | TWO(x,y) | THREE(x,y,z)

## ------------------------------------------------------------------------
ONE(1)
TWO(1,2)
THREE(1,2,3)

## ------------------------------------------------------------------------
f <- function(v) {
    cases(v,
          ZERO         -> 0,
          ONE(x)       -> x,
          TWO(x,y)     -> x + y,
          THREE(x,y,z) -> x + y + z)
}

f(ZERO)
f(ONE(1))
f(TWO(1,2))
f(THREE(1,2,3))

## ------------------------------------------------------------------------
f <- function(v) {
    cases(v,
          ZERO               -> 0,
          ONE(x)             -> x,
          TWO(ONE(x),ONE(y)) -> x + y + 42,
          TWO(x,y)           -> x + y,
          THREE(x,y,z)       -> x + y + z)
}

f(TWO(ONE(10),ONE(-10)))

## ------------------------------------------------------------------------
cases(42,
      1 -> 1,
      13 -> 13,
      otherwise -> 24)

## ------------------------------------------------------------------------
one_or_two := ONE(x : numeric) | TWO(x : numeric, y : numeric)

## ---- error=TRUE---------------------------------------------------------
ONE(1)
ONE("foo")

## ------------------------------------------------------------------------
tree := L(elm : numeric) | T(left : tree, right : tree)

## ------------------------------------------------------------------------
f <- function(x) {
    cases(x, 
          L(v) -> v, 
          T(left,right) -> c(f(left), f(right)))
}
x <- T(T(L(1),L(2)), T(T(L(3),L(4)),L(5)))
f(x)

## ----gh-installation, eval = FALSE---------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("mailund/pmatch")

## ----list-definition-----------------------------------------------------
linked_list := NIL | CONS(car, cdr : linked_list)
lst <- CONS(1, CONS(2, CONS(3, NIL)))

## ----list-functions------------------------------------------------------
list_length <- function(lst, acc = 0) {
  force(acc)
  cases(lst,
        NIL -> acc,
        CONS(car, cdr) -> list_length(cdr, acc + 1))
}

list_length(lst)

reverse_list <- function(lst, acc = NIL) {
  force(acc)
  cases(lst,
        NIL -> acc,
        CONS(car, cdr) -> reverse_list(cdr, CONS(car, acc)))
}

reverse_list(lst)

## ------------------------------------------------------------------------
vector_to_list <- function(vec) {
  lst <- NIL
  for (i in seq_along(vec)) {
    lst <- CONS(vec[[i]], lst)
  }
  reverse_list(lst)
}

list_to_vector <- function(lst) {
  n <- list_length(lst)
  v <- vector("list", length = n)
  f <- function(lst, i) {
    force(i)
    cases(lst,
          NIL -> NULL,
          CONS(car, cdr) -> {
            v[[i]] <<- car
            f(cdr, i + 1)
            }
          )
  }
  f(lst, 1)
  v %>% unlist
}

lst <- vector_to_list(1:5)
list_length(lst)
list_to_vector(lst)
lst %>% reverse_list %>% list_to_vector

## ------------------------------------------------------------------------
search_tree := E | T(left : search_tree, value, right : search_tree)

## ------------------------------------------------------------------------
tree <- T(T(E,1,E), 3, T(E,4,E))
tree

## ------------------------------------------------------------------------
member <- function(tree, x) {
  cases(tree,
        E -> FALSE,
        T(left, val, right) -> {
          if (x < val) member(left, x)
          else if (x > val) member(right, x)
          else TRUE
        })
}
member(tree, 0)
member(tree, 1)
member(tree, 2)
member(tree, 3)
member(tree, 4)

## ------------------------------------------------------------------------
insert <- function(tree, x) {
  cases(tree,
        E -> T(E, x, E),
        T(left, val, right) ->
          if (x < val)
            T(insert(left, x), val, right)
          else if (x > val)
            T(left, val, insert(right, x))
          else
            T(left, x, right)
        )
}

tree <- E
for (i in sample(2:4))
  tree <- insert(tree, i)

for (i in 1:6) {
  cat(i, " : ", member(tree, i), "\n")
}

## ------------------------------------------------------------------------
colour := R | B
rb_tree := E | T(col : colour, left : rb_tree, value, right : rb_tree)

## ------------------------------------------------------------------------
member <- function(tree, x) {
  cases(tree,
        E -> FALSE,
        T(col, left, val, right) -> {
          if (x < val) member(left, x)
          else if (x > val) member(right, x)
          else TRUE
        })
}

tree <- T(R, E, 2, T(B, E, 5, E))
for (i in 1:6) {
  cat(i, " : ", member(tree, i), "\n")
}

## ------------------------------------------------------------------------
insert_rec <- function(tree, x) {
  match(tree,
        E -> T(R, E, x, E),
        T(col, left, val, right) -> {
          if (x < val)
            balance(T(col, insert_rec(left, x), val, right))
          else if (x > val)
            balance(T(col, left, val, insert_rec(right, x)))
          else
            T(col, left, x, right) # already here
        })
}
insert <- function(tree, x) {
  tree <- insert_rec(tree, x)
  tree$col <- B
  tree
}

## ------------------------------------------------------------------------
balance <- function(tree) {
  match(tree,
        T(B,T(R,a,x,T(R,b,y,c)),z,d) -> T(R,T(B,a,x,b),y,T(B,c,z,d)),
        T(B,T(R,T(R,a,x,b),y,c),z,d) -> T(R,T(B,a,x,b),y,T(B,c,z,d)),
        T(B,a,x,T(R,b,y,T(R,c,z,d))) -> T(R,T(B,a,x,b),y,T(B,c,z,d)),
        T(B,a,x,T(R,T(R,b,y,c),z,d)) -> T(R,T(B,a,x,b),y,T(B,c,z,d)),
        otherwise -> tree)
}

