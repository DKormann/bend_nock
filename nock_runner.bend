# no error handling just crash

# parsing
is_number n = (== match n {List/Cons: 0 List/Nil: 1} match n {List/Cons: 1 List/Nil: 0})

parse list = 
  if (is_number list) {!list} else{
  match list{
  List/Nil: !list
  List/Cons:
    let left = (parse list.head)
    match list.tail{
      List/Nil: left
      List/Cons: ![left (parse list.tail)]
    }
  }
}

unparse nock = fold nock {Tree/Leaf: nock.value Tree/Node: [nock.left nock.right] }

#core 
slot ad data =
switch ad{
  0: crsh 1: data _: match data{
    Tree/Leaf: crsh Tree/Node: match d = (slot (/ ad 2) data){
      Tree/Leaf: crsh Tree/Node: switch (& ad 1) {0: d.left _: d.right} 
    }
  }
}

edit ad value data = 
  switch ad{
    0: crsh
    1: value
    _: match data{
      Tree/Leaf: crsh
      Tree/Node: switch (& ad 1){
          0: ![data.left (edit (/ad 2) value data.right)]
          _: ![(edit (/ad 2) value data.left) data.right]
        }
    }
  }

crsh = (eval !1 !1)

unpack x n =

if (== n 1){x} else { match x{
Tree/Leaf: crsh
Tree/Node: (x.left, (unpack x.right (- n 1))) }}

eval data formula =
  match formula {
  Tree/Leaf: crsh
  Tree/Node: match formula.left{
    Tree/Node: ![(eval data formula.left) (eval data formula.right)]
    Tree/Leaf:
    switch instruction = formula.left.value{
      # essentials
      0: match formula.right {Tree/Node: crsh Tree/Leaf: (slot formula.right.value data)}
      1: formula.right
      2: match formula.right {Tree/Leaf: crsh Tree/Node: (eval (eval data formula.right.left) (eval data formula.right.right))}
      3: match (eval data formula.right) {Tree/Leaf: 1 Tree/Node: 0}
      4: match ev = (eval data formula.right) {Tree/Leaf: !(+1 ev.value) Tree/Node: crsh}
      5: match rr=formula.right {Tree/Leaf: crsh Tree/Node: !(- 1 (eq (eval data rr.left) (eval data rr.right)))}
      # sugars
      6: let (b, c, d) = (unpack formula.right 3) (eval data (eval ![c d] ![!0 (eval ![!2 !3] ![!0 (eval data ![!4 ![!4 b]])])]))
      7: let (b, c) = (unpack formula.right 2) (eval (eval data b) c)
      8: let (b, c) = (unpack formula.right 2) (eval ![(eval data b) data] c)
      9: let (b, c) = (unpack formula.right 2) (eval (eval data c) ![!2 ![![!0 !1] ![!0 b]]])
      10: let (b, c, d) = (unpack formula.right 3) match b {Tree/Node: crsh Tree/Leaf: (edit b.value (eval data c) (eval data d)) }
      11: let (b, c) = (unpack formula.right 2) let res= (eval data c) match b {Tree/Node: if (eq (eval data b.right) !77) {res} else {res} Tree/Leaf: res}
      _: crsh
    }
  }
}

# testing

all ls = match ls{
  List/Nil: 1
  List/Cons: switch ls.head{0: 0 _: (all ls.tail) }
}
eq a b = match a { Tree/Leaf: match b{ Tree/Leaf: (== a.value b.value) Tree/Node: 0 }
  Tree/Node: match b { Tree/Leaf: 0 Tree/Node: (all [(eq a.left b.left) (eq a.right b.right)])}
}
list_eval data formula = (unparse (eval (parse data) (parse formula)))
list_all_eq ls = 
  match ls{
    List/Nil: "ok."
    List/Cons:
      let (a, b) = ls.head
      if (eq (parse a) (parse b)) {(list_all_eq ls.tail)}
      else {[a "!=" b]}
  }

main = 
let cc = [[1 2] [4 5] 6 7]
[
  (list_all_eq[
    # parsing
    ([1,2], [1,2])
    (cc, (unparse (parse cc)))
    ((unparse (slot 1 77)), 77)
    # slot
    ((list_eval cc [0 1]), cc)
    ((list_eval cc [0 6]), [4,5])
    ((list_eval cc [0 3]), [[4,5], [6,7]])
    # const
    ((list_eval cc [1 6]), 6)
    ((list_eval 77 [[1 2][1 cc]]), [2, cc])
    # eval
    ((list_eval [1 1] [2 [1 7] [0 1]]), 1)
    ((list_eval [0 1] [2 [1 7] [0 1]]), 7)
    ((list_eval [[0 1] [1 22]] [2 [1 7] [0 1]]), [7 22])
    # check
    ((list_eval [1 1] [3 [1 7]]), 1)
    ((list_eval [0 1] [3 [0 1]]), 0)
    # inc
    ((list_eval [1 1] [4 [1 7]]), 8)
    ((list_eval 77 [4 [0 1]]), 78)
    # eq
    ((list_eval 77 [5 [0 1] [1 77]]), 0)
    ((list_eval 77 [5 [0 1] [1 8]]), 1)
  ])
  
  # decrement implementation with only essential instructions
  let decrement = [2 [[[0 1] 1 0] 1 2 [[[0 4] 4 0 5] 0 3] 1 2 [0 1] 2 [[1 0 5] 0 3][1 0] 4 4 5 [4 0 5] 0 4] 1 2 [0 1] 0 3]

  (list_all_eq[
    ((list_eval 22  decrement), 21)
    ((list_eval 5  decrement), 4)
  ])

  (list_all_eq[
  ((list_eval cc [6 [1 1] [1 2] [1 1]]), 1)
  ((list_eval cc [6 [1 0] [0 2] [1 1]]), [1 2])
  ((list_eval cc [6 [1 1] [0 2] [0 6]]), [4 5])

  ((list_eval cc [7 [1 1] [[0 1] [1 22]]]), [1 22])
  ((list_eval cc [7 [0 1] [[0 1] [0 2]]]), [cc [1 2]])

  ((list_eval 42 [8 [4 0 1] [0 1]]), [43, 42])
  ((list_eval 42 [8 [4 0 1] [4 0 3]]), 43)

  ((list_eval [0 1] [9 1 0 1]), [0 1])
  ((list_eval 88 [9 1 [[1 1] [0 1]]]), 88)

  ((unparse(edit 1 !88 !77)), 88)
  ((unparse(edit 2 !7 ![!1 !2])), [1 7])
  ((unparse(edit 2 ![!3 !3] ![!1 !2])), [1 3 3])

  ((list_eval cc [10 1 [1 77] [0 1]]), 77)
  ((list_eval cc [10 6 [1 77] [0 1]]), [[1 2] 77 6 7])

  ((list_eval [3 3] [11 [[88 88] [0 2]] [0 1]]), [3 3])
  ])

  # (list_eval [0 1] [11 [[88 88] [88 88]] [0 1]])

]

