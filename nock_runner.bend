
eq a b = match a { Tree/Leaf: match b{ Tree/Leaf: (== a.value b.value) Tree/Node: 0 }
  Tree/Node: match b { Tree/Leaf: 0 Tree/Node: (& (eq a.left b.left) (eq a.right b.right))}
}

parse list = 
  if ((== match list {List/Cons: 0 List/Nil: 1} match list {List/Cons: 1 List/Nil: 0}) list) {!list}
  else{
    match list{
      List/Nil: !list
      List/Cons: match list.tail{ List/Nil: (parse list.head) List/Cons: ![(parse list.head) (parse list.tail)]}
    }
  }

unparse nock = fold nock {Tree/Leaf: nock.value Tree/Node: [nock.left nock.right] }

slot ad data =
switch ad{ 0: crsh 1: data _: match data{
    Tree/Leaf: crsh Tree/Node: match d = (slot (/ ad 2) data){
      Tree/Leaf: crsh Tree/Node: switch (& ad 1) {0: d.left _: d.right}  }
}}

edit ad value data = 
  switch ad{ 0: crsh 1: value _: match data{
      Tree/Leaf: crsh Tree/Node: switch (& ad 1) {
        0: ![data.left (edit (/ad 2) value data.right)]
        _: ![(edit (/ad 2) value data.left) data.right]}
  }}

crsh = (eval !1 !1)

unpack x n = if (== n 1){x} else { match x{Tree/Leaf: crsh Tree/Node: (x.left, (unpack x.right (- n 1))) }}

eval data formula =
  match formula {
  Tree/Leaf: crsh
  Tree/Node:
    let arg = formula.right
    let upck = @n (unpack arg n)
    match formula.left{
    Tree/Node: ![(eval data formula.left) (eval data arg)]
    Tree/Leaf:
    switch instruction = formula.left.value{
      # essentials
      0: match arg {Tree/Node: crsh Tree/Leaf: (slot arg.value data)}
      1: arg
      2: match arg {Tree/Leaf: crsh Tree/Node: (eval (eval data arg.left) (eval data arg.right))}
      3: match (eval data arg) {Tree/Leaf: 1 Tree/Node: 0}
      4: match ev = (eval data arg) {Tree/Leaf: !(+1 ev.value) Tree/Node: crsh}
      5: let (b, c) = (upck 2) !(- 1 (eq (eval data b) (eval data c)))
      # sugars
      6: let (b, c, d) = (upck 3) (eval data (eval ![c d] ![!0 (eval ![!2 !3] ![!0 (eval data ![!4 ![!4 b]])])]))
      7: let (b, c) = (upck 2) (eval (eval data b) c)
      8: let (b, c) = (upck 2) (eval ![(eval data b) data] c)
      9: let (b, c) = (upck 2) (eval (eval data c) ![!2 ![![!0 !1] ![!0 b]]])
      10: let (b, c, d) = (upck 3)
          match b {Tree/Node: crsh Tree/Leaf: (edit b.value (eval data c) (eval data d)) }
      11: let (b, c) = (upck 2)
          let res= (eval data c)
          match b {Tree/Node: if (eq (eval data b.right) !77) {res} else {res} Tree/Leaf: res}
      _: crsh
    }
  }
}


run code = match code{
  List/Cons: (unparse (eval (parse code.head) (parse code.tail)))
  List/Nil: "nock code must be a cell like [subject code]"
}

main = (run [55 [2 [[[0 1] 1 0] 1 2 [[[0 4] 4 0 5] 0 3] 1 2 [0 1] 2 [[1 0 5] 0 3][1 0] 4 4 5 [4 0 5] 0 4] 1 2 [0 1] 0 3]])