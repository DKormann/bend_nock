type Nock = (Atom value) | (Cell ~head ~tail)
type Maybe = (Just value) | Nothing
type Res = (err message) | (ok value)

Nock/Compare a b = match a{
  Nock/Atom: match b{
    Nock/Atom: (== a.value b.value)
    Nock/Cell: 0
  }
  Nock/Cell: match b{
    Nock/Atom: 0
    Nock/Cell: (* (Nock/Compare a.head b.head) (Nock/Compare a.tail b.tail))
  }
}

parse list=
  if ((== match list { List/Cons: 1 List/Nil: 0} match list { List/Cons: 0 List/Nil: 1})){
    (Res/ok (Nock/Atom list)) # this a number
  }else{
    match list {
      List/Cons:
        match list.tail {
          List/Nil: (parse list.head)
          List/Cons: match head = (parse list.head){
            Res/err: head
            Res/ok: match tail = (parse list.tail){
              Res/err: tail
              Res/ok: (Res/ok (Nock/Cell head.value tail.value))
            }
          }
        }
      List/Nil: (Res/err "parse nil")
    }
  }

unparse nock = match nock {
    Nock/Atom: nock.value
    Nock/Cell: [(unparse nock.head) (unparse nock.tail)]
  }

all_ok ls = match ls{
  List/Nil: (Res/ok)
  List/Cons: match ls.head{
    Res/ok: (all_ok ls.tail)
    Res/err: ls.head
  }
} 


assert_eq a b = match a {
  Res/err: match b{
    Res/err: (Res/ok "ok: both err")
    Res/ok: a
  }
  Res/ok: match b{
    Res/err: b
    Res/ok: 
    switch (Nock/Compare a.value b.value){
      0: (Res/err ["assertion failed",  a,  b])
      _: (Res/ok "ok")
    }
  }
}


unpack x = match x {
  Res/err: x
  Res/ok: x.value
}
cc = (parse [[1 2] [4 5] [6 7]])

test_parse =

let t = (all_ok[
  (assert_eq (parse 1) (Res/ok (Nock/Atom 1)))
  (assert_eq (parse [1 2]) (Res/ok (Nock/Cell (Nock/Atom 1) (Nock/Atom 2))))
  (assert_eq cc cc)
  (assert_eq (parse (unparse (unpack cc))) (cc))
])
t




# A noun is an atom or a cell.  An atom is a natural number.  A cell is an ordered pair of nouns.

# Reduce by the first matching pattern; variables match any noun.

# nock(a)             *a
# [a b c]             [a [b c]]

# ?[a b]              0
# ?a                  1
# +[a b]              +[a b]
# +a                  1 + a
# =[a a]              0
# =[a b]              1

# /[1 a]              a
# /[2 a b]            a
# /[3 a b]            b
# /[(a + a) b]        /[2 /[a b]]
# /[(a + a + 1) b]    /[3 /[a b]]
# /a                  /a

# #[1 a b]            a
# #[(a + a) b c]      #[a [b /[(a + a + 1) c]] c]
# #[(a + a + 1) b c]  #[a [/[(a + a) c] b] c]
# #a                  #a

# *[a [b c] d]        [*[a b c] *[a d]]

# *[a 0 b]            /[b a]
# *[a 1 b]            b
# *[a 2 b c]          *[*[a b] *[a c]]
# *[a 3 b]            ?*[a b]
# *[a 4 b]            +*[a b]
# *[a 5 b c]          =[*[a b] *[a c]]

# *[a 6 b c d]        *[a *[[c d] 0 *[[2 3] 0 *[a 4 4 b]]]]
# *[a 7 b c]          *[*[a b] c]
# *[a 8 b c]          *[[*[a b] a] c]
# *[a 9 b c]          *[*[a c] 2 [0 1] 0 b]
# *[a 10 [b c] d]     #[b *[a c] *[a d]]

# *[a 11 [b c] d]     *[[*[a c] *[a d]] 0 3]
# *[a 11 b c]         *[a c]

# *a                  *a

# slot nck ad = 
#   switch ad{
#     0: nck
#     1: nck
#     _: match nck{
#       Nock/Atom: (Nock/Crash "slot atom")
#       Nock/Cell:
#         let n = (slot nck (/ ad 2))
#         match n{
#           Nock/Atom: (Nock/Crash "slot atom")
#           Nock/Cell: 
#           switch (& 1 ad){
#             0: n.head
#             _: n.tail
#           }
#         }
#     }
#   }

slot c ad =
  switch ad{
    0: (Res/err "slot 0")
    1: (Res/ok c)
    _: match c{
      Nock/Atom: (Res/err "slot atom")
      Nock/Cell:
        let n = (slot c (/ ad 2))
        match n{
          Res/err: n
          Res/ok: match sa = n.value{
            Nock/Atom: (Res/err "slot atom")
            Nock/Cell: 
            switch (& 1 ad){
              0: (Res/ok sa.head)
              _: (Res/ok sa.tail)
            }
          }
        }
    }
  }

eval (Nock/Atom value) = (Res/err "evaluating atom")
eval (Nock/Cell core arm) = match arm{
  Nock/Atom: (Res/err "evaluating atom")
  Nock/Cell: match opcode = arm.head{
    Nock/Cell: # unpacking multi instruction

      match first = (eval (Nock/Cell core arm.head)){
        Res/err: first
        Res/ok: match second = (eval (Nock/Cell core arm.tail)){
          Res/err: second
          Res/ok: (Res/ok (Nock/Cell first.value second.value))
        }
      }
    Nock/Atom: switch opcode.value{
      0: match ad = arm.tail{
        Nock/Cell: (Res/err ["slot crash on ad", ad])
        Nock/Atom: (slot core ad.value)
      }
      1: (Res/ok arm.tail)
      _: (Res/err ["crash: opcode", opcode.value, "not implemented"])
    }
  }
}


assert_eval code target = (assert_eq (eval (unpack (parse code))) (parse target))

ucc = (unpack cc)





main=
[
  ["test_parse" (test_parse)]

  ["test_eval" (all_ok [

    # slot internal
    (assert_eq cc cc)
    (assert_eq (slot ucc 0) (Res/err []))
    (assert_eq (slot ucc 1) cc)
    (assert_eval [7 12 33] []) #error
    (assert_eval [7 0 1] 7)
    # slot
    (assert_eval [[1 2] 0 1] [1 2])
    (assert_eval [[1 2] 0 2] 1)
    (assert_eval [[1 2] 0 3] 2)
    (assert_eval [[1 [7 3]] 0 6] 7)
    # const
    (assert_eval [[1 [7 3]] 1 6] 6)
    (assert_eval [[1 7] 1 [2 4]] [2 4])
    # unpacking
    (assert_eval [77 [[0 1] [0 1]]] [77 77])



    # (eval (unpack (parse [[1 2] 0 1])))
    # (Res/err ["pr" (eval [4 0 2])])

  ])
  
  ]
    # (eval (unpack (parse [7 0 7])))
    # (eval (unpack (parse [7 0 1])))
    # (eval (unpack (parse [[1 2] 0 1])))


  # ["printings" ucc]
]
# let check = (all_ok [
#   (assert_crash [7 7])
#   (assert_eval [7 1 22] 22)
#   (assert_eq (slot (parse [7 1 22]) 1) (parse [7 1 22]))
#   (assert_eq (slot (parse [7 1 22]) 2) (parse [7]))
#   (assert_eq (slot (parse [[7 1] 22]) 5) (parse [1]))
#   (assert_eval [7 0 1] 7)
#   (assert_eval [[7 7] 0 2] 7)
#   (assert_eval [[7 7] 0 1] [7 7])
#   (assert_eval [[[7 6] 7] 0 5] 6)



# ])

# check

