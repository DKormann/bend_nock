from nock_runner import (unparse, parse, eval, edit, slot, eq)

list_eval data formula = (unparse (eval (parse data) (parse formula)))
list_all_eq ls = match ls{ List/Nil: "ok." List/Cons:
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
  ]

