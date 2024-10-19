

inc x = (+ x 1)
fn x y = (+ x y)


map f list = match list{
  List/Nil: List/Nil
  List/Cons: (List/Cons (f list.head) (map f list.tail))
}

main=
  # (fn 4 5 5)
  
  (map inc [1 2 3])
