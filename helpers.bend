type Nock = (Atom value) | (Cell ~head ~tail)

parse list = 
  if ((== match list { List/Cons: 1 List/Nil: 0} match list { List/Cons: 0 List/Nil: 1})){
    (Nock/Atom list)
  }else{
    match list {
      List/Cons: 
        match list.tail {
          List/Nil: (parse list.head) List/Cons: (Nock/Cell (parse list.head) (parse list.tail))
        }
      List/Nil: (Nock/Atom 777)
    }
  }

unparse nock =
  match nock {
    Nock/Atom: nock.value
    Nock/Cell: (List/Cons (unparse nock.head)
      match t = nock.tail{
        Nock/Atom: [(unparse t)]
        Nock/Cell: (unparse t)
    })
  }


main = 44