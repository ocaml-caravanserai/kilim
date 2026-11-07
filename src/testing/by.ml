type t = Node of Fake_dom.node | Test_id of string

let node n = Node n

let test_id s = Test_id s
