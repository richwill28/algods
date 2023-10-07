module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end

module type Avltree =
  sig
    type elt
    type t
    val empty : t
    val is_empty : t -> bool
    val height : t -> int
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val remove : elt -> t -> t
    val iter : (elt -> unit) -> t -> unit
  end

module Make (Ord : OrderedType) =
  struct
    type elt = Ord.t
    type t = Empty | Node of t * elt * t * int

    let empty = Empty

    let is_empty = function
      | Empty -> true
      | Node (_) -> false

    let height = function
      | Empty -> -1
      | Node (_, _, _, h) -> h

    let rec mem x = function
    | Empty -> false
    | Node (l, v, r, _) ->
      let c = Ord.compare x v in
      if c < 0 then mem x l
      else if c > 0 then mem x r
      else true

    let left_rotate = function
      | Empty -> Empty
      | Node (l, v, r, _) ->
        match r with
        | Empty -> invalid_arg "Avltree.left_rotate"
        | Node (rl, rv, rr, _) ->
          let l = Node (l, v, rl, (Int.max (height l) (height rl)) + 1) in
          Node (l, rv, rr, (Int.max (height l) (height rr)) + 1)

    let right_rotate = function
      | Empty -> Empty
      | Node (l, v, r, _) ->
        match l with
        | Empty -> invalid_arg "Avltree.right_rotate"
        | Node (ll, lv, lr, _) ->
          let r = Node (lr, v, r, (Int.max (height lr) (height r)) + 1) in
          Node (ll, lv, r, (Int.max (height ll) (height r)) + 1)

    let balance_factor = function
      | Empty -> invalid_arg "Avltree.balance_factor"
      | Node (l, _, r, _) -> height r - height l

    let balance = function
      | Empty -> Empty
      | Node (l, v, r, h) as t ->
        let bf = balance_factor t in
        if bf < -1 then
          if balance_factor l >= 1
          then right_rotate (Node (left_rotate l, v, r, h))
          else right_rotate t
        else if bf > 1 then
          if balance_factor r <= -1
          then left_rotate (Node (l, v, right_rotate r, h))
          else left_rotate t
        else t

    let rec add x = function
      | Empty -> Node (Empty, x, Empty, 0)
      | Node (l, v, r, _) as t ->
        let c = Ord.compare x v in
        if c < 0 then
          let l = add x l in
          balance (Node (l, v, r, (Int.max (height l) (height r)) + 1))
        else if c > 0 then
          let r = add x r in
          balance (Node (l, v, r, (Int.max (height l) (height r)) + 1))
        else t

    let rec min_elt = function
      | Empty -> raise Not_found
      | Node (Empty, v, _, _) -> v
      | Node (l, _, _, _) -> min_elt l

    let rec remove_min_elt = function
      | Empty -> invalid_arg "Avltree.remove_min_elt"
      | Node (Empty, _, r, _) -> r
      | Node (l, v, r, _) ->
        let l = remove_min_elt l in
        balance (Node (l, v, r, (Int.max (height l) (height r)) + 1))

    (** Merge two trees l and r into one.
        All elements of l must precede the elements of r.
        Assume | height r - height l | < 2. *)
    let merge l r =
      match (l, r) with
      | (Empty, r) -> r
      | (l, Empty) -> l
      | (l, r) ->
        let v = min_elt r in
        let r = remove_min_elt r in
        balance (Node (l, v, r, (Int.max (height l) (height r)) + 1))

    let rec remove x = function
      | Empty -> invalid_arg "Avltree.remove"
      | Node (l, v, r, _) ->
        let c = Ord.compare x v in
        if c < 0 then
          let l = remove x l in
          balance (Node (l, v, r, (Int.max (height l) (height r)) + 1))
        else if c > 0 then
          let r = remove x r in
          balance (Node (l, v, r, (Int.max (height l) (height r)) + 1))
        else merge l r

    let rec iter f = function
      | Empty -> ()
      | Node (l, v, r, _) ->
        iter f l; f v; iter f r
  end
