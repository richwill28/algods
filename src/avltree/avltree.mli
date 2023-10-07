module type OrderedType =
  sig
    (** The type of the tree elements. *)
    type t

    (** A total ordering function over the tree elements. *)
    val compare : t -> t -> int
  end

module type Avltree =
  sig
    (** The type of the tree elements. *)
    type elt

    (** The type of the tree. *)
    type t

    (** The empty tree. *)
    val empty : t

    (** Test whether a tree is empty or not. *)
    val is_empty : t -> bool

    (** [height t] returns the height of the tree [t]. *)
    val height : t -> int

    (** [mem x t] tests whether [x] belongs to the tree [t]. *)
    val mem : elt -> t -> bool

    (** [add x t] returns a tree containing all elements of [t],
        plus [x]. If [x] was already in [t], [t] is return unchanged. *)
    val add : elt -> t -> t

    (** [remove x t] returns a tree containing all elements of [t],
        except [x]. If [x] was not in [t], [t] is returned unchanged. *)
    val remove : elt -> t -> t

    (** [iter f t] applies [f] in turn to all elements of [t].
        The elemtnts of [t] are presented to [f] in increasing order
        with respect to the ordering over the type of the elements. *)
    val iter : (elt -> unit) -> t -> unit
  end

(** Functor building an implementation of the tree structure
    given a totally orderd type. *)
module Make (Ord : OrderedType) : Avltree with type elt = Ord.t
