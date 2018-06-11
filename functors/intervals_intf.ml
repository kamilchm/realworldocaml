open Base

module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module type Interval_intf = sig
  type t
  type endpoint
  val create : endpoint -> endpoint -> t
  val is_empty : t -> bool
  val contains : t -> endpoint -> bool
  val intersect : t -> t -> t
end

module type Interval_intf_with_sexp = sig
  type t
  include Interval_intf with type t := t
  include Core_kernel.Sexpable with type t := t
end

module Make_interval(Endpoint : sig
    type t
    include Comparable with type t := t
    include Core_kernel.Sexpable with type t := t
  end)
  : (Interval_intf_with_sexp with type endpoint := Endpoint.t)
  = struct

  type t = | Interval of Endpoint.t * Endpoint.t
           | Empty
	[@@deriving sexp]

  (** [create low high] creates a new interval from [low] to
      [high]. If [low > high], then the interval is empty *)
  let create low high =
    if Endpoint.compare low high > 0 then Empty
    else Interval (low,high)

  let t_of_sexp sexp =
    match t_of_sexp sexp with
    | Empty -> Empty
    | Interval (x,y) -> create x y

  (** Returns true if the interval is empty *)
  let is_empty = function
    | Empty -> true
    | Interval _ -> false

  (** [contains t x] returns true if [x] is contained in the
      interval [t] *)
  let contains t x =
    match t with
    | Empty -> false
    | Interval (l,h) ->
      Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  (** [intersect t1 t2] returns the intersection of the two input
      intervals *)
  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match t1,t2 with
    | Empty,_ | _,Empty -> Empty
    | Interval (l1,h1), Interval(l2,h2) ->
      create (max l1 l2) (min h1 h2)

end

module Int_interval = Make_interval(Int)
module Rev_int_interval =
  Make_interval(struct
    type t = int [@@deriving sexp]
    let compare x y = Int.compare y x
  end)

module String_interval = Make_interval(String)
