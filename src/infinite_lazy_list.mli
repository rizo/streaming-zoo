
type 'a t = :: of 'a * 'a t lazy_t

val count : int -> int t
val cycle : 'a list -> 'a t
val drop : int -> 'a t -> 'a t
val drop_while : ('a -> bool) -> 'a t -> 'a t
val filter : ('a -> bool) -> 'a t -> 'a t
val group : ?eq:('a -> 'a -> bool) -> 'a t -> ('a list) t
val head : 'a t -> 'a
val interleave : 'a t -> 'a t -> 'a t
val intersperse : 'a -> 'a t -> 'a t
val iterate : ('a -> 'a) -> 'a -> 'a t
val join : ('a t) t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val nth : int -> 'a t -> 'a
val partition : ('a -> bool) -> 'a t -> ('a t * 'a t)
val repeat : 'a -> 'a t
val scan : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t
val scan1 : ('a -> 'a -> 'a) -> 'a t -> 'a t
val show : ('a -> string) -> 'a t -> string
val span : ('a -> bool) -> 'a t -> ('a list * 'a t)
val tail : 'a t -> 'a t
val take : int -> 'a t -> 'a list
val take_while : ('a -> bool) -> 'a t -> 'a list
val transpose : ('a t) t -> ('a t) t
val unfold : ('c -> ('a * 'c)) -> 'c -> 'a t
val unzip : ('a * 'b) t -> ('a t * 'b t)
val zip : 'a t -> 'b t -> ('a * 'b) t
val zip_with : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t


