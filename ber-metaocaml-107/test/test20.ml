open Bool2

type ('a,'b,'c) unary = {
  unow : 'b -> 'c ;
  ulater : ('a,'b) code -> ('a, 'c) code
}

module type SET =
sig
  type n
  val to_string : ('a, n, string) unary
end

module E2 (N : SET) = struct
    let to_str p = match p with
      | Now (x,y) -> Now ((N.to_string.unow x) ^ (N.to_string.unow y))
      | Later p -> Later (.< let (x, y) = .~p in 
                .~(N.to_string.ulater .<x>.) ^ 
                .~(N.to_string.ulater .<y>.) >. )
end
