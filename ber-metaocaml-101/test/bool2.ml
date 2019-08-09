type ('a,'b) staged = Now of 'b | Later of ('a, 'b) code

(* It is the *presence* of this module with a nested module which 
 * causes the failure, even though it is empty and not used! *)
module XXX = struct
  module TT = struct
  end
end
