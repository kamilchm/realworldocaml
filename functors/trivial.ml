open Base

module type X_int = sig val x : int end

module Increment (M : X_int) : X_int = struct
  let x = M.x + 1
end

module Three = struct let x = 3 end
module Four = Increment(Three)
