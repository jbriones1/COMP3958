module Astar = struct
  type 'a t =
    {
      cost : 'a -> 'a -> int;
      goal : 'a;
      get_next_states: 'a -> 'a list
    }

  type 'a path =
    {

    }
end