`Extra` is a one-or-both data type:

    data Extra a b
      = One a
      | Both a b

It corresponds to the type `(a, Maybe b)` or the regular expression `ab?`, and
can be used to represent a cons cell in a non-empty heterogeneous list. As a
monad, it represents computations that must log at every step or fail.

It could use a better name.
