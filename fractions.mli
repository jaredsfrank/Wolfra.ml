(*[frac d] returns (numerator, denominator) that represents the fraction form of d*)
val frac : float -> int*int

(*[is_int f] returns true iff f is within .000001 of an integer value*)
val is_int: float -> bool