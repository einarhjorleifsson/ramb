# input validation

    Code
      rb_encode_ices_rectangle(1:3, 1:2)
    Condition
      Error in `rb_encode_ices_rectangle()`:
      ! 'lon' and 'lat' must have the same length.

---

    Code
      rb_encode_ices_rectangle("a", 1)
    Condition
      Error in `rb_encode_ices_rectangle()`:
      ! Both 'lon' and 'lat' must be numeric vectors.

