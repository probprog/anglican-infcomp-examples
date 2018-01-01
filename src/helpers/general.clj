(ns helpers.general)

(defn empirical-MAP
  "calculates a maximum a-posteriori value from weighted samples;
  - accepts a map or sequence of log weighted
  values [x log-w].
  - returns the x with largest log-w"
  [weighted]
  (first (apply max-key second weighted)))
