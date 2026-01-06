(defn e/makef
  [base fmt & args]
  (merge base {:msg (string/format fmt ;args)}))

(defn e/emf
  [base fmt & args]
  (error (e/makef base fmt ;args)))

(defn e/show
  [err]
  (assertf (dictionary? err) "expected dictionary but got: %n" err)
  #
  (eprintf "%s: %s" (get err :in) (get err :msg))
  (when-let [args (get err :args)]
    (eprint "  args:")
    (eachp [n v] args
      (eprintf "    %s: %n" n v)))
  (when-let [locals (get err :locals)]
    (eprint "  locals:")
    (eachp [n v] locals
      (eprintf "    %s: %n" n v)))
  (when-let [e (get err :e-via-try)]
    (eprintf "  e via try: %n" e)))

