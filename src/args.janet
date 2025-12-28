(defn parse-args
  [args]
  (def the-args (array ;args))
  #
  (def head (get the-args 0))
  #
  (def conf-file ".jtfm.jdn")
  #
  (when (or (= head "-h") (= head "--help")
            # might have been invoked with no paths in repository root
            (and (not head)
                 (not= :file (os/stat conf-file :mode))))
    (break @{:show-help true}))
  #
  (when (or (= head "-v") (= head "--version")
            # might have been invoked with no paths in repository root
            (and (not head)
                 (not= :file (os/stat conf-file :mode))))
    (break @{:show-version true}))
  #
  (def opts
    (if head
      (if-not (and (string/has-prefix? "{" head)
                   (string/has-suffix? "}" head))
        @{}
        (let [parsed
              (try (parse (string "@" head))
                ([e] (eprint e)
                     (errorf "failed to parse options: %n" head)))]
          (assertf (and parsed (table? parsed))
                   "expected table but found: %s" (type parsed))
          (array/remove the-args 0)
          parsed))
      @{}))
  #
  (def [includes excludes]
    (cond
      # paths on command line take precedence over conf file
      (not (empty? the-args))
      [the-args @[]]
      # conf file
      (= :file (os/stat conf-file :mode))
      (let [conf (try (parse (slurp conf-file))
                   ([e] (error e)))]
        (assertf conf "failed to parse: %s" conf-file)
        (assertf (dictionary? conf)
                 "expected dictionary, got: %s" (type conf))
        #
        [(array ;(get conf :includes @[]))
         (array ;(get conf :excludes @[]))])
      #
      (errorf "unexpected result parsing: %n" args)))
  #
  (defn merge-indexed
    [left right]
    (default left [])
    (default right [])
    (distinct [;left ;right]))
  #
  (merge opts
         {:includes (merge-indexed includes (get opts :includes))
          :excludes (merge-indexed excludes (get opts :excludes))}))

(comment

  (parse-args ["src/main.janet"])
  # =>
  @{:excludes @[]
    :includes @["src/main.janet"]}

  (parse-args ["-h"])
  # =>
  @{:show-help true}

  (parse-args ["{:overwrite true}" "src/main.janet"])
  # =>
  @{:excludes @[]
    :includes @["src/main.janet"]
    :overwrite true}

  (parse-args [`{:excludes ["src/args.janet"]}` "src/main.janet"])
  # =>
  @{:excludes @["src/args.janet"]
    :includes @["src/main.janet"]}

  )

