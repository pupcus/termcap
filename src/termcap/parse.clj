(ns termcap.parse
  (:require [clojure.string :as str]))

(defn expect [expectation & {:keys [from]}]
  (let [pattern (re-pattern expectation)]
    (when-let [[match token] (re-find pattern from)]
      [match token (subs from (count match))])))

(defn next-int [s]
  (when-let [[found] (re-find #"^(\d+)" s)]
    (read-string found)))

(defn binary-operation [f stack]
  (conj (pop (pop stack)) (f (peek stack) (peek (pop stack)))))

(defn unary-operation [f stack]
  (conj (pop stack) (f (peek stack))))

(defn is-true? [value]
  (cond
    (string? value)
    (not (empty? value))

    (number? value)
    (not (zero? value))

    :otherwise
    (true? value)))

(defn and? [v1 v2]
  (true? (and (is-true? v1) (is-true? v2))))

(defn or? [v1 v2]
  (true? (or (is-true? v1) (is-true? v2))))

(declare execute)

(defn parse [s stack vars & args]
  (loop [current s
         result ""
         stack (or stack [])
         vars (or vars {})
         args (vec args)]
    (if (seq current)
      (let [n (first current)]
        (cond
          (= n \%)
          (let [[match token fss :as found] (expect "%((%)|([^%]*))" :from current)
                [[r ess] stack vars args :as executed] (execute found stack vars args)]
            (recur (or ess fss)
                   (str result r)
                   stack
                   vars
                   args))

          :otherwise
          (recur (subs current 1)
                 (str result n)
                 stack
                 vars
                 args)))
      [result stack vars args])))

(defn execute-dispatch-fn [[_ token _] _ _ _]

;;  (println "TOKEN = " token)
  (cond

    (re-matches #"[?].*" token)     :conditional
    (re-matches #"t.*" token)       :truthy
    (re-matches #"e.*" token)       :else
    (re-matches #";.*" token)       :end
    (re-matches #"!.*" token)       :not
    (re-matches #"[\%].*" token)     :percent
    (re-matches #"&.*" token)       :bit-and
    (re-matches #"'.*" token)       :character
    (re-matches #"[*].*" token)     :multiply
    (re-matches #"[+].*" token)     :add
    (re-matches #"-.*" token)       :subtract
    (re-matches #"/.*" token)       :divide
    (re-matches #"<.*" token)       :less-than
    (re-matches #"=.*" token)       :equal
    (re-matches #">.*" token)       :greater-than
    (re-matches #"A.*" token)       :logical-and
    (re-matches #"O.*" token)       :logical-or
    (re-matches #"P.*" token)       :set-dynamic-var
    (re-matches #"[\^].*" token)    :bit-exclusive-or

    (re-matches #":?[^doxXsc]*(d|o|x|X|s|c).*" token)  :format
    (re-matches #"[23]{1}.*" token)                    :format-special-case

    (re-matches #"g.*" token)       :get-dynamic-var
    (re-matches #"i.*" token)       :increment
    (re-matches #"l.*" token)       :length
    (re-matches #"m.*" token)       :mod
    (re-matches #"p.*" token)       :push-arg
    (re-matches #"[{].*" token)     :number
    (re-matches #"[|].*" token)       :bit-or
    (re-matches #"~.*" token)       :complement

    ))

(defmulti execute #'execute-dispatch-fn)

(defmethod execute :conditional [[_ [_ & more] ss] stack vars args]
  (let [[match token css] (expect "(.+?(?=%t))" :from ss)
        [result ns nv a] (apply parse (str (str/join more) match) stack vars args)]
    [[result (str (str/join more) css)] ns nv a]))

(defmethod execute :truthy [[_ [_ & more] ss] stack vars args]
  (if (is-true? (peek stack))
    (if-let [[match token _] (expect "(.*?(?=%e))" :from ss)]
      (let [[result ns nv a] (apply parse (str (str/join more) match) (pop stack) vars args)
            [_ _ ess] (expect "(.*?(?=%;))" :from ss)]
        [[result ess] ns nv a])
      (let [[match token ess] (expect "(.*?(?=%;))" :from ss)
            [result ns nv a] (apply parse (str (str/join more) match) (pop stack) vars args)]
        [[result ess] ns nv a]))
    (let [[_ _ ess] (expect "(.*?(?=%e))" :from ss)]
      [[nil ess] (pop stack) vars args])))

(defmethod execute :else [[_ [_ & more] ss] stack vars args]
  (if-let [[match token ess] (expect "(.*?(?=%t))" :from ss)]
    (let [[result ns nv a] (apply parse (str (str/join more) match) stack vars args)]
      [[result ess] ns nv a])
    (let [[match token ess] (expect "(.*?(?=%;))" :from ss)
          [result ns nv a] (apply parse (str (str/join more) match) stack vars args)]
      [[result ess] ns nv a])))

(defmethod execute :end [[_ [_ & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] stack vars args])

(defmethod execute :add [[_ [_ & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (binary-operation + stack) vars args])

(defmethod execute :bit-and [[_ [_ & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (binary-operation bit-and stack) vars args])

(defmethod execute :bit-exclusive-or [[_ [_ & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (binary-operation bit-xor stack) vars args])

(defmethod execute :bit-or [[_ [_ & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (binary-operation bit-or stack) vars args])

(defmethod execute :character [[_ [_ c _ & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (conj stack (int c)) vars args])

(defmethod execute :complement [[_ [_ & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (unary-operation bit-not stack) vars args])

(defmethod execute :divide [[_ [_ & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (binary-operation / stack) vars args])

(defmethod execute :equal [[_ [_ & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (binary-operation = stack) vars args])

(defmethod execute :format [[_ format-str-plus ss] stack vars args]
  (let [[_ format-str more] (re-find #":?([^dxXosc]*?(?:d|x|X|o|s|c))(.*)" format-str-plus)
        char? (= \c (last format-str))
        value (peek stack)]
    [[(format (str "%" format-str) (if char? (char value) value)) (str more ss)] (pop stack) vars args]))

(defmethod execute :format-special-case [[_ [width & more] ss] stack vars args]
  (let [format-str (str "%0" width "d")]
    [[(format format-str (peek stack)) (str more ss)] (pop stack) vars args]))

(defmethod execute :get-dynamic-var [[_ [_ var-name & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (conj stack (get vars var-name)) vars args])

(defmethod execute :greater-than [[_ [_ & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (binary-operation > stack) vars args])

(defmethod execute :increment [[_ [_ & more] ss] stack vars [arg1 arg2 & rest-args]]
  [[nil (str (str/join more) ss)] stack vars (vec (concat [(inc arg1) (inc arg2)] (vec rest-args)))])

(defmethod execute :length [[_ [_ & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (conj (pop stack) (count (str (peek stack)))) vars args])

(defmethod execute :less-than [[_ [_ & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (binary-operation < stack) vars args])

(defmethod execute :logical-and [[_ [_ & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (binary-operation and? stack) vars args])

(defmethod execute :logical-or [[_ [_ & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (binary-operation or? stack) vars args])

(defmethod execute :mod [[_ [_ & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (binary-operation mod stack) vars args])

(defmethod execute :multiply [[_ [_ & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (binary-operation * stack) vars args])

(defmethod execute :not [[_ [_ & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (unary-operation (comp not is-true?) stack) vars args])

(defmethod execute :number [[_ token ss] stack vars args]
  (let [[_ number more] (re-find #"\{(\d*)\}(.*)" token)]
    [[nil (str more ss)] (conj stack (next-int number)) vars args]))

(defmethod execute :percent [[_ [_ & more] ss] stack vars args]
  [["%" (str (str/join more) ss)] stack vars args])

(defmethod execute :push-arg [[_ token ss] stack vars args]
  (let [[_ number more] (re-find #"(\d+)(.*)" token)
        index (next-int number)
        arg (get args (dec index))]
    [[nil (str more ss)] (conj stack arg) vars args]))

(defmethod execute :subtract [[_ [_ & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (binary-operation - stack) vars args])

(defmethod execute :set-dynamic-var [[_ [_ var-name & more] ss] stack vars args]
  [[nil (str (str/join more) ss)] (pop stack) (assoc vars var-name (peek stack)) args])

(defn done [])
