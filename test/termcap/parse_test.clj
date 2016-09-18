(ns termcap.parse-test
  (:require [clojure.test :refer :all]
            [termcap.parse :as subject]))

(deftest parsing-parametrized-strings

  (let [stack []
        vars {}
        args []]

    (testing "test putting numeric values on the stack"
      (let [[r s v a] (apply subject/parse "%{1}%{100}" stack vars args)]
        (is (= "" r))
        (is (= [1 100] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test putting character values on the stack"
      (let [[r s v a] (apply subject/parse "%'i'%'o'" stack vars args)]
        (is (= "" r))
        (is (= [105 111] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test putting arguments on the stack"
      (let [args [20 30]
            [r s v a] (apply subject/parse "%p1%p2" stack vars args)]
        (is (= "" r))
        (is (= [20 30] s))
        (is (= {} v))
        (is (= [20 30] a))))

    (testing "test setting a dynamic variable"
      (let [[r s v a] (apply subject/parse "%{100}%Pa" stack vars args)]
        (is (= "" r))
        (is (= [] s))
        (is (= {\a 100} v))
        (is (= [] a))))

    (testing "test setting a static variable"
      (let [[r s v a] (apply subject/parse "%{100}%PA" stack vars args)]
        (is (= "" r))
        (is (= [] s))
        (is (= {\A 100} v))
        (is (= [] a))))

    (testing "test getting a dynamic variable"
      (let [vars {\a 100}
            [r s v a] (apply subject/parse "%ga" stack vars args)]
        (is (= "" r))
        (is (= [100] s))
        (is (= {\a 100} v))
        (is (= [] a))))

    (testing "test getting a static variable"
      (let [vars {\A 100}
            [r s v a] (apply subject/parse "%gA" stack vars args)]
        (is (= "" r))
        (is (= [100] s))
        (is (= {\A 100} v))
        (is (= [] a))))

    (testing "test output a literal '%'"
      (let [[r s v a] (apply subject/parse "%%" stack vars args)]
        (is (= "%" r))
        (is (= [] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test checking the string length of an string item on the stack"
      (let [stack ["four"]
            [r s v a] (apply subject/parse "%l" stack vars args)]
        (is (= "" r))
        (is (= [4] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test checking the string length of a numeric item on the stack"
      (let [stack [100]
            [r s v a] (apply subject/parse "%l" stack vars args)]
        (is (= "" r))
        (is (= [3] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test increment of next two parameters"
      (let [args [0 1 2 3]
            [r s v a] (apply subject/parse "%i" stack vars args)]
        (is (= "" r))
        (is (= [] s))
        (is (= {} v))
        (is (= [1 2 2 3] a))))

    (testing "test + operator"
      (let [stack [0 1 2 3]
            [r s v a] (apply subject/parse "%+%+%+" stack vars args)]
        (is (= "" r))
        (is (= [6] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test - operator"
      (let [stack [1 2 4]
            [r s v a] (apply subject/parse "%-%-" stack vars args)]
        (is (= "" r))
        (is (= [1] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test * operator"
      (let [stack [10 2 2]
            [r s v a] (apply subject/parse "%+%*" stack vars args)]
        (is (= "" r))
        (is (= [40] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test / operator"
      (let [stack [2 2 8]
            [r s v a] (apply subject/parse "%+%/" stack vars args)]
        (is (= "" r))
        (is (= [5] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test m (mod) operator"
      (let [stack [3 2 8]
            [r s v a] (apply subject/parse "%+%m" stack vars args)]
        (is (= "" r))
        (is (= [1] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test ^ (xor) operator"
      (let [stack [3 8 2]
            [r s v a] (apply subject/parse "%-%^" stack vars args)]
        (is (= "" r))
        (is (= [-7] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test & operator"
      (let [stack [3 -3 7]
            [r s v a] (apply subject/parse "%&" stack vars args)]
        (is (= "" r))
        (is (= [3 5] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test | operator"
      (let [stack [3 -3 7]
            [r s v a] (apply subject/parse "%|" stack vars args)]
        (is (= "" r))
        (is (= [3 -1] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test ! operator"
      (let [stack ["test"]
            [r s v a] (apply subject/parse "%!" stack vars args)]
        (is (= "" r))
        (is (= [false] s))
        (is (= {} v))
        (is (= [] a)))

      (let [stack [""]
            [r s v a] (apply subject/parse "%!" stack vars args)]
        (is (= "" r))
        (is (= [true] s))
        (is (= {} v))
        (is (= [] a)))

      (let [stack [true]
            [r s v a] (apply subject/parse "%!" stack vars args)]
        (is (= "" r))
        (is (= [false] s))
        (is (= {} v))
        (is (= [] a)))

      (let [stack [false]
            [r s v a] (apply subject/parse "%!" stack vars args)]
        (is (= "" r))
        (is (= [true] s))
        (is (= {} v))
        (is (= [] a)))

      (let [stack [0]
            [r s v a] (apply subject/parse "%!" stack vars args)]
        (is (= "" r))
        (is (= [true] s))
        (is (= {} v))
        (is (= [] a)))

      (let [stack [1]
            [r s v a] (apply subject/parse "%!" stack vars args)]
        (is (= "" r))
        (is (= [false] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test ~ (complement) operator"
      (let [stack [1]
            [r s v a] (apply subject/parse "%~" stack vars args)]
        (is (= "" r))
        (is (= [-2] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test A (logical AND) operator"
      (let [stack [1 ""]
            [r s v a] (apply subject/parse "%A" stack vars args)]
        (is (= "" r))
        (is (= [false] s))
        (is (= {} v))
        (is (= [] a)))

      (let [stack [0 "test"]
            [r s v a] (apply subject/parse "%A" stack vars args)]
        (is (= "" r))
        (is (= [false] s))
        (is (= {} v))
        (is (= [] a)))

      (let [stack [1 "test"]
            [r s v a] (apply subject/parse "%A" stack vars args)]
        (is (= "" r))
        (is (= [true] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test O (logical OR) operator"
      (let [stack [1 ""]
            [r s v a] (apply subject/parse "%O" stack vars args)]
        (is (= "" r))
        (is (= [true] s))
        (is (= {} v))
        (is (= [] a)))

      (let [stack [0 "test"]
            [r s v a] (apply subject/parse "%O" stack vars args)]
        (is (= "" r))
        (is (= [true] s))
        (is (= {} v))
        (is (= [] a)))

      (let [stack [1 "test"]
            [r s v a] (apply subject/parse "%O" stack vars args)]
        (is (= "" r))
        (is (= [true] s))
        (is (= {} v))
        (is (= [] a)))

      (let [stack [0 ""]
            [r s v a] (apply subject/parse "%O" stack vars args)]
        (is (= "" r))
        (is (= [false] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test simple if-then conditional"
      (let [[r s v a] (apply subject/parse "%?%{1}%t4%{2}%;" stack vars args)]
        (is (= "4" r))
        (is (= [2] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test simple if-then-else truthy conditional"
      (let [[r s v a] (apply subject/parse "%?%{1}%t4%{3}%e5%;" stack vars args)]
        (is (= "4" r))
        (is (= [3] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test simple if-then-else falsey conditional"
      (let [[r s v a] (apply subject/parse "%?%{0}%t4%e5%{4}%;" stack vars args)]
        (is (= "5" r))
        (is (= [4] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test simple if-then-else-if truthy conditional"
      (let [[r s v a] (apply subject/parse "%?%{0}%t4%e5%{1}%tei%{7}%e8%;" stack vars args)]
        (is (= "5ei" r))
        (is (= [7] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test simple if-then-else-if falsey conditional"
      (let [[r s v a] (apply subject/parse "%?%{0}%t4%e5%{0}%tei%{7}%e8%;" stack vars args)]
        (is (= "58" r))
        (is (= [] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test output of simple integer"
      (let [[r s v a] (apply subject/parse "%{2}%d" stack vars args)]
        (is (= "2" r))
        (is (= [] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test output of simple integer as hex"
      (let [[r s v a] (apply subject/parse "%{31}%X" stack vars args)]
        (is (= "1F" r))
        (is (= [] s))
        (is (= {} v))
        (is (= [] a)))

      (let [[r s v a] (apply subject/parse "%{31}%x" stack vars args)]
        (is (= "1f" r))
        (is (= [] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test output of simple integer as string"
      (let [[r s v a] (apply subject/parse "%{31}%s" stack vars args)]
        (is (= "31" r))
        (is (= [] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test output of simple integer as character"
      (let [[r s v a] (apply subject/parse "%{97}%c" stack vars args)]
        (is (= "a" r))
        (is (= [] s))
        (is (= {} v))
        (is (= [] a))))

    (testing "test output of simple integer with special case format specifiers %2 and %3"
      (let [[r s v a] (apply subject/parse "%{1}%2" stack vars args)]
        (is (= "01" r))
        (is (= [] s))
        (is (= {} v))
        (is (= [] a)))

      (let [[r s v a] (apply subject/parse "%{1}%3" stack vars args)]
        (is (= "001" r))
        (is (= [] s))
        (is (= {} v))
        (is (= [] a))))

    (comment
      (testing "test output of a floating point value as a hexadecimal??"
        (let [args [1.1]
              [r s v a] (apply subject/parse "%p1%{1000}%/%2.2X" stack vars args)]
          (is (= "a" r))
          (is (= [] s))
          (is (= {} v))
          (is (= [] a)))))

    (testing "test a more complicated string (from xterm-256color)"
      (let [args [3]
            [r s v a] (apply subject/parse "^[[%?%p1%{8}%<%t3%p1%d%e%p1%{16}%<%t9%p1%{8}%-%d%e38;5;%p1%d%;m," stack vars args)]
        (is (= "^[[38;5;3m," r))
        (is (= [] s))
        (is (= {} v))
        (is (= args a))))
))
