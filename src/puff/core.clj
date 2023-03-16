(ns puff.core)


;; full map of ethereum opcode names to their hex values
(def opcodes
  {:stop       0x00
   :add        0x01
   :mul        0x02
   :sub        0x03
   :div        0x04
   :sdiv       0x05
   :mod        0x06
   :smod       0x07
   :addmod     0x08
   :mulmod     0x09
   :exp        0x0a
   :signextend 0x0b
   :lt         0x10
   :gt         0x11
   :slt        0x12
   :sgt        0x13
   :eq         0x14
   :iszero     0x15
   :and        0x16
   :or         0x17
   :xor        0x18
   :not        0x19
   :byte       0x1a
   :sha3       0x20
   :address    0x30
   :balance    0x31
   :origin     0x32
   :caller     0x33
   :callvalue  0x34
   :calldataload 0x35
   :push1      0x60
   :push2      0x61
   :push3      0x62
   :push4      0x63
   :push5      0x64
   :push6      0x65
   :push7      0x66
   :push8      0x67
   :push9      0x68
   :push10     0x69
   :push11     0x6a
   :push12     0x6b
   :push13     0x6c
   :push14     0x6d
   :push15     0x6e
   :push16     0x6f
   :push17     0x70
   :push18     0x71
   :push19     0x72
   :push20     0x73
   :push21     0x74
   :push22     0x75
   :push23     0x76
   :push24     0x77
   :push25     0x78
   :push26     0x79
   :push27     0x7a
   :push28     0x7b
   :push29     0x7c
   :push30     0x7d
   :push31     0x7e
   :push32     0x7f
   :dup1       0x80
   :dup2       0x81
   :dup3       0x82
   :dup4       0x83
   :dup5       0x84
   :dup6       0x85
   :dup7       0x86
   :dup8       0x87
   :dup9       0x88
   :dup10      0x89
   :dup11      0x8a
   :dup12      0x8b
   :dup13      0x8c
   :dup14      0x8d
   :dup15      0x8e
   :dup16      0x8f
   :swap1      0x90
   :swap2      0x91
   :swap3      0x92
   :swap4      0x93
   :swap5      0x94
   :swap6      0x95
   :swap7      0x96
   :swap8      0x97
   :swap9      0x98
   :swap10     0x99
   :swap11     0x9a
   :swap12     0x9b
   :swap13     0x9c
   :swap14     0x9d
   :swap15     0x9e
   :swap16     0x9f
   :log0       0xa0
   :log1       0xa1
   :log2       0xa2
   :log3       0xa3
   :log4       0xa4
   :create     0xf0
   :call       0xf1
   :callcode   0xf2
   :return     0xf3
   :delegatecall 0xf4
   :create2    0xf5
   :staticcall 0xfa
   :revert     0xfd
   :invalid    0xfe
   :selfdestruct 0xff})

;; EVM interpreter
(defn interpret [code]
  (let [stack (atom [])
        pc (atom 0)
        memory (atom [])
        gas (atom 0)]
    (while (not= (nth code @pc) (opcodes :stop))
      (let [opcode (nth code @pc)]
        (println @pc)
        (println opcode)
        (condp = opcode
          (opcodes :push1) (do
                             (swap! stack conj (nth code (inc @pc)))
                             (swap! pc inc))
          (opcodes :push2) (do
                             (swap! stack conj (nth code (inc @pc)))
                             (swap! stack conj (nth code (+ 2 @pc)))
                             (swap! pc #(+ 2 %)))
          (opcodes :push3)  (do
                              (swap! stack conj (nth code (inc @pc)))
                              (swap! stack conj (nth code (+ 2 @pc)))
                              (swap! stack conj (nth code (+ 3 @pc)))
                              (swap! pc #(+ 3 %)))
          ;; should behave like (opcodes :add) (swap! stack conj (+ (first @stack) (second @stack)))
          (opcodes :add) (let [a (first @stack)
                               b (second @stack)]
                           (swap! stack rest)
                           (swap! stack rest)
                           (swap! stack conj (+ a b)))
          (opcodes :mul) (let [a (first @stack)
                               b (second @stack)]
                           (swap! stack rest)
                           (swap! stack rest)
                           (swap! stack conj (* a b)))
          (opcodes :sub) (let [a (first @stack)
                               b (second @stack)]
                           (swap! stack rest)
                           (swap! stack rest)
                           (swap! stack conj (- a b)))
            (opcodes :div) (let [a (first @stack)
                                 b (second @stack)]
                                (swap! stack rest)
                                (swap! stack rest)
                                (swap! stack conj (/ a b)))
            (opcodes :sdiv) (let [a (first @stack)
                                  b (second @stack)]
                                (swap! stack rest)
                                (swap! stack rest)
                                (swap! stack conj (int (/ a b))))
            (opcodes :mod) (let [a (first @stack)
                                 b (second @stack)]
                                (swap! stack rest)
                                (swap! stack rest)
                                (swap! stack conj (mod a b)))
            (opcodes :smod) (let [a (first @stack)
                                  b (second @stack)]
                                (swap! stack rest)
                                (swap! stack rest)
                                (swap! stack conj (int (mod a b))))
            (opcodes :addmod) (let [a (first @stack)
                                    b (second @stack)
                                    c (nth @stack 2)]
                                (swap! stack rest)
                                (swap! stack rest)
                                (swap! stack rest)
                                (swap! stack conj (mod (+ a b) c)))
            (opcodes :mulmod) (let [a (first @stack)
                                    b (second @stack)
                                    c (nth @stack 2)]
                                (swap! stack rest)
                                (swap! stack rest)
                                (swap! stack rest)
                                (swap! stack conj (mod (* a b) c)))
            (opcodes :exp) (let [a (first @stack)
                                 b (second @stack)]
                                (swap! stack rest)
                                (swap! stack rest)
                                (swap! stack conj (.pow Math a b)))
            (opcodes :signextend) (let [a (first @stack)
                                        b (second @stack)]
                                    (swap! stack rest)
                                    (swap! stack rest)
                                    (swap! stack conj (if (pos? a)
                                                        (if (pos? (bit-and b (bit-shift-left 1 (- 255 a))))
                                                            (bit-or b (bit-shift-left -1 (- 256 a)))
                                                            (bit-and b (bit-shift-left 1 (- 255 a))))
                                                        b)))
            (opcodes :lt) (let [a (first @stack)
                                b (second @stack)]
                            (swap! stack rest)
                            (swap! stack rest)
                            (swap! stack conj (if (< a b) 1 0)))
            (opcodes :gt) (let [a (first @stack)
                                b (second @stack)]
                            (swap! stack rest)
                            (swap! stack rest)
                            (swap! stack conj (if (> a b) 1 0)))
            (opcodes :slt) (let [a (first @stack)
                                 b (second @stack)]
                             (swap! stack rest)
                             (swap! stack rest)
                             (swap! stack conj (if (< a b) 1 0)))
            (opcodes :sgt) (let [a (first @stack)
                                 b (second @stack)]
                                (swap! stack rest)
                                (swap! stack rest)
                                (swap! stack conj (if (> a b) 1 0)))
            (opcodes :eq) (let [a (first @stack)
                                b (second @stack)]
                            (swap! stack rest)
                            (swap! stack rest)
                            (swap! stack conj (if (= a b) 1 0)))
            (opcodes :iszero) (let [a (first @stack)]
                                (swap! stack rest)
                                (swap! stack conj (if (= a 0) 1 0)))
            (opcodes :and) (let [a (first @stack)
                                 b (second @stack)]
                                (swap! stack rest)
                                (swap! stack rest)
                                (swap! stack conj (bit-and a b)))
            (opcodes :or) (let [a (first @stack)
                                b (second @stack)]
                            (swap! stack rest)
                            (swap! stack rest)
                            (swap! stack conj (bit-or a b)))
            (opcodes :xor) (let [a (first @stack)
                                 b (second @stack)]
                             (swap! stack rest)
                             (swap! stack rest)
                             (swap! stack conj (bit-xor a b)))
            (opcodes :not) (let [a (first @stack)]
                                (swap! stack rest)
                                (swap! stack conj (bit-not a)))
            (opcodes :byte) (let [a (first @stack)
                                  b (second @stack)]
                                (swap! stack rest)
                                (swap! stack rest)
                                (swap! stack conj (bit-shift-right b (* 8 (- 31 a)))))

          )

        (swap! pc inc)))
    @stack))

;; parse a mixed list of opcode symbols and bytes into a list of bytes
(defn parse [code]
  (map (fn [x]
         (if (keyword? x)
           (opcodes x)
           x))
       code))

;; (def instrs [:push1 0x01 :push1 0x02 :add :push1 0x05 :sub :stop])

;; (interpret (parse instrs))
