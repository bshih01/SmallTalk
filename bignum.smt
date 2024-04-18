;; Starter code for SmallTalk assignement.
;; Author: Richard Townsend 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;             Exercise 1 classes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(class Natural
   [subclass-of Magnitude]

   (class-method first:rest: (aD anM) ; private
      (((anM isZero) & (aD = 0)) ifTrue:ifFalse:
         {(NatZero new)}
         {((NatNonzero new) first:rest: aD anM)}))

   (class-method fromSmall: (anInteger) (
      (anInteger = 0) ifTrue:ifFalse:
         {(NatZero new)}
         {(self first:rest: 
            (anInteger mod: (Natural base))
            (self fromSmall: (anInteger div: (Natural base))))}))

   (class-method base () 64) ; private

   ; private methods suggested from textbook (page 672)
   (method modBase () (self subclassResponsibility)) 
   (method divBase () (self subclassResponsibility)) 
   (method timesBase () (self subclassResponsibility)) 
   (method compare:withLt:withEq:withGt: (aNatural ltBlock eqBlock gtBlock) 
      (self subclassResponsibility)) 
   (method plus:carry: (aNatural c) (self subclassResponsibility)) 
   (method minus:borrow: (aNatural c) (self subclassResponsibility)) 

   (method timesDigit:plus: (d r) (self subclassResponsibility)) ; private

   (method = (aNatural) 
      (self compare:withLt:withEq:withGt: 
               aNatural [block () false] [block () true] [block () false]))

   (method < (aNatural) 
      (self compare:withLt:withEq:withGt:
               aNatural [block () true] [block () false] [block () false]))

   (method + (aNatural) (self plus:carry: aNatural 0))

   (method * (aNatural) (self subclassResponsibility))

   (method subtract:withDifference:ifNegative: (aNatural diffBlock exnBlock)
      ((self < aNatural) ifTrue:ifFalse:
         exnBlock
         {(diffBlock value: (self minus:borrow: aNatural 0))}))

   (method sdivmod:with: (n aBlock) (self subclassResponsibility))

   (method decimal () [locals num n d0]
      (set n self)
      (set num (List new))
      ({(n isZero)} whileFalse:
         {(set d0 (n smod: 10))
         (set n (n sdiv: 10))
         (num addFirst: d0)})
      ((num isEmpty) ifTrue:ifFalse:
         {(num addFirst: 0)}
         {num}))

   (method isZero  () (self subclassResponsibility))

   ;; methods that are already implemented for you
   (method - (aNatural)
      (self subtract:withDifference:ifNegative:
            aNatural
            [block (x) x]
            {(self error: 'Natural-subtraction-went-negative)}))

   (method sdiv: (n) (self sdivmod:with: n [block (q r) q]))

   (method smod: (n) (self sdivmod:with: n [block (q r) r]))

   (method print () ((self decimal) do: [block (x) (x print)]))

   ;; private methods for testing
   (method validated ()
      ((self invariant) ifFalse:
         {(self printrep)
          (self error: 'invariant-violation)})
      self)

   (method compare-symbol: (aNat)
      (self compare:withLt:withEq:withGt: aNat {'LT} {'EQ} {'GT}))
)

;; Represents a 0 natural number
(class NatZero
   [subclass-of Natural]
   (method invariant () true) ;; private

   (method timesDigit:plus: (d r) (Natural fromSmall: r)) ; private

   ;; for debugging
   (method printrep () (0 print))

   (method isZero () true)

   (method divBase () self)

   (method modBase () 0)

   (method timesBase () self)

   (method * (aNatural) self)

   (method sdivmod:with: (anInteger aBlock) (aBlock value:value: self 0))

   (method compare:withLt:withEq:withGt: (aNatural ltBlock eqBlock gtBlock)
      ((aNatural isZero) ifTrue:ifFalse:
         eqBlock
         ltBlock))

   (method plus:carry: (aNatural c) 
      ((aNatural isZero) ifTrue:ifFalse:
         {(Natural fromSmall: c)}
         {(aNatural plus:carry: self c)}))

   (method minus:borrow: (aNatural c)
      (((aNatural isZero) & (c = 0)) ifTrue:ifFalse:
         {self}
         {(self error: 'Natural-cannot-be-negative)}))
)

; Represents a natural number greater than 0
(class NatNonzero
   [subclass-of Natural]
   [ivars m d] ; a non-zero natural number is of the form d + m * b, where d
               ; is integer representing a digit of base b, and m is a natural
               ; number

   (method first:rest: (aD anM) ;; private
      (set m anM) 
      (set d aD) 
      self)

   (method invariant () (((d < (Natural base)) & (d >= 0)) &  ;; private
                        (((m isZero) & (d = 0)) not)))

   (method isZero () false)

   ;; addition with a carry bit
   (method plus:carry: (aNatural c) [locals sum least cout]
      (set sum ((d + (aNatural modBase)) + c))
      (set least (sum mod: (Natural base)))
      (set cout  (sum div: (Natural base)))
      (NatNonzero first:rest: least (m plus:carry: (aNatural divBase) cout)))
      
   ;; subtraction with a borrow bit
   (method minus:borrow: (aNatural b) [locals diff least bout]
      (set diff (d - ((aNatural modBase) + b)))
      ((diff < 0) ifTrue:ifFalse:
         {(set diff (diff + (Natural base)))
          (set bout 1)}
         {(set bout 0)})
      (NatNonzero first:rest: diff (m minus:borrow: (aNatural divBase) bout)))

   ;; multiplication
   (method * (aNatural) [locals d1 d2 m1 m2]
      ;; simple method; fastest; based on this law:
      ;;   (d + b * m) * n == (d * n) + b * (m * n)
      ((aNatural timesDigit:plus: d 0) + ((m * aNatural) timesBase)))

   (method timesDigit:plus: (dig r) ; private, answers self * d + r
      [locals pp]
      (set pp ((d * dig) + r))
      (NatNonzero first:rest: (pp mod: (Natural base))
                  (m timesDigit:plus: dig (pp div: (Natural base)))))

   ;; debugging method
   (method printrep () (m printrep) (', print) (d print))

   (method divBase () m)

   (method modBase () d)

   (method timesBase () (Natural first:rest: 0 (Natural first:rest: d m)))

   (method sdivmod:with: (n aBlock) [locals x0 qp rp]
      (m sdivmod:with: n (block (q r)
         (set x0 ((r * (Natural base)) + d))
         (set qp (Natural first:rest: (x0 div: n) q))
         (set rp (x0 mod: n))
         (aBlock value:value: qp rp))))

   (method compare:withLt:withEq:withGt: (aNatural ltBlock eqBlock gtBlock) 
      [locals xp x0 yp y0 aBlock]
      (set xp (self sdiv: (Natural base)))
      (set x0 (self smod: (Natural base)))
      (set yp (aNatural sdiv: (Natural base)))
      (set y0 (aNatural smod: (Natural base)))
      ((x0 = y0) ifTrue:ifFalse:
         {(set aBlock eqBlock)}
         {((x0 < y0) ifTrue:ifFalse:
            {(set aBlock ltBlock)}
            {(set aBlock gtBlock)})})
      (xp compare:withLt:withEq:withGt: yp ltBlock aBlock gtBlock))
)

;; For testing naturals
(class DebugNat
   [subclass-of Object]
   [ivars nat] ; a natural number
   (class-method of: (aNat) ((self new) init: aNat))
   (method init: (n) (set nat n) self) ;; private
   (method print () (nat printrep))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put your unit tests for Exercise 1 here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;             Exercise 2 classes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(class LargeInteger
   [subclass-of Integer]
   [ivars magnitude]

   (class-method withMagnitude: (aNatural)
      ((self new) magnitude: aNatural))

   (method magnitude: (aNatural) ; private, for initialization
      (set magnitude aNatural)
      self)

   (method magnitude () magnitude)

   (class-method fromSmall: (anInteger)
      ((anInteger isNegative) ifTrue:ifFalse: 
         {(((self fromSmall: 1) + (self fromSmall: ((anInteger + 1) negated)))
            negated)}
         {((LargePositiveInteger new) magnitude: 
                  (Natural fromSmall: anInteger))}))

   (method isZero () (magnitude isZero))

   (method = (anInteger) ((self - anInteger)     isZero))

   (method < (anInteger) ((self - anInteger) isNegative))

   (method div: (n) (self sdiv: n))

   (method mod: (n) (self smod: n))

   (method sdiv: (n) (self subclassResponsibility))

   (method smod: (n) (self - ((LargeInteger fromSmall: n) * (self sdiv: n))))

)

; Represents a positive integer
(class LargePositiveInteger
   [subclass-of LargeInteger]

   ;; short division (already implemented for you)
   (method sdiv: (anInteger)
      ((anInteger isStrictlyPositive) ifTrue:ifFalse: 
         {(LargePositiveInteger withMagnitude:  (magnitude sdiv: anInteger))}
         {((((self - (LargeInteger fromSmall: anInteger)) -
            (LargeInteger fromSmall: 1))
               sdiv: (anInteger negated))
               negated)}))

   (method print ()
      ((self magnitude) print))

   (method isNegative () false)

   (method isNonnegative () true)

   (method isStrictlyPositive () (((self magnitude) isZero) not))

   (method negated () 
      (LargeNegativeInteger withMagnitude: (self magnitude)))

   (method multiplyByLargePositiveInteger: (n)
      (LargePositiveInteger withMagnitude: ((self magnitude) * (n magnitude))))
   
   (method multiplyByLargeNegativeInteger: (n)
      (LargeNegativeInteger withMagnitude: ((self magnitude) * (n magnitude))))

   (method * (n) 
      (n multiplyByLargePositiveInteger: self))

   (method addLargePositiveIntegerTo: (n)
      (LargePositiveInteger withMagnitude: ((self magnitude) + (n magnitude))))

   (method addLargeNegativeIntegerTo: (n)
      (((self magnitude) < (n magnitude)) ifTrue:ifFalse:
         {(LargeNegativeInteger withMagnitude: 
                                 ((n magnitude) - (self magnitude)))}
         {(LargePositiveInteger withMagnitude: 
                                 ((self magnitude) - (n magnitude)))}))

   (method + (n)
      (n addLargePositiveIntegerTo: self))
)

;; Represents a negative integer
(class LargeNegativeInteger
   [subclass-of LargeInteger]

   ;; short division (already implemented for you)
   (method sdiv: (anInteger)
      ((self negated) sdiv: (anInteger negated)))

   (method print ()
      ((((self magnitude) isZero) not) ifTrue:
         {('- print)})
      ((self magnitude) print))

   (method isNegative () true)

   (method isNonnegative () false)

   (method isStrictlyPositive () false)

   (method negated () 
      (LargePositiveInteger withMagnitude: (self magnitude)))

   (method multiplyByLargePositiveInteger: (n)
      (LargeNegativeInteger withMagnitude: ((self magnitude) * (n magnitude))))
   
   (method multiplyByLargeNegativeInteger: (n)
      (LargePositiveInteger withMagnitude: ((self magnitude) * (n magnitude))))

   (method * (n) 
      (n multiplyByLargeNegativeInteger: self))

   (method addLargePositiveIntegerTo: (n)
      (((n magnitude) < (self magnitude)) ifTrue:ifFalse:
         {(LargeNegativeInteger withMagnitude: 
                                 ((self magnitude) - (n magnitude)))}
         {(LargePositiveInteger withMagnitude: 
                                 ((n magnitude) - (self magnitude)))}))

   (method addLargeNegativeIntegerTo: (n)
      (LargeNegativeInteger withMagnitude: ((self magnitude) + (n magnitude))))


   (method + (n)
      (n addLargeNegativeIntegerTo: self))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put your unit tests for Exercise 2 here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


