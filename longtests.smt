;; ;; step 7
;; (check-assert ((NatZero new) isZero))
;; (check-assert (((NatNonzero first:rest: 1 1) isZero) not))
;; 
;; ;; step 8
;; (check-print (DebugNat of: (Natural fromSmall: 0)) 0)
;; (check-print (DebugNat of: (Natural fromSmall: 5)) 0,1,0,1)
;; (check-print (DebugNat of: (Natural fromSmall: 3)) 0,1,1)
;; (check-print (DebugNat of: (Natural fromSmall: 57)) 0,1,1,1,0,0,1)
;; (check-print (DebugNat of: 
;;                 (Natural fromSmall: ((Natural base) * (Natural base))))
;;              0,1,0,0)
;; 
;; ;; step 10
;; (check-assert ((NatNonzero fromSmall: 7) isKindOf: Natural))
;; (check-assert (((NatZero new) divBase) isKindOf: Natural))
;; (check-assert (((NatZero new) modBase) isKindOf: SmallInteger))
;; (check-assert (((NatZero new) timesBase) isKindOf: Natural))
;; (check-assert (((NatNonzero fromSmall: 3) divBase) isKindOf: Natural))
;; (check-assert (((NatNonzero fromSmall: 5) modBase) isKindOf: SmallInteger))
;; (check-assert (((NatNonzero fromSmall: 3) timesBase) isKindOf: Natural))
;; (check-print (DebugNat of: ((Natural fromSmall: 0) divBase)) 0)
;; (check-print ((Natural fromSmall: 0) modBase) 0)
;; (check-print (DebugNat of: ((Natural fromSmall: 0) timesBase)) 0)
;; (check-print (DebugNat of: ((Natural fromSmall: 5) divBase)) 0,1,0)
;; (check-print ((Natural fromSmall: 5) modBase) 1)
;; (check-print (DebugNat of: ((Natural fromSmall: 5) timesBase)) 0,1,0,1,0)
;; 
;; ;; step 13
;; (check-print 
;;    (DebugNat of: ((Natural fromSmall: 0) + (Natural fromSmall: 0))) 0)
;; (check-print 
;;    (DebugNat of: ((Natural fromSmall: 1) + (Natural fromSmall: 0))) 0,1)
;; (check-print 
;;    (DebugNat of: ((Natural fromSmall: 7) + (Natural fromSmall: 3))) 
;;    0,1,0,1,0)
;; (check-print 
;;    (DebugNat of: ((Natural fromSmall: 13) + (Natural fromSmall: 12))) 
;;    0,1,1,0,0,1)
;; (check-print 
;;    (DebugNat of: ((Natural fromSmall: 7) + (Natural fromSmall: 24))) 
;;    0,1,1,1,1,1)
;; (check-print 
;;    (DebugNat of: ((Natural fromSmall: 5) plus:carry: 
;;                                            (Natural fromSmall: 6) 
;;                                            0)) 
;;    0,1,0,1,1)
;; 
;; ;; step 15
;; (check-print (DebugNat of: ((Natural fromSmall: 0) sdiv: 0)) 0)
;; (check-print (DebugNat of: ((Natural fromSmall: 0) sdiv: 2)) 0)
;; (check-print (DebugNat of: ((Natural fromSmall: 2) sdiv: 2)) 0,1)
;; (check-print (DebugNat of: ((Natural fromSmall: 3) sdiv: 2)) 0,1)
;; (check-print (DebugNat of: ((Natural fromSmall: 4) sdiv: 2)) 0,1,0)
;; (check-print (DebugNat of: ((Natural fromSmall: 17) sdiv: 3)) 0,1,0,1)
;; (check-print ((Natural fromSmall: 0) smod: 2) 0)
;; (check-print ((Natural fromSmall: 2) smod: 2) 0)
;; (check-print ((Natural fromSmall: 3) smod: 2) 1)
;; (check-print ((Natural fromSmall: 4) smod: 2) 0)
;; (check-print ((Natural fromSmall: 17) smod: 3) 2)
;; 
;; ;; step 17
;; (check-print (Natural fromSmall: 5) 5)
;; (check-print (Natural fromSmall: 121) 121)
;; (check-print (Natural fromSmall: 0) 0)
;; (check-print (Natural fromSmall: 29) 29)
;; (check-print (Natural fromSmall: 1234789) 1234789)
;; 
;; ;; step 20
;; (check-assert ((Natural fromSmall: 0) = (Natural fromSmall: 0)))
;; (check-assert ((Natural fromSmall: 0) < (Natural fromSmall: 1)))
;; (check-assert ((Natural fromSmall: 2) > (Natural fromSmall: 0)))
;; (check-assert ((Natural fromSmall: 2) < (Natural fromSmall: 5)))
;; (check-assert ((Natural fromSmall: 5) > (Natural fromSmall: 2)))
;; (check-assert ((Natural fromSmall: 5) = (Natural fromSmall: 5)))
;; (check-assert ((Natural fromSmall: 10) = (Natural fromSmall: 10)))
;; (check-assert ((Natural fromSmall: 78) > (Natural fromSmall: 24)))
;; (check-assert ((Natural fromSmall: 54) > (Natural fromSmall: 52)))
;; (check-assert ((Natural fromSmall: 94) < (Natural fromSmall: 95)))
;; 
;; ;; step 22
;; (check-print ((Natural fromSmall: 5) - (Natural fromSmall: 2)) 3)
;; (check-print ((Natural fromSmall: 51) - (Natural fromSmall: 2)) 49)
;; (check-print ((Natural fromSmall: 27) - (Natural fromSmall: 9)) 18)
;; (check-error ((Natural fromSmall: 0) - (Natural fromSmall: 2)))

(Natural addSelector:withMethod: 'squared
  (compiled-method () (self * self)))
(Natural addSelector:withMethod: 'coerce:
  (compiled-method (i) (Natural fromSmall: i)))
(Natural addSelector:withMethod: 'raisedToInteger:
  (Number compiledMethodAt: 'raisedToInteger:))

(check-print ((Natural fromSmall: 10) raisedToInteger: 10) 10000000000)
(check-print ((Natural fromSmall:  9) raisedToInteger:  9)   387420489)
(check-print ((Natural fromSmall: 99) raisedToInteger: 99) 369729637649726772657187905628805440595668764281741102430259972423552570455277523421410650010128232727940978889548326540119429996769494359451621570193644014418071060667659301384999779999159200499899)


;; step 2
(check-print (LargeInteger fromSmall: 10) 10)
(check-print (LargeNegativeInteger withMagnitude: (Natural fromSmall: 40)) -40)
(check-print (LargeNegativeInteger withMagnitude: (Natural fromSmall: 0)) 0)

;; step 4
(check-assert ((LargePositiveInteger fromSmall: 10) isNonnegative))
(check-assert ((LargePositiveInteger fromSmall: 10) isStrictlyPositive))
(check-assert (((LargePositiveInteger fromSmall: 0) isStrictlyPositive) not))
(check-assert (((LargeNegativeInteger fromSmall: 0) isStrictlyPositive) not))
(check-assert 
  ((LargeNegativeInteger withMagnitude: (Natural fromSmall: 5)) isNegative))

;; step 6
(check-assert (((LargePositiveInteger fromSmall: 10) negated) isNegative))
(check-assert 
  ((((LargePositiveInteger fromSmall: 0) negated) isStrictlyPositive) not))
(check-assert 
  (((LargeNegativeInteger withMagnitude: 
                            (Natural fromSmall: 5)) negated) isNonnegative))
(check-assert 
  (((LargeNegativeInteger withMagnitude: 
                            (Natural fromSmall: 5)) negated) 
                              isStrictlyPositive))

;; step 8
(val ten (LargePositiveInteger fromSmall: 10))
(val neg-ten (ten negated))
(check-print (ten * ten) 100)
(check-print (neg-ten * neg-ten) 100)
(check-print (ten * neg-ten) -100)
(check-print (neg-ten * ten) -100)
(check-print (ten + ten) 20)
(check-print (neg-ten + neg-ten) -20)
(check-print (ten + neg-ten) 0)
(check-print (neg-ten + ten) 0)