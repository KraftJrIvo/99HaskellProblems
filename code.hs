--1 Last Element
myLast :: [a] -> a
myLast [] = error "List is empty, it has no last element."
myLast [a] = a
myLast list = list !! (length list - 1)
--2 But Last Element
myButLast :: [a] -> a
myButLast [] = error "List is empty, it has no but last element."
myButLast [a] = error "List is too short, it has no but last element."
myButLast [a, b] = a
myButLast list = list !! (length list - 2)
--3 K-th Element
myKthElement :: [a] -> Int -> a
myKthElement [] k = error "List is empty, it has no k-th element."
myKthElement list k = if k >= 1 && (k - 1) < (length list)
                        then list !! (k - 1)
                        else error "Value of k is out of scope of given list."
--4 Length of List
myLength :: [a] -> Int
myLength [] = 0
myLength list = (myLength (tail list)) + 1