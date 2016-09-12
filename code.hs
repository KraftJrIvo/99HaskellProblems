--1 Last Element
myLast :: [a] -> a
myLast [] = error "List is empty, it has no last element."
myLast [a] = a
myLast list = myLast (tail list)
--2 But Last Element
myButLast :: [a] -> a
myButLast [] = error "List is empty, it has no but last element."
myButLast [a] = error "List is too short, it has no but last element."
myButLast [a, b] = a
myButLast list = myButLast (tail list)
--3 K-th Element
myKthElement :: [a] -> Int -> a
myKthElement [] k = error "List is empty, it has no k-th element."
myKthElement list k = if k >= 1 && (k - 1) < (myLength list)
                        then if k == 1 
                            then head list
                            else myKthElement (tail list) (k - 1)
                        else error "Value of k is out of scope of the given list."
--4 Length of List
myLength :: [a] -> Int
myLength [] = 0
myLength list = (myLength (tail list)) + 1
--5 Reverse List
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
--6 Is List a Palindrome
myPalindromeCheck :: (Eq a) => [a] -> Bool
myPalindromeCheck [] = True
myPalindromeCheck [a] = True
myPalindromeCheck (x:xs) = if x == myLast xs
                            then myPalindromeCheck (init xs)
                            else False
--7 List to Flat List (List of Elem Type)
data NestedList a = Elem a | List [NestedList a]
myFlat :: NestedList a -> [a]
myFlat (List []) = []
myFlat (Elem a) = [a]
myFlat (List (x:xs)) = myFlat x ++ myFlat (List xs)
--8 Remove Consecutive Duplicates from the List
myDuplicatesRemove :: (Eq a) => [a] -> [a]
myDuplicatesRemove [] = []
myDuplicatesRemove [a] = [a]
myDuplicatesRemove (x:xs) = if x == head xs
                                then myDuplicatesRemove xs
                                else [x] ++ myDuplicatesRemove xs
--Tools for problem 9
myFirstDuplicatesRemove :: (Eq a) => [a] -> [a]
myFirstDuplicatesRemove [] = []
myFirstDuplicatesRemove [a] = []
myFirstDuplicatesRemove (x:xs) = if x == head xs
                                    then myFirstDuplicatesRemove xs
                                    else xs
myDuplicatesExtract :: (Eq a) => [a] -> [a]
myDuplicatesExtract [] = []
myDuplicatesExtract [a] = [a]
myDuplicatesExtract (x:xs) = if x == head xs
                                then [x] ++ myDuplicatesExtract xs
                                else [x]
--9 Pack consecutive duplicates of list elements into sublists
myPack :: (Eq a) => [a] -> [[a]]
myPack [] = []
myPack [a] = [[a]]
myPack list = [myDuplicatesExtract list] ++ myPack (myFirstDuplicatesRemove list)
--Tools for problem 10 
myEncode :: (Eq a) => [a] -> [(Int, a)]
myEncode [] = []
myEncode list = [(myLength list, head list)]
--10 Run-length encoding of a list.
myListEncode :: (Eq a) => [a] -> [(Int, a)]
myListEncode [] = []
myListEncode [a] = myEncode [a]
myListEncode list = myEncode (myDuplicatesExtract list) ++ myListEncode (myFirstDuplicatesRemove list)